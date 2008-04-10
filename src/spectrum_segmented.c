/*
 *  Copyright (C) 2008 Greg Benison
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "spectrum_segmented.h"
#include "skiplist.h"

#define SEGMENTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmentedPrivate))
#define SEGMENTED_PRIVATE(o, field) ((SEGMENTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumSegmentedPrivate HosSpectrumSegmentedPrivate;

struct _HosSpectrumSegmentedPrivate
{
  GMutex  *segment_lock;
  GThread *io_thread;
  GCond   *segment_requested_cond;
  GCond   *segment_ready_cond;

  guint   segment_ptr;
  guint   segment_size;

  skip_node_t *request_queue;
  skip_node_t *subsequent_queue;

  skip_node_t *segment_cache;
  guint        segment_cache_size;
  guint        segment_cache_max_size;

};

static gdouble  spectrum_segmented_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_segmented_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);
static void     segmented_ensure_io_thread    (HosSpectrumSegmented *self);
static void     spectrum_segmented_io_thread  (HosSpectrumSegmented *self);
static gint     determine_next_segment        (HosSpectrumSegmented *self);
static void     idx2segment                   (HosSpectrumSegmented *self,
					       guint *idx, gint *dest_segid, gint *dest_pt_idx);
static void     request_segment_accumulate    (HosSpectrumSegmented *self, gint segid);
static gboolean segmented_fetch_point         (HosSpectrumSegmented *self, gint segid, gint pt_idx, gdouble *dest);
static void     load_segment                  (HosSpectrumSegmented *self, gint segid);

typedef struct _cache_slot cache_slot_t;
struct _cache_slot
{
  guint     id;
  gboolean  valid;
  gint      segid;
  guint     last_access_time;
  gdouble  *buf;
};

static cache_slot_t* segment_cache_obtain_slot     (HosSpectrumSegmented *self);
static void          segment_cache_bless_slot      (cache_slot_t* slot);
static void          find_least_used_slot          (cache_slot_t* slot, cache_slot_t** result);
static cache_slot_t* cache_slot_new                (guint segment_size);
static guint         access_timer                  (void);

G_DEFINE_ABSTRACT_TYPE (HosSpectrumSegmented, hos_spectrum_segmented, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_segmented_class_init(HosSpectrumSegmentedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->accumulate = spectrum_segmented_accumulate;
  spectrum_class->tickle     = spectrum_segmented_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumSegmentedPrivate));
}

static void
hos_spectrum_segmented_init(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);
  priv->segment_lock           = g_mutex_new();
  priv->segment_requested_cond = g_cond_new();
  priv->request_queue          = skip_list_new(16, 0.5);
  priv->subsequent_queue       = skip_list_new(16, 0.5);
  priv->segment_ready_cond     = g_cond_new();
  priv->segment_cache          = skip_list_new(20, 0.7);
  priv->segment_cache_size     = 0;

  spectrum_segmented_set_cache_size(self, 32);
}

static gdouble
spectrum_segmented_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  gdouble result;
  HosSpectrumSegmented *self_segmented = HOS_SPECTRUM_SEGMENTED(self);
  segmented_ensure_io_thread(self_segmented);

  gint segid, pt;
  idx2segment(self_segmented, idx, &segid, &pt);

  if (!segmented_fetch_point(self_segmented, segid, pt, &result))
    {
      GMutex *lock                   = SEGMENTED_PRIVATE(self, segment_lock);
      GCond  *segment_ready_cond     = SEGMENTED_PRIVATE(self, segment_ready_cond);
      GCond  *segment_requested_cond = SEGMENTED_PRIVATE(self, segment_requested_cond);

      g_mutex_lock(lock);

      while (1)
	{
	  if (segmented_fetch_point(self_segmented, segid, pt, &result))
	    break;
	  request_segment_accumulate(self_segmented, segid);
	  g_cond_signal(segment_requested_cond);
	  g_cond_wait(segment_ready_cond, lock);
	}
      g_mutex_unlock(lock);
    }

  return result;
}

/*
 * (possibly) fill '*dest' with the value from segmented spectrum 'self' in segment 'segid'
 * with point index 'pt_idx'.
 *
 * returns:
 *   TRUE - available; '*dest' contains the point value
 *  FALSE - not available; '*dest' unchanged
 */
static gboolean
segmented_fetch_point(HosSpectrumSegmented *self, gint segid, gint pt_idx, gdouble *dest)
{
  /* Find slot corresponding to 'segid' */
  cache_slot_t* slot = skip_list_lookup(SEGMENTED_PRIVATE(self, segment_cache), segid);

  if (slot == NULL)
    return FALSE;

  guint id = slot->id;

  if (slot->valid == FALSE)
    return FALSE;

  gdouble result = slot->buf[pt_idx];

  g_atomic_int_set(&slot->last_access_time, access_timer());

  if (slot->segid != segid)
    return FALSE;

  if (slot->id != id)
    return FALSE;

  *dest = result;

  return TRUE;
}

static void
request_segment_accumulate(HosSpectrumSegmented *self, gint segid)
{
  /*
   * FIXME
   *
   * maintain per-'root' record of 'accumulation segment request' which the IO thread will consult
   * to decide on the next segment to obtain.
   *
   * set the accumulation request of 'root', then block waiting for it to be instantiated by IO thread
   * block by waiting on segment_ready_cond
   */
}

static gboolean
spectrum_segmented_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  gint segid, pt;
  idx2segment(HOS_SPECTRUM_SEGMENTED(self), idx, &segid, &pt);
  gboolean result = segmented_fetch_point (HOS_SPECTRUM_SEGMENTED(self), segid, pt, dest);

  if (result == FALSE)
    {
      /* FIXME */
      /* add segid to a 'tickle request queue' to trigger reading of the missing segment */
    }

  return result;
}

static void
segmented_ensure_io_thread(HosSpectrumSegmented *self)
{
  if (SEGMENTED_PRIVATE(self, io_thread) == NULL)
    {
      GError  *error  = NULL;
      GThread *thread = g_thread_create((GThreadFunc)spectrum_segmented_io_thread,
					self,
					FALSE,
					&error);
      g_assert(error == NULL);
      SEGMENTED_PRIVATE(self, io_thread) = thread;
    }
}

/*
 * Returns:
 * the next most auspicious segment ID of 'self' for reading,
 * or -1 if none is auspicious at this time
 */
static gint
determine_next_segment(HosSpectrumSegmented *self)
{
  /* FIXME */

  /* 
   * decide next segment -- 
   * consult all per-'root' accumulation-records and tickle-request-queues.
   */

  return -1;
}

static void
idx2segment(HosSpectrumSegmented *self, guint *idx, gint *dest_segid, gint *dest_pt_idx)
{
  HosSpectrumSegmentedClass *class = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);
  g_assert(class->idx2segment != NULL);
  class->idx2segment(self, idx, dest_segid, dest_pt_idx);
}

static void
spectrum_segmented_io_thread(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv=SEGMENTED_GET_PRIVATE(self);
  HosSpectrumSegmentedClass *class = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);

  while (1)
    {
      gint next_segment_id;

      g_mutex_lock(priv->segment_lock);
      while (1)
	{
	  next_segment_id = determine_next_segment(self);
	  if (next_segment_id >= 0)
	    break;
	  g_cond_wait(priv->segment_requested_cond, priv->segment_lock);
	}
      g_mutex_unlock(priv->segment_lock);
      load_segment(self, next_segment_id);
      g_cond_signal(priv->segment_ready_cond);
    }
}

static void
load_segment(HosSpectrumSegmented *self, gint segid)
{
  HosSpectrumSegmentedClass   *class = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);
  HosSpectrumSegmentedPrivate *priv  = SEGMENTED_GET_PRIVATE(self);

  if (!skip_list_lookup(priv->segment_cache, segid))
    {
      cache_slot_t* slot = segment_cache_obtain_slot(self);
      g_assert(slot != NULL);
      if (slot->segid >= 0)
	{
	  cache_slot_t* popped = skip_list_pop(priv->segment_cache, slot->segid);
	  g_assert(popped == slot);
	}
      g_assert(slot->buf != NULL);
      slot->segid = segid;
      skip_list_insert(priv->segment_cache, slot->segid, slot);
      class->read_segment(self, segid, slot->buf);
      segment_cache_bless_slot(slot);
    }
}

/*
 * For testing purposes, force 'self' to load segment 'segid'.
 */
void
spectrum_segmented_test_load_segment(HosSpectrumSegmented *self, gint segid)
{
  load_segment(self, segid);
}

/*
 * Callback for skip_list_foreach();
 */
static void
find_least_used_slot(cache_slot_t* slot, cache_slot_t** result)
{
  if (*result == NULL)
    *result = slot;

  if (slot->last_access_time > (*result)->last_access_time)
    *result = slot;
}

static cache_slot_t*
cache_slot_new(guint segment_size)
{
  cache_slot_t* result = g_new0(cache_slot_t, 1);
  result->buf = g_new(gdouble, segment_size);
  result->segid = -1;

  return result;
}

/*
 * Acquire a cache slot in spectrum 'self', mark it as invalid
 * so it can be filled with data.
 */
static cache_slot_t*
segment_cache_obtain_slot(HosSpectrumSegmented *self)
{
  cache_slot_t *result = NULL;
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);

  if (priv->segment_cache_size < priv->segment_cache_max_size)
    {
      ++priv->segment_cache_size;
      result = cache_slot_new(priv->segment_size);
    }
  else
    skip_list_foreach(priv->segment_cache, (GFunc)find_least_used_slot, &result);

  g_assert(result != NULL);

  result->valid = FALSE;
  result->last_access_time = 0;
  ++result->id;

  return result;
}

static void
segment_cache_bless_slot(cache_slot_t *slot)
{
  ++slot->id;
  slot->valid = TRUE;
}

static void
set_segment_size(cache_slot_t* slot, gpointer data)
{
  guint size = GPOINTER_TO_UINT(data);
  slot->valid = FALSE;
  slot->buf   = g_renew(gdouble, slot->buf, size);
}

void
spectrum_segmented_set_segment_size(HosSpectrumSegmented *self, guint size)
{
  if (size != SEGMENTED_PRIVATE(self, segment_size))
    {
      SEGMENTED_PRIVATE(self, segment_size) = size;
      
      skip_node_t *segment_cache = SEGMENTED_PRIVATE(self, segment_cache);
      
      skip_list_foreach(SEGMENTED_PRIVATE(self, segment_cache),
			(GFunc)set_segment_size, GUINT_TO_POINTER(size));
    }
}

void
spectrum_segmented_set_cache_size(HosSpectrumSegmented *self, guint size)
{
  SEGMENTED_PRIVATE(self, segment_cache_max_size) = size;
}

/*
 * Roughly, provide an incrementing value on every call, like a timer.
 * Since this function will be called from multiple threads, it is not
 * thread-safe, e.g. not guaranteed to return unique values.  But for
 * its application, it is OK for it to behave "about right".
 */
static guint
access_timer()
{
  static guint clicks = 0;

  guint _clicks = g_atomic_int_get(&clicks);
  ++_clicks;
  g_atomic_int_set(&clicks, _clicks);
}

/*
 * Look up point 'idx' in 'self' without triggering traversal; store result
 * in *dest if found.
 *
 * Returns:
 * TRUE:  value found
 * FALSE: point not instantiated; *dest unchanged
 */
gboolean
spectrum_segmented_test_peek(HosSpectrumSegmented *self, gint *idx, gdouble *dest)
{
  return spectrum_tickle(HOS_SPECTRUM(self),
			 HOS_SPECTRUM(self),
			 idx, dest);
}

void
spectrum_segmented_test_print_cache  (HosSpectrumSegmented *self)
{
  skip_list_print(SEGMENTED_PRIVATE(self, segment_cache));
}
