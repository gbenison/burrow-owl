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

typedef struct _cache_slot cache_slot_t;
struct _cache_slot
{
  guint    id;
  gboolean valid;
  gint     segid;
  gdouble  *buf;
};

static cache_slot_t* segment_cache_obtain_slot     (HosSpectrumSegmented *self);
static void          segment_cache_bless_slot      (cache_slot_t* slot);


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
  priv->segment_lock = g_mutex_new();
  priv->segment_requested_cond = g_cond_new();
  priv->segment_ready_cond = g_cond_new();
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
 * FIXME must be thread-safe
 * non-blocking
 *
 * returns:
 *   TRUE - available; '*dest' contains the point value
 *  FALSE - not available; '*dest' unchanged
 */
static gboolean
segmented_fetch_point(HosSpectrumSegmented *self, gint segid, gint pt_idx, gdouble *dest)
{
  /* Find slot corresponding to 'segid' */
  cache_slot_t* slot = NULL;  /* FIXME */

  if (slot == NULL)
    return FALSE;

  guint id = slot->id;

  if (slot->valid == FALSE)
    return FALSE;

  gdouble result = slot->buf[pt_idx];

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
  /*
   * FIXME
   *
   * maintain per-'root' tickle-request-queue, sorted list of requested segments.
   * IO thread consults tickle-request-queue (together with accumulation-request-record) to
   * determine next segment to read.
   *
   * Grab point if the segment is instantiated; otherwise add segment to the tickle-request-queue.
   */
  return FALSE;
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

      cache_slot_t* slot = segment_cache_obtain_slot(self);
      g_assert(slot != NULL);
      g_assert(slot->buf != NULL);
      slot->segid = next_segment_id;
      class->read_segment(self, next_segment_id, slot->buf);
      segment_cache_bless_slot(slot);

      g_cond_signal(priv->segment_ready_cond);

    }
}

/*
 * Acquire a cache slot in spectrum 'self', mark it as invalid
 * so it can be filled with data.
 */
static cache_slot_t*
segment_cache_obtain_slot(HosSpectrumSegmented *self)
{
  cache_slot_t *result = NULL;

  /* FIXME find an available slot */

  if (0)
    {
      result->valid = FALSE;
      ++result->id;
    }

  return result;
}

static void
segment_cache_bless_slot(cache_slot_t *slot)
{
  ++slot->id;
  slot->valid = TRUE;
}

void
spectrum_segmented_set_segment_size(HosSpectrumSegmented *self, guint size)
{
  SEGMENTED_PRIVATE(self, segment_size) = size;
  /* FIXME allocate buffers */
}


