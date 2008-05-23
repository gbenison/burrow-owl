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
#include "spectrum_priv.h"
#include "skiplist.h"

#define SEGMENTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmentedPrivate))
#define SEGMENTED_PRIVATE(o, field) ((SEGMENTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumSegmentedPrivate HosSpectrumSegmentedPrivate;

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


struct _HosSpectrumSegmentedPrivate
{
  GThread *io_thread;
  GMutex  *segment_request_lock;
  GCond   *segment_request_cond;

  GList   *iterators;
  GMutex  *iterators_lock;

  guint   segment_size;

  skip_list_t *request_queue;

  skip_list_t *segment_cache;
  guint        segment_cache_max_size;

};

struct segmented_iterator
{
  struct spectrum_iterator parent;

  HosSpectrumSegmentedPrivate *priv;
  HosSpectrumSegmentedClass   *class;
  gpointer                     traversal_env;

  gint         segid;
  gint         segid_saved;
  gint         pt;
  gint         pt_saved;

  skip_list_t *request_queue;
  GMutex      *request_queue_lock;

  GCond       *segment_ready_cond;
  GMutex      *wait_lock;

  cache_slot_t *last_slot;

};

static gdouble  spectrum_segmented_wait       (struct spectrum_iterator* self);
static void     spectrum_segmented_mark       (struct spectrum_iterator* self);
static gboolean spectrum_segmented_tickle     (struct spectrum_iterator* self, gdouble* dest);

static struct spectrum_iterator* spectrum_segmented_construct_iterator (HosSpectrum *self);
static void                      spectrum_segmented_free_iterator      (struct spectrum_iterator* self);

static void     segmented_ensure_io_thread    (HosSpectrumSegmented *self);
static void     spectrum_segmented_io_thread  (HosSpectrumSegmented *self);
static gint     determine_next_segment        (HosSpectrumSegmentedPrivate *priv);
static void     request_segment_accumulate    (HosSpectrumSegmentedPrivate *priv, gint segid);
static gboolean segmented_fetch_point         (struct segmented_iterator *iterator, gint segid, gint pt_idx, gdouble *dest);


G_DEFINE_ABSTRACT_TYPE (HosSpectrumSegmented, hos_spectrum_segmented, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_segmented_class_init(HosSpectrumSegmentedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->construct_iterator = spectrum_segmented_construct_iterator;
  spectrum_class->free_iterator      = spectrum_segmented_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumSegmentedPrivate));
}

static void
hos_spectrum_segmented_init(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);
  priv->segment_request_cond   = g_cond_new();
  priv->segment_request_lock   = g_mutex_new();
  priv->request_queue          = skip_list_new(16, 0.5);
  priv->segment_cache          = skip_list_new(16, 0.7);
  priv->iterators_lock         = g_mutex_new();

  spectrum_segmented_set_cache_size(self, 32);
}

static void
spectrum_segmented_mark(struct spectrum_iterator* self)
{
  self->blocked = TRUE;

  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  segmented_iterator->segid_saved = segmented_iterator->segid;
  segmented_iterator->pt_saved    = segmented_iterator->pt;
}

static gdouble
spectrum_segmented_wait(struct spectrum_iterator* self)
{
  gdouble result = 0;
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  HosSpectrumSegmentedPrivate *priv               = segmented_iterator->priv;
  HosSpectrumSegmentedClass   *class              = segmented_iterator->class;

  gint segid = segmented_iterator->segid_saved;
  gint pt    = segmented_iterator->pt_saved;

  if (!segmented_fetch_point(segmented_iterator, segid, pt, &result))
    {
      g_mutex_lock(segmented_iterator->wait_lock);
      while (!segmented_fetch_point(segmented_iterator, segid, pt, &result))
	{
	  g_mutex_lock(priv->segment_request_lock);
	  g_mutex_lock(segmented_iterator->request_queue_lock);
	  skip_list_insert(segmented_iterator->request_queue, segid, NULL);
	  g_mutex_unlock(segmented_iterator->request_queue_lock);
	  g_cond_signal(priv->segment_request_cond);
	  g_mutex_unlock(priv->segment_request_lock);
	  g_cond_wait(segmented_iterator->segment_ready_cond, segmented_iterator->wait_lock);
	}
      g_mutex_unlock(segmented_iterator->wait_lock);
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
segmented_fetch_point(struct segmented_iterator *iterator, gint segid, gint pt_idx, gdouble *dest)
{
  HosSpectrumSegmentedPrivate *priv = iterator->priv;
  cache_slot_t* slot;

  /* Find slot corresponding to 'segid' */
  if ((iterator->last_slot != NULL) && (iterator->last_slot->segid == segid))
    slot = iterator->last_slot;
  else
    slot = skip_list_lookup(priv->segment_cache, segid);

  if (slot == NULL)
    return FALSE;

  iterator->last_slot = slot;

  guint    slot_id_before    = g_atomic_int_get(&slot->id);
  gboolean slot_valid_before = g_atomic_int_get(&slot->valid);
  gdouble* slot_buf          = g_atomic_pointer_get(&slot->buf);

  if (slot_valid_before == FALSE)    return FALSE;

  gdouble  result   = slot_buf[pt_idx];

  gint     slot_segid       = g_atomic_int_get(&slot->segid);
  guint    slot_id_after    = g_atomic_int_get(&slot->id);
  gboolean slot_valid_after = g_atomic_int_get(&slot->valid);

  if (slot_segid != segid)              return FALSE;
  if (slot_valid_after == FALSE)        return FALSE;
  if (slot_id_before != slot_id_after)  return FALSE;

  *dest = result;
  g_atomic_int_set(&slot->last_access_time, access_timer());

  return TRUE;
}

static gboolean
spectrum_segmented_tickle(struct spectrum_iterator* self, gdouble *dest)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  HosSpectrumSegmentedPrivate *priv               = segmented_iterator->priv;
  HosSpectrumSegmentedClass   *class              = segmented_iterator->class;

  gint segid, pt;
  gint old_segid = segmented_iterator->segid;
  class->idx2segment(segmented_iterator->traversal_env, self->idx, &segid, &pt);
  segmented_iterator->segid = segid;
  segmented_iterator->pt    = pt;
  gboolean result = segmented_fetch_point (segmented_iterator, segid, pt, dest);

  if (result == FALSE)
    {
      if (old_segid != segid)
	{
	  g_mutex_lock(segmented_iterator->request_queue_lock);
	  skip_list_insert(segmented_iterator->request_queue, segid, NULL);
	  g_mutex_unlock(segmented_iterator->request_queue_lock);
	}
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
determine_next_segment(HosSpectrumSegmentedPrivate *priv)
{

  g_mutex_lock(priv->iterators_lock);
  GList *iterators = priv->iterators;
  for (iterators = priv->iterators; iterators != NULL; iterators = iterators->next)
    {
      struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)(iterators->data);
      g_mutex_lock(segmented_iterator->request_queue_lock);
      while (1)
	{
	  gint next = skip_list_pop_first(segmented_iterator->request_queue);
	  if (next < 0) break;
	  skip_list_insert(priv->request_queue, next, NULL);
	}
      g_mutex_unlock(segmented_iterator->request_queue_lock);
    }
  g_mutex_unlock(priv->iterators_lock);

  while (1)
    {
      /* underflow condition? no segment requests pending */
      if (skip_list_is_empty(priv->request_queue))
	return -1;
      
      gint next = skip_list_pop_first(priv->request_queue);

      if (!skip_list_lookup(priv->segment_cache, next))
	return next;
    }
  
  g_assert_not_reached();

}

static void
spectrum_segmented_io_thread(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv  = SEGMENTED_GET_PRIVATE(self);
  HosSpectrumSegmentedClass   *class = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);

  while (1)
    {
      gint segid;

      segid = determine_next_segment(priv);
      if (segid < 0)
	{
	  g_mutex_lock(priv->segment_request_lock);
	  while (1)
	    {
	      /* FIXME merge segment requests */
	      segid = determine_next_segment(priv);
	      if (segid >= 0)
		break;
	      g_cond_wait(priv->segment_request_cond, priv->segment_request_lock);
	    }
	  g_mutex_unlock(priv->segment_request_lock);
	}

      /* load segment */
      if (!skip_list_lookup(priv->segment_cache, segid))
	{
	  cache_slot_t* slot = segment_cache_obtain_slot(self);
	  g_assert(slot != NULL);
	  g_assert(slot->buf != NULL);
	  class->read_segment(self->traversal_env, segid, slot->buf);
	  g_atomic_int_set(&slot->segid, segid);
	  skip_list_insert(priv->segment_cache, segid, slot);
	  segment_cache_bless_slot(slot);
	}

      /*
       * FIXME
       * Look through the iterators list, and if the new segment corresponds to any
       * that an 'accumulate' request is waiting for, set the iterator's 'blocked' field
       * to false.
       */
      
      g_mutex_lock(priv->iterators_lock);
      GList *iterators = priv->iterators;
      for (iterators = priv->iterators; iterators != NULL; iterators = iterators->next)
	{
	  struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)(iterators->data);
	  struct spectrum_iterator  *iterator           = (struct spectrum_iterator*)(iterators->data);
	  if (segid == segmented_iterator->segid_saved)
	    {
	      g_mutex_lock(segmented_iterator->wait_lock);
	      if (segid == segmented_iterator->segid_saved)
		{
		  iterator->blocked = FALSE;
		  g_cond_signal(segmented_iterator->segment_ready_cond);
		}
	      g_mutex_unlock(segmented_iterator->wait_lock);
	    }
	}
      g_mutex_unlock(priv->iterators_lock);
      
    }
}

/*
 * Callback for skip_list_foreach();
 */
static void
find_least_used_slot(cache_slot_t* slot, cache_slot_t** result)
{
  if (*result == NULL)
    *result = slot;

  if (slot->last_access_time < (*result)->last_access_time)
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

  if (skip_list_length(priv->segment_cache) < priv->segment_cache_max_size)
    result = cache_slot_new(priv->segment_size);
  else
    {
      skip_list_foreach(priv->segment_cache, (GFunc)find_least_used_slot, &result);
      g_assert(result != NULL);

      cache_slot_t* popped = skip_list_pop(priv->segment_cache, result->segid);
      g_assert(popped == result);
      g_atomic_int_set(&result->segid, -1);
    }

  g_assert(result != NULL);

  gint old_id = g_atomic_int_get(&result->id);
  g_atomic_int_set(&result->valid, FALSE);
  g_atomic_int_set(&result->last_access_time, access_timer());
  g_atomic_int_set(&result->id, old_id + 1);

  return result;
}

static void
segment_cache_bless_slot(cache_slot_t *slot)
{
  gint old_id = g_atomic_int_get(&slot->id);
  g_atomic_int_set(&slot->id, old_id + 1);
  slot->valid = TRUE;
}

static void
set_segment_size(cache_slot_t* slot, gpointer data)
{
  guint size = GPOINTER_TO_UINT(data);
  slot->valid = FALSE;
  slot->buf   = g_renew(gdouble, slot->buf, size);
}

static struct spectrum_iterator*
spectrum_segmented_construct_iterator(HosSpectrum *self)
{
  segmented_ensure_io_thread(HOS_SPECTRUM_SEGMENTED(self));
  struct segmented_iterator* result = g_new0(struct segmented_iterator, 1);

  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);
  result->priv          = priv;
  result->class         = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);
  result->traversal_env = HOS_SPECTRUM_SEGMENTED(self)->traversal_env;

  result->segment_ready_cond = g_cond_new();
  result->wait_lock          = g_mutex_new();
  result->request_queue      = skip_list_new(16, 0.5);
  result->request_queue_lock = g_mutex_new();

  g_mutex_lock(priv->iterators_lock);
  priv->iterators = g_list_append(priv->iterators, result);
  g_mutex_unlock(priv->iterators_lock);

  struct spectrum_iterator* spectrum_iterator = (struct spectrum_iterator*)result;
  spectrum_iterator->tickle     = spectrum_segmented_tickle;
  spectrum_iterator->mark       = spectrum_segmented_mark;
  spectrum_iterator->wait       = spectrum_segmented_wait;

  return (struct spectrum_iterator*)result;
}

static void
spectrum_segmented_free_iterator(struct spectrum_iterator* self)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self->root);

  g_mutex_lock(priv->iterators_lock);
  priv->iterators = g_list_remove(priv->iterators, self);
  g_mutex_unlock(priv->iterators_lock);

  g_free(self);
}


void
spectrum_segmented_set_segment_size(HosSpectrumSegmented *self, guint size)
{
  if (size != SEGMENTED_PRIVATE(self, segment_size))
    {
      SEGMENTED_PRIVATE(self, segment_size) = size;
      
      skip_list_t *segment_cache = SEGMENTED_PRIVATE(self, segment_cache);
      
      skip_list_foreach(SEGMENTED_PRIVATE(self, segment_cache),
			(GFunc)set_segment_size, GUINT_TO_POINTER(size));
    }
}

void
spectrum_segmented_set_cache_size(HosSpectrumSegmented *self, guint size)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);

  priv->segment_cache_max_size = size;
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
  skip_list_print_last_row(SEGMENTED_PRIVATE(self, segment_cache));
}
