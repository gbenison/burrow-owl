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
#include "debug.h"

/*
 * Maximum time (usec) a segmented iterator will wait for 'segment_ready_cond' before waking up;
 * prevents a deadlock where the IO thread is starved for requests and all iterators are
 * waiting for their request to be fulfilled.
 */
static glong segmented_wait_delay = 10000;

#define SEGMENTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmentedPrivate))
#define SEGMENTED_PRIVATE(o, field) ((SEGMENTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumSegmentedPrivate HosSpectrumSegmentedPrivate;

typedef struct _cache_slot cache_slot_t;
struct _cache_slot
{
  GStaticRWLock  lock;
  gint           segid;
  gdouble       *buf;
};


struct _HosSpectrumSegmentedPrivate
{
  GThread *io_thread;
  gboolean condemned;
  GThread *deadlock_detection_thread;
  GTimeVal deadline;

  GList   *iterators;
  GMutex  *iterators_lock;
  GCond   *iterators_pending_cond;

  guint    segment_size;

  GPtrArray    *segment_cache;
  skip_list_t  *request_queue;

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
  skip_list_t *segment_cache;
  GMutex      *request_queue_lock;

  GCond       *segment_ready_cond;
  GTimeVal     timeout;

  gboolean     valid;

  cache_slot_t *last_slot;

};

struct _deadline
{
  guint    nsec;
  gboolean satisfied;
};
typedef struct _deadline deadline_t;

static cache_slot_t* cache_slot_new           (guint segment_size);
static cache_slot_t* iterator_fetch_segid     (struct segmented_iterator *iterator,
					       gint segid);
static gdouble  spectrum_segmented_wait       (struct spectrum_iterator* self);
static void     spectrum_segmented_mark       (struct spectrum_iterator* self);
static void     spectrum_segmented_restore    (struct spectrum_iterator* self);
static gboolean spectrum_segmented_probe      (struct spectrum_iterator* self);
static gboolean spectrum_segmented_tickle     (struct spectrum_iterator* self, gdouble* dest);
static void     spectrum_segmented_increment  (struct spectrum_iterator* self, guint dim, gint delta);

static struct spectrum_iterator* spectrum_segmented_construct_iterator (HosSpectrum *self);
static void                      spectrum_segmented_free_iterator      (struct spectrum_iterator* self);

static void     spectrum_segmented_dispose    (GObject *object);

static void     spectrum_segmented_io_thread  (HosSpectrumSegmented *self);
static void     ensure_io_thread              (HosSpectrumSegmented *self);

static void     deadline_set      (deadline_t *self, guint nsec);
static void     deadline_release  (deadline_t *self);
static void     deadline_handler  (deadline_t *self);

static gboolean segmented_acquire_slot        (struct segmented_iterator *iterator, gint segid, gboolean block);


G_DEFINE_ABSTRACT_TYPE (HosSpectrumSegmented, hos_spectrum_segmented, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_segmented_class_init(HosSpectrumSegmentedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose             = spectrum_segmented_dispose;

  spectrum_class->construct_iterator = spectrum_segmented_construct_iterator;
  spectrum_class->free_iterator      = spectrum_segmented_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumSegmentedPrivate));
}

static void
hos_spectrum_segmented_init(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);
  priv->request_queue          = skip_list_new(16, 0.5);
  priv->segment_cache          = g_ptr_array_new();
  priv->iterators_lock         = g_mutex_new();
  priv->iterators_pending_cond = g_cond_new();
  priv->condemned              = FALSE;
  
  spectrum_segmented_set_cache_size(self, 32);
}

static void
spectrum_segmented_dispose (GObject *object)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(object);
  g_atomic_int_set(&(priv->condemned), TRUE);
  if (SEGMENTED_PRIVATE(object, io_thread) != NULL)
    {
      /* wake up IO thread */
      g_mutex_lock(priv->iterators_lock);
      g_cond_signal(priv->iterators_pending_cond);
      g_mutex_unlock(priv->iterators_lock);
      g_thread_join(SEGMENTED_PRIVATE(object, io_thread));
      SEGMENTED_PRIVATE(object, io_thread) = NULL;
    }
  G_OBJECT_CLASS(hos_spectrum_segmented_parent_class)->dispose (object);
}

static void
ensure_io_thread (HosSpectrumSegmented *self)
{
  g_mutex_lock (SEGMENTED_PRIVATE(self, iterators_lock));
  if (SEGMENTED_PRIVATE(self, io_thread) == NULL)
    {
      GError *error = NULL;
      SEGMENTED_PRIVATE(self, io_thread)
	= g_thread_create((GThreadFunc)spectrum_segmented_io_thread,
			  self,
			  TRUE,
			  &error);
      g_assert(error == NULL);
    }
  g_mutex_unlock (SEGMENTED_PRIVATE(self, iterators_lock));
}

static void
spectrum_segmented_mark(struct spectrum_iterator* self)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  segmented_iterator->segid_saved = segmented_iterator->segid;
  segmented_iterator->pt_saved    = segmented_iterator->pt;

  CONFESS("Tr (0x%x): set segid_saved to %d", segmented_iterator, segmented_iterator->segid_saved);
}

static void
spectrum_segmented_restore(struct spectrum_iterator* self)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  segmented_iterator->segid = segmented_iterator->segid_saved;
  segmented_iterator->pt    = segmented_iterator->pt_saved;
}

static gboolean
spectrum_segmented_probe(struct spectrum_iterator *self)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  return segmented_acquire_slot(segmented_iterator, segmented_iterator->segid_saved, FALSE);
}

static gdouble
spectrum_segmented_wait(struct spectrum_iterator* self)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;

  gint segid = segmented_iterator->segid;
  gint pt    = segmented_iterator->pt;

  /* ensure segid and pt are sync'd */
  if (segid < 0)
    segmented_iterator->class->idx2segment(segmented_iterator->traversal_env, self->idx, &segid, &pt);

  g_assert(segmented_acquire_slot(segmented_iterator, segid, TRUE) == TRUE);
  return segmented_iterator->last_slot->buf[pt];
}

/*
 * Return the cache slot of 'iterator' holding segment 'segid',
 * or NULL if 'segid' is not in the cache.
 *
 * postcondition: if result != NULL, (result)->lock is held
 */
static cache_slot_t*
iterator_fetch_segid(struct segmented_iterator *iterator, gint segid)
{
  cache_slot_t *slot = skip_list_lookup(iterator->segment_cache, segid);
  if (slot != NULL)
    {
      g_static_rw_lock_reader_lock(&slot->lock);
      CONFESS("Tr (0x%x): locked slot %x", iterator, slot);
      /* consistency: do the slot index and the slot agree on the segid? */
      if (slot->segid != segid)
	{
	  /* evict from index */
	  skip_list_pop(iterator->segment_cache, segid);
	  g_static_rw_lock_reader_unlock(&slot->lock);
	  CONFESS("Tr (0x%x): unlocked slot %x", iterator, slot);
	  slot = NULL;
	}
    }
  return slot;
}

/*
 * Attempt to acquire a read lock on cache slot 'segid'
 * and store in the field 'last_slot' of 'segmented_iterator'.
 * If 'block' is true, block until 'segid' is available.
 *
 * Precondition:  (iterator->last_slot == NULL) || (read lock is held on iterator->last_slot)
 * Postcondition: (iterator->last_slot == NULL) || (iterator->last_slot->segid == segid && read lock is held)
 *
 * Returns:
 * TRUE:  success, read lock held on 'segid'
 * FALSE: fail, last_slot == NULL
 */
static gboolean
segmented_acquire_slot(struct segmented_iterator *iterator, gint segid, gboolean block)
{
  if ((iterator->last_slot != NULL) && (iterator->last_slot->segid == segid))
    return TRUE;
  else
    {
      if (iterator->last_slot != NULL)
	{
	  g_static_rw_lock_reader_unlock(&iterator->last_slot->lock);
	  CONFESS("Tr (0x%x): unlocked slot %x", iterator, iterator->last_slot);
	}
      iterator->last_slot = NULL;

      gboolean repeat = TRUE;
      g_mutex_lock(iterator->request_queue_lock);
      while (repeat)
	{
	  repeat = FALSE;
	  iterator->last_slot = iterator_fetch_segid(iterator, segid);
	  if ((iterator->last_slot == NULL) && (block == TRUE))
	    {
	      skip_list_insert(iterator->request_queue, segid, NULL);
	      CONFESS("Tr (0x%x): waiting for segid %d", iterator, segid);
	      g_get_current_time(&iterator->timeout);
	      g_time_val_add(&iterator->timeout, segmented_wait_delay);
	      g_cond_timed_wait(iterator->segment_ready_cond,
				iterator->request_queue_lock,
				&iterator->timeout);
	      repeat = TRUE;
	    }
	}
      g_mutex_unlock(iterator->request_queue_lock);
      if (block == TRUE) g_assert(iterator->last_slot != NULL);
      if (iterator->last_slot != NULL)
	g_assert(iterator->last_slot->segid == segid);
      return (iterator->last_slot == NULL) ? FALSE : TRUE;
    }
  g_assert_not_reached();
}

static void
spectrum_segmented_increment(struct spectrum_iterator* self, guint dim, gint delta)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  HosSpectrumSegmentedPrivate *priv               = segmented_iterator->priv;
  HosSpectrumSegmentedClass   *class              = segmented_iterator->class;

  gint segid, pt;
  class->idx2segment(segmented_iterator->traversal_env, self->idx, &segid, &pt);
  segmented_iterator->segid = segid;
  segmented_iterator->pt    = pt;
}

static gboolean
spectrum_segmented_tickle(struct spectrum_iterator* self, gdouble *dest)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  HosSpectrumSegmentedPrivate *priv               = segmented_iterator->priv;
  HosSpectrumSegmentedClass   *class              = segmented_iterator->class;

  gint segid = segmented_iterator->segid;
  gint pt    = segmented_iterator->pt;

  /*ensure segid and pt are sync'd */
  if (segid < 0)
    segmented_iterator->class->idx2segment(segmented_iterator->traversal_env, self->idx, &segid, &pt);

  gboolean result = segmented_acquire_slot (segmented_iterator, segid, FALSE);

  if (result == FALSE)
    {
      g_mutex_lock(segmented_iterator->request_queue_lock);
      g_assert(segid >= 0);
      if (!skip_list_has_key(segmented_iterator->request_queue, segid))
	{
	  CONFESS("Tr (0x%x): tickled segment %d", segmented_iterator, segid);
	  skip_list_insert(segmented_iterator->request_queue, segid, NULL);
	}
      g_mutex_unlock(segmented_iterator->request_queue_lock);
    }
  else
    *dest = segmented_iterator->last_slot->buf[pt];
  return result;
}

static void
deadline_set(deadline_t *self, guint nsec)
{
  self->nsec = nsec;
  self->satisfied = FALSE;

  GError *error = NULL;
  g_thread_create((GThreadFunc)deadline_handler,
		  self,
		  FALSE,
		  &error);
  g_assert(error == NULL);
}

static void
deadline_release(deadline_t *self)
{
  self->satisfied = TRUE;
}

static void
deadline_handler(deadline_t *self)
{
  g_usleep(1000000 * self->nsec);
      
  if (self->satisfied == FALSE)
    g_error("segmented spectrum: deadlock detected");

}

static void
spectrum_segmented_io_thread(HosSpectrumSegmented *self)
{
  HosSpectrumSegmentedPrivate *priv  = SEGMENTED_GET_PRIVATE(self);
  HosSpectrumSegmentedClass   *class = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);

  cache_slot_t *active_slot = NULL;

  while (1)
    {
      /* maintainance on all active iterators */
      CONFESS("IO (0x%x): lock iterators (0x%x)", self, priv->iterators_lock);
      CONFESS("IO (0x%x): acquired lock", self);
      g_mutex_lock(priv->iterators_lock);
      while (g_list_length(priv->iterators) == 0)
	{
	  CONFESS("IO (0x%x): no iterators pending", self);
	  if (g_atomic_int_get(&(priv->condemned)) == TRUE)
	    goto done;
	  g_cond_wait(priv->iterators_pending_cond, priv->iterators_lock);
	}

      GList *iterators = priv->iterators;
      for (iterators = priv->iterators; iterators != NULL; iterators = iterators->next)
	{
	  struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)(iterators->data);
	  struct spectrum_iterator  *iterator           = (struct spectrum_iterator*)(iterators->data);

	  CONFESS("IO (0x%x): acquiring request_queue_lock of 0x%x", self, iterator);
	  g_mutex_lock(segmented_iterator->request_queue_lock);
	  CONFESS("IO (0x%x): acquired lock", self);

	  if (segmented_iterator->valid)
	    {
	      CONFESS("IO (0x%x): segment is valid", self, g_thread_self());
	      /* inform the iterator of the last segment read, if appropriate */
	      if (active_slot != NULL)
		{
		  if (active_slot->segid < 0)
		    CONFESS("IO (0x%x): PROBLEM: active slot 0x%x has segid %d", self,  active_slot, active_slot->segid);

		  g_assert(active_slot->segid >= 0);

		  /* put a link to active_slot into segment_iterator's segment cache */
		  if (skip_list_has_key(segmented_iterator->request_queue, active_slot->segid))
		    {
		      skip_list_pop(segmented_iterator->request_queue, active_slot->segid);
		      skip_list_insert(segmented_iterator->segment_cache, active_slot->segid, active_slot);
		    }
		  if (active_slot->segid == segmented_iterator->segid_saved)
		    g_cond_signal(segmented_iterator->segment_ready_cond);
		}
	      
	      /* get the next pending request */
	      gint next = skip_list_peek_first(segmented_iterator->request_queue);
	      if (next >= 0)
		skip_list_insert(priv->request_queue, next, NULL);
	    }
	  CONFESS("IO (0x%x): releasing request_queue_lock of iterator 0x%x", self,  segmented_iterator);
	  g_mutex_unlock(segmented_iterator->request_queue_lock);
	  CONFESS("IO (0x%x): released iterator 0x%x", self,  segmented_iterator);
	}
      CONFESS("IO (0x%x): unlock iterators (0x%x)", self,  priv->iterators_lock);
      g_mutex_unlock(priv->iterators_lock);

      gint segid = skip_list_pop_first(priv->request_queue);

      if (segid >= 0)
	{
	  CONFESS("IO (0x%x): Loading segment %d", self,  segid);

	  /*
	   * Free a slot.
	   * In this implementation, a victim is chosen at random,
	   * on the theory that this may outperform any least-recently-used algorithm that would
	   * require lots of sorting.
	   */
	  static guint max_tries = 5;
	  gint i;
	  /* try to find an idle slot... */
	  for (i = 0; i < max_tries; ++i)
	    {
	      gint idx = g_random_int_range(0, priv->segment_cache->len - 1);
	      active_slot = g_ptr_array_index(priv->segment_cache, idx);
	      
	      if (g_static_rw_lock_writer_trylock(&active_slot->lock))
		break;
	      else
		active_slot = NULL;
	      
	      CONFESS("IO (0x%x): collision with slot %d", self,  idx);
	    }
	  /* finally pick one and block */
	  if (active_slot == NULL)
	    {
	      deadline_t deadline;
	      
	      /* FIXME can deadlock if there is a reader lock on active_slot */
	      gint idx = g_random_int_range(0, priv->segment_cache->len - 1);
	      active_slot = g_ptr_array_index(priv->segment_cache, idx);
	      CONFESS("IO (0x%x): seizing slot %d", self,  idx);
	      deadline_set(&deadline, 1);
	      g_static_rw_lock_writer_lock(&active_slot->lock);
	      deadline_release(&deadline);
	      CONFESS("IO (0x%x): seized slot %d; segid %d", self, idx, active_slot->segid);
	    }

	  g_assert(active_slot != NULL);
	  g_assert(active_slot->buf != NULL);
	  class->read_segment(self->traversal_env, segid, active_slot->buf);
	  active_slot->segid=segid;
	  g_static_rw_lock_writer_unlock(&active_slot->lock);
	  CONFESS("IO (0x%x): active slot is 0x%x with segid %d", self,  active_slot, active_slot->segid);
	}
      else
	{
	  CONFESS("IO (0x%x): sleep because no segments are pending", self, g_thread_self());
	  active_slot = NULL;
	  if (g_atomic_int_get(&(priv->condemned)) == TRUE)
	    goto done;
	  g_usleep(5000);
	}
    }
 done:
  1 == 1;
}

static cache_slot_t*
cache_slot_new(guint segment_size)
{
  cache_slot_t* result = g_new0(cache_slot_t, 1);
  result->buf   = g_new(gdouble, segment_size);
  result->segid = -1;
  g_static_rw_lock_init(&result->lock);

  return result;
}

static void
set_segment_size(cache_slot_t* slot, guint size)
{
  g_static_rw_lock_writer_lock(&slot->lock);
  slot->buf   = g_renew(gdouble, slot->buf, size);
  g_static_rw_lock_writer_unlock(&slot->lock);
}

static struct spectrum_iterator*
spectrum_segmented_construct_iterator(HosSpectrum *self)
{
  ensure_io_thread(HOS_SPECTRUM_SEGMENTED(self));

  struct segmented_iterator* result = g_new0(struct segmented_iterator, 1);
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);

  result->priv          = priv;
  result->class         = HOS_SPECTRUM_SEGMENTED_GET_CLASS(self);
  result->traversal_env = HOS_SPECTRUM_SEGMENTED(self)->traversal_env;

  result->request_queue      = skip_list_new(16, 0.5);
  result->segment_cache      = skip_list_new(16, 0.5);
  result->request_queue_lock = g_mutex_new();
  result->segment_ready_cond = g_cond_new();
  
  result->valid              = TRUE;

  CONFESS("Tr (0x%x): construct: lock iterators (0x%x)", result, priv->iterators_lock);
  g_mutex_lock(priv->iterators_lock);
  CONFESS("Tr (0x%x): acquired lock", result);
  priv->iterators = g_list_append(priv->iterators, result);
  g_cond_signal(priv->iterators_pending_cond);
  CONFESS("Tr (0x%x): construct: unlock iterators (0x%x)", result, priv->iterators_lock);
  g_mutex_unlock(priv->iterators_lock);

  struct spectrum_iterator* spectrum_iterator = (struct spectrum_iterator*)result;
  spectrum_iterator->tickle     = spectrum_segmented_tickle;
  spectrum_iterator->mark       = spectrum_segmented_mark;
  spectrum_iterator->restore    = spectrum_segmented_restore;
  spectrum_iterator->wait       = spectrum_segmented_wait;
  spectrum_iterator->probe      = spectrum_segmented_probe;
  spectrum_iterator->increment  = spectrum_segmented_increment;

  /* ensure invalid values for result->segid so that sync is triggered */
  result->segid = -1;
  result->pt    = -1;

  return (struct spectrum_iterator*)result;
}

static void
spectrum_segmented_free_iterator(struct spectrum_iterator* self)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self->root);

  struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)self;

  if (segmented_iterator->last_slot != NULL)
    {
      g_static_rw_lock_reader_unlock(&segmented_iterator->last_slot->lock);
      CONFESS("Tr (0x%x): unlocked slot %x", segmented_iterator, segmented_iterator->last_slot);
      segmented_iterator->last_slot = NULL;
    }

  /*
   * In case the IO thread is waiting on this iterator,
   * inform it that there will be no more requests coming.
   */
  CONFESS("Tr: about to mark iterator 0x%x as invalid", self);
  g_mutex_lock(segmented_iterator->request_queue_lock);
  segmented_iterator->valid = FALSE;
  g_mutex_unlock(segmented_iterator->request_queue_lock);

  CONFESS("Tr: removing iterator 0x%x from iterator list", self);
  CONFESS("Tr (0x%x): free: lock iterators (0x%x)", segmented_iterator, priv->iterators_lock);
  g_mutex_lock(priv->iterators_lock);
  priv->iterators = g_list_remove(priv->iterators, self);
  CONFESS("Tr (0x%x): free: unlock iterators (0x%x)", segmented_iterator, priv->iterators_lock);
  g_mutex_unlock(priv->iterators_lock);

  CONFESS("Tr: freeing iterator 0x%x", self);
  g_free(self);
}

void
spectrum_segmented_set_segment_size(HosSpectrumSegmented *self, guint size)
{
  if (size != SEGMENTED_PRIVATE(self, segment_size))
    {
      SEGMENTED_PRIVATE(self, segment_size) = size;
      
      GPtrArray *segment_cache = SEGMENTED_PRIVATE(self, segment_cache);
      gint i;
      for (i = 0; i < segment_cache->len; ++i)
	set_segment_size(g_ptr_array_index(segment_cache, i), size);
    }
}

void
spectrum_segmented_set_cache_size(HosSpectrumSegmented *self, guint size)
{
  HosSpectrumSegmentedPrivate *priv = SEGMENTED_GET_PRIVATE(self);
  
  while (priv->segment_cache->len < size)
    g_ptr_array_add(priv->segment_cache, cache_slot_new(priv->segment_size));
}

