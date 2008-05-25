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
  GMutex   *lock;
  gint      segid;
  gdouble  *buf;
};

static cache_slot_t* cache_slot_new                (guint segment_size);

struct _HosSpectrumSegmentedPrivate
{
  GThread *io_thread;

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

  gboolean     valid;

  cache_slot_t *last_slot;

};

static gdouble  spectrum_segmented_wait       (struct spectrum_iterator* self);
static void     spectrum_segmented_mark       (struct spectrum_iterator* self);
static gboolean spectrum_segmented_tickle     (struct spectrum_iterator* self, gdouble* dest);

static struct spectrum_iterator* spectrum_segmented_construct_iterator (HosSpectrum *self);
static void                      spectrum_segmented_free_iterator      (struct spectrum_iterator* self);

static void     spectrum_segmented_io_thread  (HosSpectrumSegmented *self);
static gboolean segmented_fetch_point         (struct segmented_iterator *iterator, gint segid, gint pt_idx, gdouble *dest);


G_DEFINE_ABSTRACT_TYPE (HosSpectrumSegmented, hos_spectrum_segmented, HOS_TYPE_SPECTRUM)

#ifdef G_LOG_DOMAIN
#undef G_LOG_DOMAIN
#endif
#define G_LOG_DOMAIN "spectrum_segmented.c"


static void
null_log_handler(const gchar *log_domain,
		 GLogLevelFlags log_level,
		 const gchar *message,
		 gpointer user_data)
{
  /* do nothing, i.e. suppress debugging output */
}

static void
hos_spectrum_segmented_class_init(HosSpectrumSegmentedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->construct_iterator = spectrum_segmented_construct_iterator;
  spectrum_class->free_iterator      = spectrum_segmented_free_iterator;

  if (g_getenv("DEBUG") == NULL)
    g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG | G_LOG_LEVEL_MESSAGE, null_log_handler, NULL);

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

  GError  *error  = NULL;
  GThread *thread = g_thread_create((GThreadFunc)spectrum_segmented_io_thread,
				    self,
				    FALSE,
				    &error);
  g_assert(error == NULL);
  priv->io_thread = thread;

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

  cache_slot_t* slot = NULL;
  gboolean done = FALSE;

  g_mutex_lock(segmented_iterator->request_queue_lock);
  while (done == FALSE)
    {
      slot = skip_list_lookup(segmented_iterator->segment_cache, segid);
      if (slot != NULL)
	{
	  g_mutex_lock(slot->lock);
	  if (slot->segid == segid)
	    {
	      result = slot->buf[pt];
	      done   = TRUE;
	    }
	  g_mutex_unlock(slot->lock);
	}
      if (done != TRUE)
	{
	  g_assert(segid >= 0);
	  skip_list_insert(segmented_iterator->request_queue, segid, NULL);
	  g_cond_wait(segmented_iterator->segment_ready_cond, segmented_iterator->request_queue_lock);
	}
    }
  g_mutex_unlock(segmented_iterator->request_queue_lock);

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
  gboolean result = FALSE;

  if (iterator->last_slot != NULL)
    {
      g_mutex_lock(iterator->last_slot->lock);
      if (iterator->last_slot->segid == segid)
	{
	  *dest = iterator->last_slot->buf[pt_idx];
	  result = TRUE;
	}
      g_mutex_unlock(iterator->last_slot->lock);
    }

  if (result == FALSE)
    {
      g_mutex_lock(iterator->request_queue_lock);
      
      cache_slot_t* slot = skip_list_lookup(iterator->segment_cache, segid);
      if (slot != NULL)
	{
	  iterator->last_slot = slot;
	  g_mutex_lock(slot->lock);
	  if (slot->segid == segid)
	    {
	      *dest = slot->buf[pt_idx];
	      result = TRUE;
	    }
	  else
	    {
	      skip_list_pop(iterator->segment_cache, segid);
	      result = FALSE;
	    }
	  g_mutex_unlock(slot->lock);
	}
      g_mutex_unlock(iterator->request_queue_lock);
    }

  return result;
}

static gboolean
spectrum_segmented_tickle(struct spectrum_iterator* self, gdouble *dest)
{
  struct segmented_iterator   *segmented_iterator = (struct segmented_iterator*)self;
  HosSpectrumSegmentedPrivate *priv               = segmented_iterator->priv;
  HosSpectrumSegmentedClass   *class              = segmented_iterator->class;

  gint segid, pt;
  class->idx2segment(segmented_iterator->traversal_env, self->idx, &segid, &pt);
  segmented_iterator->segid = segid;
  segmented_iterator->pt    = pt;
  gboolean result = segmented_fetch_point (segmented_iterator, segid, pt, dest);

  if (result == FALSE)
    {
      g_mutex_lock(segmented_iterator->request_queue_lock);
      g_assert(segid >= 0);
      skip_list_insert(segmented_iterator->request_queue, segid, NULL);
      g_mutex_unlock(segmented_iterator->request_queue_lock);
    }
  return result;
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
      g_debug("IO (0x%x, thread 0x%x): lock iterators (0x%x)", self, g_thread_self(), priv->iterators_lock);
      g_debug("IO (0x%x, thread 0x%x): acquired lock", self, g_thread_self());
      g_mutex_lock(priv->iterators_lock);
      while (g_list_length(priv->iterators) == 0)
	{
	  g_debug("IO (0x%x, thread 0x%x): no iterators pending", self, g_thread_self());
	  g_cond_wait(priv->iterators_pending_cond, priv->iterators_lock);
	}

      GList *iterators = priv->iterators;
      for (iterators = priv->iterators; iterators != NULL; iterators = iterators->next)
	{
	  struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)(iterators->data);
	  struct spectrum_iterator  *iterator           = (struct spectrum_iterator*)(iterators->data);

	  g_debug("IO (0x%x, thread 0x%x): acquiring request_queue_lock of 0x%x", self, g_thread_self(), iterator);
	  g_mutex_lock(segmented_iterator->request_queue_lock);
	  g_debug("IO (0x%x, thread 0x%x): acquired lock", self, g_thread_self());

	  if (segmented_iterator->valid)
	    {
	      g_debug("IO (0x%x, thread 0x%x): segment is valid", self, g_thread_self());
	      /* inform the iterator of the last segment read, if appropriate */
	      if (active_slot != NULL)
		{

		  if (active_slot->segid < 0)
		    g_debug("IO (0x%x, thread 0x%x): PROBLEM: active slot 0x%x has segid %d", self, g_thread_self(), active_slot, active_slot->segid);

		  g_assert(active_slot->segid >= 0);

		  /* put a link to active_slot into segment_iterator's segment cache */
		  if (skip_list_has_key(segmented_iterator->request_queue, active_slot->segid))
		    {
		      skip_list_pop(segmented_iterator->request_queue, active_slot->segid);
		      skip_list_insert(segmented_iterator->segment_cache, active_slot->segid, active_slot);
		    }
		  if (active_slot->segid == segmented_iterator->segid_saved)
		    {
		      iterator->blocked = FALSE;
		      g_cond_signal(segmented_iterator->segment_ready_cond);
		    }
		}
	      
	      /* get the next pending request */
	      gint next = skip_list_peek_first(segmented_iterator->request_queue);
	      if (next >= 0)
		skip_list_insert(priv->request_queue, next, NULL);
	    }
	  g_debug("IO (0x%x, thread 0x%x): releasing request_queue_lock of iterator 0x%x", self, g_thread_self(), segmented_iterator);
	  g_mutex_unlock(segmented_iterator->request_queue_lock);
	  g_debug("IO (0x%x, thread 0x%x): released iterator 0x%x", self, g_thread_self(), segmented_iterator);
	}
      g_debug("IO (0x%x, thread 0x%x): unlock iterators (0x%x)", self, g_thread_self(), priv->iterators_lock);
      g_mutex_unlock(priv->iterators_lock);

      gint segid = skip_list_pop_first(priv->request_queue);

      if (segid >= 0)
	{
	  g_debug("IO (0x%x, thread 0x%x): Loading segment %d", self, g_thread_self(), segid);
	  /*
	   * Free a slot.
	   * In this implementation, a victim is chosen at random,
	   * on the theory that this may outperform any least-recently-used algorithm that would
	   * require lots of sorting.
	   */
	  gint idx = g_random_int_range(0, priv->segment_cache->len - 1);
	  active_slot = g_ptr_array_index(priv->segment_cache, idx);
	  g_mutex_lock(active_slot->lock);
	  active_slot->segid=-1;
	  g_mutex_unlock(active_slot->lock);
	  
	  g_assert(active_slot != NULL);
	  g_assert(active_slot->buf != NULL);
	  class->read_segment(self->traversal_env, segid, active_slot->buf);

	  g_mutex_lock(active_slot->lock);
	  active_slot->segid=segid;
	  g_mutex_unlock(active_slot->lock);
	  g_debug("IO (0x%x, thread 0x%x): active slot is 0x%x with segid %d", self, g_thread_self(), active_slot, active_slot->segid);
	}
      else
	{
	  g_debug("IO (0x%x, thread 0x%x): sleep because no segments are pending", self, g_thread_self());
	  g_usleep(5000);
	}
    }
}

static cache_slot_t*
cache_slot_new(guint segment_size)
{
  cache_slot_t* result = g_new0(cache_slot_t, 1);
  result->buf   = g_new(gdouble, segment_size);
  result->segid = -1;
  result->lock  = g_mutex_new();

  return result;
}

static void
set_segment_size(cache_slot_t* slot, guint size)
{
  g_mutex_lock(slot->lock);
  slot->buf   = g_renew(gdouble, slot->buf, size);
  g_mutex_unlock(slot->lock);
}

static struct spectrum_iterator*
spectrum_segmented_construct_iterator(HosSpectrum *self)
{

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

  g_debug("Tr (0x%x): construct: lock iterators (0x%x)", result, priv->iterators_lock);
  g_mutex_lock(priv->iterators_lock);
  g_debug("Tr (0x%x): acquired lock", result);
  priv->iterators = g_list_append(priv->iterators, result);
  g_cond_signal(priv->iterators_pending_cond);
  g_debug("Tr (0x%x): construct: unlock iterators (0x%x)", result, priv->iterators_lock);
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

  struct segmented_iterator *segmented_iterator = (struct segmented_iterator*)self;

  /*
   * In case the IO thread is waiting on this iterator,
   * inform it that there will be no more requests coming.
   */
  g_debug("Tr: about to mark iterator 0x%x as invalid", self);
  g_mutex_lock(segmented_iterator->request_queue_lock);
  segmented_iterator->valid = FALSE;
  g_mutex_unlock(segmented_iterator->request_queue_lock);

  g_debug("Tr: removing iterator 0x%x from iterator list", self);
  g_debug("Tr (0x%x): free: lock iterators (0x%x)", segmented_iterator, priv->iterators_lock);
  g_mutex_lock(priv->iterators_lock);
  priv->iterators = g_list_remove(priv->iterators, self);
  g_debug("Tr (0x%x): free: unlock iterators (0x%x)", segmented_iterator, priv->iterators_lock);
  g_mutex_unlock(priv->iterators_lock);

  g_debug("Tr: about to destroy iterator 0x%x", self);
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

