/*
 *  Copyright (C) 2005, 2006, 2008, 2009 Greg Benison
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

/*
 * this is a compatibility macro; it must come before any header
 * includes.  It defines which features of the C library will be
 * available.  In this file it is needed for proper round() behavior
 * (at least).
 */
/* GCB 9jun07 GNU_SOURCE seems to be causing problems */
#define _GNU_SOURCE

/* #include <stdlib.h> */
#include <assert.h>
#include <string.h>
#include <math.h>
#include <glib.h>
#include "spectrum.h"
#include "spectrum_priv.h"
#include "debug.h"

/* for debugging purposes; make value available */
static const gdouble datum_unknown_value_substitute=DATUM_UNKNOWN_VALUE_SUBSTITUTE;

#define SPECTRUM_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM, HosSpectrumPrivate))
#define SPECTRUM_PRIVATE(o, field) ((SPECTRUM_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumPrivate HosSpectrumPrivate;

struct _HosSpectrumPrivate
{
  GMutex   *traversal_lock;
  GList    *dimensions;

  gboolean  valid;

  gint      schedule_id;

  gboolean  instantiable;
  gboolean  instantiable_known;
};

/**
@defgroup HosSpectrum
@brief    A GObject representing an NMR spectrum and its metadata

@verbatim #include <burrow/spectrum.h> @endverbatim

All NMR spectra in burrow-owl are represented by 'HosSpectrum' objects.
There are many functions for manipulating HosSpectrum objects, for example:
loading them from files, extracting regions, projecting slices, and combining
them into higher-order spectra.

Two important characteristics of HosSpectrum objects are
<b>referential transparency</b> and <b>closure</b>.
This means that the functions operating on spectrum objects
do not have side effects (they do not modify their arguments)
and that a HosSpectrum object returned by any function can
be used as an argument for any other function.
For example, a function expecting a two-dimensional spectrum
as an argument could accept any of the following: a 2D spectrum
read from a file, a 2D spectrum resulting from calling
spectrum_convolute() on two 1D spectra, or a 2D spectrum
resulting from calling spectrum_project() on a 3D spectrum.
Spectra used as arguments are not modified- for example,
let S1 be a spectrum and let S2 = spectrum_transpose(S1, 1).
S1 is unchanged by this operation and S2 is totally
independent of S1.

@{

 */

/*
 * FIXME documentation:
 * describe referential transparency and closure properties
 */

enum spectrum_properties {
  PROP_0,
  PROP_READY  /**< True if buffer contents are available */
};

enum spectrum_signals {
  READY,       /**< Traversal is complete; spectrum_traverse() will succeed */
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

static void hos_spectrum_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void hos_spectrum_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);

struct _traversal_token {
  HosSpectrum *spectrum;
  gboolean valid;
};

static traversal_token_t* token_new  (HosSpectrum *spec);
static void               token_free (traversal_token_t *token);

static void          spectrum_traverse_internal (traversal_token_t *token);
static void          spectrum_traverse_and_queue(traversal_token_t *token);
static gboolean      spectrum_signal_ready      (HosSpectrum* self);
static void          spectrum_ensure_buffer     (HosSpectrum *self);

static gboolean      spectrum_bump_idx          (HosSpectrum* self, gint* idx);

static void          ensure_traversal_setup     (void);
static HosSpectrum*  spectrum_ready_pop         (void);

static void          signal_spectra_ready       (void);
static gboolean      idle_spectra_ready         (gpointer not_used);

static dimension_t*  spectrum_fetch_dimension   (HosSpectrum* self, const guint dim);
static gboolean      spectrum_grab_cached       (HosSpectrum* self, guint *idx, gdouble* dest);
static void          spectrum_push_cached       (HosSpectrum *self, guint *idx, gdouble value);

static void          point_cache_store          (HosSpectrum *spec, gsize idx, gdouble value);
static gdouble       point_cache_fetch          (HosSpectrum *spec, gsize idx);

static struct spectrum_iterator* spectrum_iterator_cached(HosSpectrum *self);

G_DEFINE_ABSTRACT_TYPE (HosSpectrum, hos_spectrum, G_TYPE_OBJECT)

/* Iterators */
static gboolean iterator_bump        (struct spectrum_iterator *self);
static gboolean iterator_check_cache (struct spectrum_iterator *self, gdouble *dest);

/** Traversal pool **/

static GAsyncQueue* ready_queue          = NULL;
static GThreadPool* traversal_pool       = NULL;

static gint compare_spectrum_priority(HosSpectrum *a, HosSpectrum *b);
static void ensure_traversal_setup();

static int      compare_gdoubles     (void* A_ptr, void* B_ptr);
static void     check_dim_count      (HosSpectrum* spec, const guint dim);

/**
 * @brief   Instantiate 'spec', asynchronously.
 *
 * Does not block the calling thread.
 * 'spec' will emit the 'ready' signal when traversal is done.
 * Returns a 'traversal token' which can be passed to
 * spectrum_traverse_cancel() if the caller decides 'spec'
 * no longer needs to be instantiated.
 *
 * @param    spec   The HosSpectrum to traverse
 * @returns  traversal token
 */
traversal_token_t*
spectrum_traverse(HosSpectrum *spec)
{
  if (spectrum_is_ready(spec))
    return NULL;

  spectrum_ensure_buffer(spec);

  traversal_token_t *result = token_new(spec);

  static gint schedule_id = 1;

  g_mutex_lock(SPECTRUM_PRIVATE(spec, traversal_lock));

  if (SPECTRUM_PRIVATE(spec, valid) == FALSE)
    {
      ensure_traversal_setup();
      SPECTRUM_PRIVATE(spec, schedule_id) = schedule_id;
      ++schedule_id;
      g_thread_pool_push(traversal_pool, result, NULL);
    }
  g_mutex_unlock(SPECTRUM_PRIVATE(spec, traversal_lock));

  return result;
}

/**
 * @brief   Cancel traversal of the spectrum referenced by 'token'
 *
 * Frees 'token'-- on return, 'token' is no longer valid
 */
void
spectrum_traverse_cancel(traversal_token_t *token)
{
  /* FIXME */
}

/**
 * \brief Blocking version of spectrum_traverse()
 *
 * Blocks the calling thread until 'spec' is instantiated.
 *
 * @param    spec  the spectrum object to traverse
 */
void
spectrum_traverse_blocking(HosSpectrum *spec)
{
  if (spectrum_is_ready(spec))
    return;

  spectrum_ensure_buffer (spec);

  traversal_token_t *token = token_new(spec);

  g_mutex_lock(SPECTRUM_PRIVATE(spec, traversal_lock));
  if (SPECTRUM_PRIVATE(spec, valid) != TRUE)
    spectrum_traverse_internal(token);  
  SPECTRUM_PRIVATE(spec, valid) = TRUE;
  g_mutex_unlock(SPECTRUM_PRIVATE(spec, traversal_lock));

  token_free(token);
}

/**
 * @brief  Get the value of one point in a spectrum
 *
 * @param  spec  The spectrum from which the value is drawn
 * @param  idx   Linearized index
 *
 * The linearized index is determined by lining up the spectrum points
 * with the leading dimension incrementing fastest, i.e.
 * (0, 0, 0, ...), (1, 0, 0, ...) etc.
 *
 * Will block the calling thread until the requested point value
 * is available.
 */
gdouble
spectrum_peek(HosSpectrum *spec, guint idx)
{
  spectrum_traverse_blocking(spec);
  gdouble* buf  = spec->buf;
  g_assert(buf != NULL);
  int spec_size = spectrum_np_total(spec);

  g_return_if_fail(idx < spec_size);

  return buf[idx];
}

/**
 * @brief    Query the number of dimensions of a spectrum object
 * @returns  The number of dimensions of 'spec'
 */
gsize
spectrum_ndim(HosSpectrum *spec)
{
  return g_list_length(SPECTRUM_PRIVATE(spec, dimensions));
}

/**
 * @brief    The number of points in a dimension of a spectrum
 * @returns  The number of points in dimension 'dim' of HosSpectrum 'spec'
 */
gsize
spectrum_np(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->np;
}

/**
 * @brief    The total number of points in a spectrum
 *
 * The total number of points is the product of the number of points
 * in each dimension.
 */
gsize
spectrum_np_total(HosSpectrum *spec)
{
  int i, result = 1;
  for (i = 0; i < spectrum_ndim(spec); ++i)
    result *= spectrum_np(spec, i);

  return result;
}

/**
 * @brief The sweep width (in Hz) of spectrum 'spec' in dimension 'dim'
 */
gdouble
spectrum_sw(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->sw;
}

/**
 * @brief The sweep width (in ppm) of spectrum 'spec' in dimension 'dim'
 */
gdouble
spectrum_sw_ppm(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_sw(spec, dim) / spectrum_sf(spec, dim);
}

/**
 * @brief The carrier frequency (in MHz) of spectrum 'spec' in dimension 'dim'
 */
gdouble
spectrum_sf(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->sf;
}

/**
 * @brief  Frequency (in Hz) of point 0 in dimension 'dim' of 'spec'
 */
gdouble
spectrum_orig(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->orig;
}

/**
 * @brief  Chemical shift (in ppm) of point 0 in dimension 'dim' of 'spec'
 */
gdouble
spectrum_orig_ppm(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return spectrum_orig(spec, dim) / spectrum_sf(spec, dim);
}

/**
 * @brief  Frequency (in Hz) of point (np - 1) of dimension 'dim' of 'spec'
 */
gdouble
spectrum_giro(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return (spectrum_orig(spec, dim) -
	  spectrum_sw(spec, dim));
}

/**
 * @brief  Chemical shift (in ppm) of point (np - 1) of dimension 'dim' of 'spec'
 */
gdouble
spectrum_giro_ppm(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return spectrum_giro(spec, dim) / spectrum_sf(spec, dim);
}

/**
 * @brief  convert chemical shift in ppm to spectral points
 *
 * @param  spec  A HosSpectrum object
 * @param  dim   A dimension number, < spectrum_ndim(spec)
 * @param  ppm   A chemical shift in ppm
 *
 * @returns Spectral point corresponding to 'ppm'.  Can be fractional-- e.g.
 *          if the value is '5.5', the requested ppm is midway between spectral
 *          points 5 and 6.
 */
gdouble
spectrum_ppm2pt(HosSpectrum* spec,
		const guint dim,
		const gdouble ppm)
{
  dimension_t* dimen = spectrum_fetch_dimension(spec, dim);

  gdouble hz     = ppm * dimen->sf;
  gdouble result = ((dimen->orig - hz) / dimen->sw) * dimen->np;

  /* FIXME should clamping behavior be the default?? */
  if (result < 0) result = 0;
  if (result >= dimen->np) result = (dimen->np - 1);

  return result;
}

/**
 * @brief  convert an index in spectral points to a chemical shift in ppm
 *
 * @param  spec  A HosSpectrum object
 * @param  dim   A dimension index, < spectrum_ndim(spec)
 * @param  pt    A spectral point index
 *
 * @returns Chemical shift in ppm of point 'pt' in dimension 'dim' of 'spec'
 */
gdouble
spectrum_pt2ppm(HosSpectrum* spec, guint dim, gdouble pt)
{
  dimension_t* dimen = spectrum_fetch_dimension(spec, dim);

  gdouble hz =
    dimen->orig - (dimen->sw * ((gdouble)pt / (gdouble)(dimen->np)));

  return hz / dimen->sf;
}

/**
 * @brief   The mean value of a spectrum
 *
 * @returns The mean intensity, taken over all points in the spectrum
 */
gdouble
spectrum_mean(HosSpectrum *spec)
{
  spectrum_traverse_blocking(spec);
  gdouble* buf = spec->buf;
  int spec_size = spectrum_np_total(spec);
  gdouble result = 0;
  int i;

  for (i = 0; i < spec_size; ++i)
    result += buf[i];

  return result / spec_size;
}

/**
 * @brief    The standard deviation of a spectrum
 *
 * @returns  The standard deviation, taken over all points in a spectrum
 */
gdouble
spectrum_stddev(HosSpectrum *spec)
{
  spectrum_traverse_blocking(spec);
  gdouble* buf = spec->buf;
  int spec_size = spectrum_np_total(spec);
  gdouble result = 0;
  gdouble mean = spectrum_mean(spec);
  int i;

  for (i = 0; i < spec_size; ++i)
    {
      gdouble delta = buf[i] - mean;
      result += delta * delta;
    }

  return sqrt(result / spec_size);
}

/*
 * Emit the 'ready' signal from spectrum 'self',
 * indicating that traversal is complete,
 * but only if 'self' really is ready
 */
static gboolean
spectrum_signal_ready(HosSpectrum* self)
{

  if (spectrum_is_ready(self))
    g_signal_emit(self, signals[READY], 0);
}

static traversal_token_t*
token_new  (HosSpectrum *spec)
{
  g_return_if_fail(HOS_IS_SPECTRUM(spec));
  traversal_token_t *result = g_new(traversal_token_t, 1);
  result->valid = TRUE;
  result->spectrum = spec;
  g_object_ref(spec);
  
  return result;
}

static void
token_free (traversal_token_t *token)
{
  g_return_if_fail(HOS_IS_SPECTRUM(token->spectrum));
  g_object_unref (token->spectrum);
  token->spectrum = NULL;
  g_free(token);
}

static int
compare_gdoubles(void* A_ptr, void* B_ptr)
{
  gdouble A = *(gdouble*)A_ptr;
  gdouble B = *(gdouble*)B_ptr;

  if (A == B) return 0;
  return (A < B) ? -1 : 1;
}

/**
 * @brief The maximum value of a spectrum
 */
gdouble
spectrum_get_max(HosSpectrum *spec)
{
  return spectrum_get_ranked(spec, spectrum_np_total(spec) - 1);
}

/**
 * @brief  The minimum value of a spectrum
 */
gdouble
spectrum_get_min(HosSpectrum *spec)
{
  return spectrum_get_ranked(spec, 0);
}

/**
 * @brief    The intensity cutoff for a percentile
 *
 * @param    spec        The spectrum object to examine
 * @param    percentile  the cutoff value (0.0 -> 1.0)
 * @returns  'percentile' fraction of all points in the spectrum are below the returned intensity
 */
gdouble
spectrum_get_percentile(HosSpectrum *spec, gdouble percentile)
{
  g_assert((percentile >= 0) && (percentile < 1.0));
  return spectrum_get_ranked(spec, percentile * spectrum_np_total(spec));
}

/**
 * @brief  Return the n'th-lowest intensity in a spectrum
 */
gdouble
spectrum_get_ranked(HosSpectrum *spec, guint n)
{
  gint np_total = spectrum_np_total(spec);
  gdouble* buf  = g_new(gdouble, np_total);

  g_assert(n < np_total);

  gdouble result;

  spectrum_traverse_blocking(spec);
  memcpy(buf, spec->buf, np_total * sizeof(gdouble));

  qsort(buf, np_total, sizeof(gdouble), compare_gdoubles);
  result = buf[n];
  g_free(buf);

  return result;
}

/**
 * @}
 */

static void
hos_spectrum_finalize(GObject *object)
{
  HosSpectrum *spectrum = HOS_SPECTRUM(object);
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(object);
  g_mutex_lock(priv->traversal_lock);
  priv->valid = FALSE;
  g_mutex_unlock(priv->traversal_lock);

  gpointer buf = spectrum->buf;
  g_atomic_pointer_set(&spectrum->buf, NULL);
  if (buf != NULL)
    g_free(buf);

  G_OBJECT_CLASS(hos_spectrum_parent_class)->finalize (object);

}

static void
hos_spectrum_class_init (HosSpectrumClass *klass)
{
  GObjectClass *gobject_class;

  g_assert(DATUM_UNKNOWN_VALUE != DATUM_UNKNOWN_VALUE_SUBSTITUTE);

  gobject_class = G_OBJECT_CLASS (klass);
  
  gobject_class->set_property = hos_spectrum_set_property;
  gobject_class->get_property = hos_spectrum_get_property;

  gobject_class->finalize = hos_spectrum_finalize;

  signals[READY] =
    g_signal_new ("ready",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET(HosSpectrumClass, ready),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  g_object_class_install_property (gobject_class,
                                   PROP_READY,
                                   g_param_spec_boolean ("ready",
							 "Ready",
							 "The spectrum is ready to be accessed.",
							 FALSE,
							 G_PARAM_READABLE));

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumPrivate));

  ready_queue = g_async_queue_new();

}

static void
hos_spectrum_init(HosSpectrum  *spectrum)
{
  SPECTRUM_PRIVATE(spectrum, valid) = FALSE;
  SPECTRUM_PRIVATE(spectrum, traversal_lock) = g_mutex_new();
}

static void
hos_spectrum_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  HosSpectrum *spec = HOS_SPECTRUM(object);

  spec=spec; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_LABEL:
      gtk_button_set_label (button, g_value_get_string (value));
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

gboolean
spectrum_is_ready(HosSpectrum *self)
{
  /* flush ready queue */
  while (spectrum_ready_pop() != NULL)
    if (SPECTRUM_PRIVATE(self, valid) == TRUE)
      break;

  if (SPECTRUM_PRIVATE(self, valid) == TRUE)
    g_assert(self->buf != NULL);
  return SPECTRUM_PRIVATE(self, valid);
}

static void
hos_spectrum_get_property (GObject         *object,
			   guint            prop_id,
			   GValue          *value,
			   GParamSpec      *pspec)
{

  switch (prop_id)
    {
    case PROP_READY:
      g_value_set_boolean (value, spectrum_is_ready(HOS_SPECTRUM(object)));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/******* the traversal thread *****/

static gint
compare_spectrum_priority(HosSpectrum *a, HosSpectrum *b)
{
  return
    SPECTRUM_PRIVATE(b, schedule_id) -
    SPECTRUM_PRIVATE(a, schedule_id);
}

static void
ensure_traversal_setup()
{
  if (!g_thread_supported ()) g_thread_init (NULL);

  if (traversal_pool == NULL)
    {
      gpointer user_data = NULL;
      gint max_threads   = 8;    /* FIXME this should be adjustable */
      gboolean exclusive = TRUE;
      GError*  error     = NULL;
      traversal_pool = g_thread_pool_new ((GFunc)spectrum_traverse_and_queue,
					  user_data,
					  max_threads,
					  exclusive,
					  &error);

      /* FIXME priority sorting is disabled */
#ifdef UNDEF
      g_thread_pool_set_sort_function(traversal_pool, (GCompareDataFunc)compare_spectrum_priority, NULL);
#endif
      g_assert(error == NULL);
    }
}

/*
 * The return value was previously passed as the argument
 * to 'spectrum_traverse'.
 */
static HosSpectrum*
spectrum_ready_pop()
{
  traversal_token_t *token = g_async_queue_try_pop(ready_queue);
  HosSpectrum *result = NULL;

  if (token != NULL)
    {
      if (token->valid == TRUE)
	{
	  HosSpectrum *spec = token->spectrum;
	  g_assert(HOS_IS_SPECTRUM(spec));
	  g_mutex_lock(SPECTRUM_PRIVATE(spec, traversal_lock));
	  g_assert(SPECTRUM_PRIVATE(spec, valid) == TRUE);
	  g_mutex_unlock(SPECTRUM_PRIVATE(spec, traversal_lock));
	  spectrum_signal_ready(spec);
	}
    }
  return result;
}

static gboolean
idle_spectra_ready(gpointer not_used)
{
  HosSpectrum *next = spectrum_ready_pop();
  return (next == NULL) ? FALSE : TRUE;
}

/*
 * signal the main thread that spectra have been traversed and are ready
 * for use.
 */
static void
signal_spectra_ready(void)
{
  /* FIXME could this be done with a glib main-loop 'source'? */
  g_idle_add((GSourceFunc)idle_spectra_ready, NULL);
}

/*
 * increment 'idx' in traversal order of 'self.
 *
 * returns:
 *   TRUE:  reached end point and wrapped around
 *   FALSE: not at end of spectrum
 */
static gboolean
spectrum_bump_idx(HosSpectrum* self, gint* idx)
{
  int dim = 0;
  while (1)
    {
      if (dim >= spectrum_ndim(self))
	return TRUE;
      idx[dim]++;
      if (idx[dim] < spectrum_np(self, dim))
	break;
      idx[dim] = 0;
      ++dim;
    }
  return FALSE;
}

static void
spectrum_traverse_and_queue(traversal_token_t *token)
{
  HosSpectrum *spec = token->spectrum;
  g_mutex_lock(SPECTRUM_PRIVATE(spec, traversal_lock));
  if (SPECTRUM_PRIVATE(spec, valid) == FALSE)
    {
      spectrum_traverse_internal(token);
      SPECTRUM_PRIVATE(spec, valid) = TRUE;
      g_async_queue_push(ready_queue, token);
      signal_spectra_ready();
    }
  g_mutex_unlock(SPECTRUM_PRIVATE(spec, traversal_lock));
}

static void
spectrum_ensure_buffer(HosSpectrum *self)
{
  g_return_if_fail(HOS_IS_SPECTRUM(self));
  if (self->buf == NULL)
    {
      guint np = spectrum_np_total(self);
      self->buf = g_new(gdouble, np);
      int i;
      for (i = 0; i < np; ++i)
	self->buf[i] = DATUM_UNKNOWN_VALUE;
    }
}

/*
 * Fill the buffer of 'self'.
 */
static void
spectrum_traverse_internal(traversal_token_t *token)
{
  HosSpectrum *self = token->spectrum;

  g_return_if_fail(HOS_IS_SPECTRUM(self));
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);
  
  g_assert(self->buf != NULL);
  gdouble *buf = self->buf;
  
  struct spectrum_iterator *iterator = spectrum_construct_iterator(self);
  gdouble* outer_dest = buf;
  
#define ALREADY_INSTANTIATED(x) DATUM_IS_KNOWN(x)
  
  /* outer loop through all spectrum points */
  while (1)
    {
      CONFESS("Iterator 0x%x, considering point %d", iterator, (int)(outer_dest - buf));
      
      /* inner loop -- tickle remaining points */
      static const gboolean lookahead_enable = TRUE;
      static const guint lookahead_probe_interval = 128;
      if (!ALREADY_INSTANTIATED(*outer_dest))
	{
	  if (iterator_tickle(iterator, outer_dest) == FALSE)
	    {
	      iterator_mark(iterator);
	      gint n = 0;
	      
	      if (lookahead_enable)
		{
		  gdouble* inner_dest = outer_dest;
		  while (iterator_bump(iterator) == FALSE)
		    {
		      ++inner_dest;
		      if (!ALREADY_INSTANTIATED(*inner_dest))
			iterator_tickle(iterator, inner_dest);
		      ++n;
		      if ((n % lookahead_probe_interval) == 0)
			if (iterator_probe(iterator))
			  {
			    CONFESS("Iterator 0x%x has become unblocked, stopping tickles", iterator);
			    break;
			  }
		    }
		  CONFESS("Iterator 0x%x has reached the end of its tickles", iterator);
		}
	      
	      CONFESS("Iterator 0x%x, forcing point %d", iterator, (int)(outer_dest - buf));
	      iterator_restore(iterator);
	      if (!ALREADY_INSTANTIATED(*outer_dest))
		*outer_dest = iterator_wait(iterator);
	    }
	}
      ++outer_dest;
      if (iterator_bump(iterator))
	break;
    }
  iterator_free(iterator);
}


static void
spectrum_determine_instantiable(HosSpectrum* self, HosSpectrumPrivate *priv)
{
  if (priv->instantiable_known == TRUE)
    return;
  else
    {
      /* set 'instantiable' to false if spectrum_np_total would overflow a 'gsize'. */
      gboolean instantiable_result = TRUE;
      gint dim;
      gsize result = 1;
      for (dim = 0; dim < spectrum_ndim(self); ++dim)
	{
	  gsize next = spectrum_np(self, dim);
	  if ((G_MAXSIZE / result) <= next)
	    instantiable_result = FALSE;
	  result *= next;
	}
      priv->instantiable = instantiable_result;
      priv->instantiable_known = TRUE;
    }
}

/*
 * If 'self' is already instantiated, set *dest and return TRUE.
 * If 'self[idx]' is cached, set *dest and return TRUE.
 * Otherwise, return FALSE.
 */
static gboolean
spectrum_grab_cached(HosSpectrum* self, guint *idx, gdouble* dest)
{
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);

  spectrum_determine_instantiable(self, priv);

  if (priv->instantiable)
    {
      /* collapse idx */
      gint stride = 1;
      gsize linear_idx = 0;
      GList* dimensions;

      for (dimensions = priv->dimensions; dimensions != NULL; dimensions = dimensions->next)
	{
	  linear_idx += stride * *idx;
	  stride *= ((dimension_t*)(dimensions->data))->np;
	  ++idx;
	}

      gdouble *buf = g_atomic_pointer_get(&self->buf);
      if (buf != NULL)
	{
	  *dest = buf[linear_idx];
	  return TRUE;
	}
      else
	{
	  gdouble cached_value = point_cache_fetch(self, linear_idx);
	  if (DATUM_IS_KNOWN(cached_value))
	    {
	      *dest = cached_value;
	      return TRUE;
	    }
	}
    }

  return FALSE;
}

static void
spectrum_push_cached(HosSpectrum *self, guint *idx, gdouble value)
{
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);
  spectrum_determine_instantiable(self, priv);

  if (priv->instantiable)
    {
      /* collapse idx */
      gint stride = 1;
      gsize linear_idx = 0;
      GList* dimensions;

      for (dimensions = priv->dimensions; dimensions != NULL; dimensions = dimensions->next)
	{
	  linear_idx += stride * *idx;
	  stride *= ((dimension_t*)(dimensions->data))->np;
	  ++idx;
	}

      point_cache_store(self, linear_idx, value);
    }
}

/*********** The point cache ****************/

static gsize default_point_cache_size = 1024 * 1024 * 8;
static gsize point_cache_size = 0;
static gboolean point_cache_enable = TRUE;

static guint point_cache_hit_count       = 0;
static guint point_cache_miss_count      = 0;
static guint point_cache_collision_count = 0;

struct _point_cache_slot
{
  HosSpectrum *spec;
  gsize        idx;
  gint         version;
  gdouble      value;
};

static struct _point_cache_slot *point_cache = NULL;

/*
 * Hash 'spec' and 'idx' to return a cache slot index;
 * return value must be in range [0..point_cache_size)
 */
static gsize
point_cache_hash(HosSpectrum *spec, gsize idx)
{
  if (point_cache_size > 0)
    return ((gsize)spec + idx) % point_cache_size;
  else
    return 0;
}

static void
point_cache_store(HosSpectrum *spec, gsize idx, gdouble value)
{
  if (point_cache_enable)
    {
      if (point_cache == NULL)
	{
	  const gchar *point_cache_str = g_getenv("POINT_CACHE_SIZE");
	  gchar *err = NULL;
	  point_cache_size = default_point_cache_size;
	  if (point_cache_str != NULL)
	    {
	      point_cache_size = (gint)(g_ascii_strtod(point_cache_str, &err)) * 1024 * 1024;
	      if (*err != '\0')
		point_cache_size = default_point_cache_size;
	    }
	  
	  if (point_cache_size > 0)
	    point_cache = g_new0(struct _point_cache_slot, point_cache_size);
	  else
	    point_cache_enable = FALSE;
	}
    }

  if (point_cache_enable)
    {
      struct _point_cache_slot *slot = point_cache + point_cache_hash(spec, idx);
      
      gint old_version = g_atomic_pointer_get(&(slot->version));
      slot->spec = NULL;
      g_atomic_int_set(&(slot->version), old_version + 1);
      slot->idx   = idx;
      slot->value = value;
      g_atomic_pointer_set(&(slot->spec), spec);
    }
}

/*
 * return spec[idx] from the point cache, or DATUM_UNKNOWN_VALUE
 */
static gdouble
point_cache_fetch(HosSpectrum *spec, gsize idx)
{
  if ((point_cache != NULL) && (point_cache_enable))
    {
       struct _point_cache_slot *slot = point_cache + point_cache_hash(spec, idx);

       gint         version_start  = g_atomic_int_get(&(slot->version));
       HosSpectrum *slot_spec      = g_atomic_pointer_get(&(slot->spec));
       gsize        slot_idx       = slot->idx;
       gdouble      slot_value     = slot->value;
       gint         version_finish = g_atomic_int_get(&(slot->version));

       if (version_start != version_finish)
	 point_cache_collision_count++;
       if ((version_start == version_finish) && (slot_spec == spec) && (slot_idx == idx))
	 {
	   DATUM_ENSURE_KNOWN(slot_value);
	   point_cache_hit_count++;
	   return slot_value;
	 }
       else
	 point_cache_miss_count++;
    }
  return DATUM_UNKNOWN_VALUE;
}

/****** iterators *******/

struct spectrum_iterator*
spectrum_construct_iterator(HosSpectrum *self)
{
  struct spectrum_iterator* result = NULL;

  if (SPECTRUM_PRIVATE(self, valid) == TRUE)
    result = spectrum_iterator_cached(self);
  else
    {
      HosSpectrumClass *class = HOS_SPECTRUM_GET_CLASS(self);
      g_assert(class->construct_iterator != NULL);
      result = class->construct_iterator(self);
      result->free = class->free_iterator;
    }

  result->root      = self;
  result->root_type = G_OBJECT_TYPE(self);
  result->ndim      = spectrum_ndim(self);
  result->idx       = g_new0(guint, spectrum_ndim(self));
  result->save_idx  = g_new0(guint, spectrum_ndim(self));
  result->np        = g_new0(gsize, spectrum_ndim(self));
  result->stride    = g_new0(gsize, spectrum_ndim(self));
  result->can_cache = TRUE;  /* innocent until proven guilty */

  gint i;

  if (spectrum_ndim(self) > 0)
    {
      result->stride[0] = 1;
      for (i = 1; i < spectrum_ndim(self); ++i)
	{
	  gsize np   = spectrum_np(self, i - 1);
	  gsize last = result->stride[i - 1];
	  if ((G_MAXSIZE / last) <= np)
	    result->can_cache = FALSE;
	  result->stride[i] = last * np;
	}
    }

  for (i = 0; i < spectrum_ndim(self); ++i)
    result->np[i] = spectrum_np(self, i);

  return result;
}

void
iterator_increment(struct spectrum_iterator *self, guint dim, gint delta)
{
  CONFESS_FULL(2, "Iterator 0x%x: incrementing dim %d by %d (linear_idx = %d)", self, dim, delta, self->idx_linear);

  self->idx[dim] += delta;

  if (self->can_cache == TRUE)
    self->idx_linear += delta * self->stride[dim];
  if (self->increment)
    (self->increment)(self, dim, delta);
}

/*
 * Increment 'self' in 'traversal order'.
 * Returns: TRUE:  traversal is complete
 *          FALSE: points remain to traverse
 */
static gboolean
iterator_bump(struct spectrum_iterator *self)
{
  gint dim = 0;
  while (1)
    {
      if (dim >= self->ndim)
	return TRUE;
      if (self->idx[dim] < self->np[dim] - 1)
	{
	  iterator_increment(self, dim, 1);
	  return FALSE;
	}
      else
	{
	  iterator_increment(self, dim, -self->idx[dim]);
	  ++dim;
	}
    }
}

void
iterator_mark(struct spectrum_iterator *self)
{
  self->save_idx_linear = self->idx_linear;
  gint i;
  for (i = 0; i < self->ndim; ++i)
    self->save_idx[i] = self->idx[i];
  if (self->mark)
    (self->mark)(self);
  CONFESS("Iterator 0x%x: marked at linear_idx %d", self, self->idx_linear);
}

/*
 * Returns:
 * TRUE:  iterator_wait() will not block
 * FALSE: iterator_wait() will block
 */
gboolean
iterator_probe(struct spectrum_iterator *self)
{
  return (self->probe) ? (self->probe)(self) : FALSE;
}

static gboolean
iterator_check_cache(struct spectrum_iterator *self, gdouble *dest)
{
  if (self->can_cache == TRUE)
    {
      if (SPECTRUM_PRIVATE(self->root, valid) == TRUE)
	{
	  g_assert(self->root->buf != NULL);
	  *dest = self->root->buf[self->idx_linear];
	  return TRUE;
	}
      else
	{
	  gdouble cached_value = point_cache_fetch(self->root, self->idx_linear);
	  if (DATUM_IS_KNOWN(cached_value))
	    {
	      *dest = cached_value;
	      return TRUE;
	    }
	}
    }
    return FALSE;
}

gboolean
iterator_tickle(struct spectrum_iterator *self, gdouble *dest)
{
  gboolean is_instantiated = iterator_check_cache(self, dest);
  if (is_instantiated == FALSE)
    {
      is_instantiated = (self->tickle)(self, dest);
      if (is_instantiated && self->can_cache)
	point_cache_store(self->root, self->idx_linear, *dest);
    }

  return is_instantiated;
}

/*
 * restore pointer location to the one saved using iterator_mark()
 */
void
iterator_restore(struct spectrum_iterator *self)
{
  self->idx_linear = self->save_idx_linear;
  gint i;
  for (i = 0; i < self->ndim; ++i)
    self->idx[i] = self->save_idx[i];
  if (self->restore) (self->restore)(self);
}

gdouble
iterator_wait(struct spectrum_iterator *self)
{
  CONFESS("Iterator 0x%x: waiting at linear_idx %d", self, self->idx_linear);
  gdouble result;
  if (iterator_check_cache(self, &result) == FALSE)
    {
      result = (self->wait)(self);
      if (self->can_cache)
	point_cache_store(self->root, self->idx_linear, result);
    }
  return result;
}

void
iterator_free(struct spectrum_iterator *self)
{
  HosSpectrumClass *class = HOS_SPECTRUM_GET_CLASS(self->root);

  g_free(self->idx);
  g_free(self->save_idx);
  g_free(self->np);
  g_free(self->stride);

  if (self->free)
    (self->free)(self);

}

/**** 'cached' iterators ****/

static struct spectrum_iterator*
spectrum_iterator_cached(HosSpectrum *self)
{
  g_return_if_fail(HOS_IS_SPECTRUM(self));
  g_assert(self->buf != NULL);
  struct spectrum_iterator *result           = g_new0(struct spectrum_iterator, 1);

  /* FIXME insert entries for result->tickle, etc. here */

  return result;
}

void
spectrum_set_dimensions(HosSpectrum* self, GList *dimensions)
{
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);
  priv->dimensions = dimensions;
}

GList*
spectrum_copy_dimensions(HosSpectrum *self)
{
  GList* result = NULL;
  GList* dimen;

  for (dimen = SPECTRUM_PRIVATE(self, dimensions); dimen != NULL; dimen = dimen->next)
    {
      dimension_t* dest = g_new0(dimension_t, 1);
      dimension_t* src  = (dimension_t*)(dimen->data);

      dest->np   = src->np;
      dest->sw   = src->sw;
      dest->sf   = src->sf;
      dest->orig = src->orig;

      result = g_list_append(result, dest);
    }

  return result;
}

static void
check_dim_count(HosSpectrum* spec, const guint dim)
{
  g_assert(HOS_IS_SPECTRUM(spec));
  g_assert(dim < g_list_length(SPECTRUM_PRIVATE(spec, dimensions)));
}

static dimension_t*
spectrum_fetch_dimension(HosSpectrum* self, const guint dim)
{
  return (dimension_t*)(g_list_nth_data(SPECTRUM_PRIVATE(self, dimensions), dim));
}



