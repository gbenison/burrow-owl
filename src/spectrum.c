/*
 *  Copyright (C) 2005, 2006, 2008 Greg Benison
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
#include "burrow/spectrum.h"
#include "spectrum_priv.h"

/* spectrum status */
enum
{
  NO_STATUS = 0,
  LATENT,
  TRAVERSING,
  COMPLETE
};

enum {
  READY,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_READY
};

#define SPECTRUM_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM, HosSpectrumPrivate))
#define SPECTRUM_PRIVATE(o, field) ((SPECTRUM_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumPrivate HosSpectrumPrivate;

struct _HosSpectrumPrivate
{
  GMutex *traversal_lock;
  GList  *dimensions;

  guint status;
};

static guint spectrum_signals[LAST_SIGNAL] = { 0 };

static void hos_spectrum_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void hos_spectrum_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);

static void          spectrum_invalidate_cache  (HosSpectrum *self);
static void          spectrum_traverse_internal (HosSpectrum* self);
static gboolean      spectrum_signal_ready      (HosSpectrum* self);
static gboolean      spectrum_is_ready          (HosSpectrum *self);

static gboolean      spectrum_bump_idx          (HosSpectrum* self, gint* idx);

static void          ensure_traversal_setup     (void);
static void          queue_ready_push           (HosSpectrum* spectrum);
static HosSpectrum*  queue_ready_fetch          (void);
static void          signal_spectra_ready       (void);
static gboolean      idle_spectra_ready         (gpointer not_used);

static dimension_t*  spectrum_fetch_dimension   (HosSpectrum* self, const guint dim);
static gint          spectrum_collapse_idx      (HosSpectrum *self, guint* idx);

G_DEFINE_ABSTRACT_TYPE (HosSpectrum, hos_spectrum, G_TYPE_OBJECT)

gsize
spectrum_ndim(HosSpectrum *spec)
{
  return g_list_length(SPECTRUM_PRIVATE(spec, dimensions));
}

gdouble*
spectrum_traverse_blocking(HosSpectrum *spec)
{
  spectrum_traverse_internal(spec);
  return spec->buf;
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
    g_signal_emit(self, spectrum_signals[READY], 0);

}

static int
compare_gdoubles(gdouble* A, gdouble* B)
{
  if (*A == *B) return 0;
  return (*A < *B) ? -1 : 1;
}

typedef int (*sortfunc)(const void*, const void*);

gsize
spectrum_np_total(HosSpectrum *spec)
{
  int i, result = 1;
  for (i = 0; i < spectrum_ndim(spec); ++i)
    result *= spectrum_np(spec, i);

  return result;
}

/*
 * Spectral points are sorted ascendingly;
 * from that list, return point number 'n',
 * starting from zero.
 */
gdouble
spectrum_get_ranked(HosSpectrum *spec, guint n)
{
  gint np_total = spectrum_np_total(spec);
  gdouble* buf  = g_new(gdouble, np_total);

  g_assert(n < np_total);

  gdouble result;

  memcpy(buf, spectrum_traverse_blocking(spec), np_total * sizeof(gdouble));

  qsort(buf, np_total, sizeof(gdouble), (sortfunc)compare_gdoubles);
  result = buf[n];
  g_free(buf);

  return result;
}

gdouble
spectrum_mean(HosSpectrum *spec)
{
  gdouble* buf = spectrum_traverse_blocking(spec);
  int spec_size = spectrum_np_total(spec);
  gdouble result = 0;
  int i;

  for (i = 0; i < spec_size; ++i)
    result += buf[i];

  return result / spec_size;
}

gdouble
spectrum_stddev(HosSpectrum *spec)
{
  gdouble* buf = spectrum_traverse_blocking(spec);
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
 * Return the value of 'spectrum' at index 'idx'
 * where the spectrum is considered a 1D array; dimension
 * zero is the fastest-incrementing
 */
gdouble
spectrum_peek(HosSpectrum *spec, guint idx)
{
  gdouble* buf = spectrum_traverse_blocking(spec);
  int spec_size = spectrum_np_total(spec);

  g_return_if_fail(idx < spec_size);

  return buf[idx];

}

gdouble
spectrum_get_max(HosSpectrum *spec)
{
  return spectrum_get_ranked(spec, spectrum_np_total(spec) - 1);
}

gdouble
spectrum_get_min(HosSpectrum *spec)
{
  return spectrum_get_ranked(spec, 0);
}

gdouble
spectrum_get_percentile(HosSpectrum *spec, gdouble percentile)
{
  assert((percentile >= 0) && (percentile < 1.0));
  return spectrum_get_ranked(spec, percentile * spectrum_np_total(spec));
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

/*
 * Destroy spectrum's cached contents, forcing
 * subsequent traversals to access the underlying data source.
 */
static void
spectrum_invalidate_cache(HosSpectrum *self)
{
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);
  g_mutex_lock(priv->traversal_lock);
  gdouble *old_buf = self->buf;
  g_atomic_pointer_set(&self->buf, NULL);
  if (old_buf != NULL)
    g_free(old_buf);
  priv->status = LATENT;
  g_mutex_unlock(priv->traversal_lock);
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

gsize
spectrum_np(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->np;
}

gdouble
spectrum_sw(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->sw;
}

gdouble
spectrum_sw_ppm(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_sw(spec, dim) / spectrum_sf(spec, dim);
}

gdouble
spectrum_sf(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->sf;
}

gdouble
spectrum_orig(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return spectrum_fetch_dimension(spec, dim)->orig;
}

gdouble
spectrum_giro_ppm(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return spectrum_giro(spec, dim) / spectrum_sf(spec, dim);
}

gdouble
spectrum_orig_ppm(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return spectrum_orig(spec, dim) / spectrum_sf(spec, dim);
}

gdouble
spectrum_giro(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return (spectrum_orig(spec, dim) -
	  spectrum_sw(spec, dim));
}

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

gdouble
spectrum_pt2ppm(HosSpectrum* spec, guint dim, gdouble pt)
{
  dimension_t* dimen = spectrum_fetch_dimension(spec, dim);

  gdouble hz =
    dimen->orig - (dimen->sw * ((gdouble)pt / (gdouble)(dimen->np)));

  return hz / dimen->sf;
}

HosSpectrum*
spectrum_project_ppm(HosSpectrum* self, const gdouble ppm)
{
  return spectrum_project(self, spectrum_ppm2pt(self, 0, ppm));
}

HosSpectrum*
spectrum_extract_ppm(HosSpectrum* self, const gdouble A, const gdouble B)
{
  return spectrum_extract(self,
			  spectrum_ppm2pt(self, 0, A),
			  spectrum_ppm2pt(self, 0, B));
}

static void
hos_spectrum_finalize(GObject *object)
{
  HosSpectrum *spectrum = HOS_SPECTRUM(object);

  spectrum_invalidate_cache(spectrum);

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

/* here is where you would set klass->member etc. */

  spectrum_signals[READY] =
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

}

static void
hos_spectrum_init(HosSpectrum  *spectrum)
{
  SPECTRUM_PRIVATE(spectrum, status) = LATENT;
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

static gboolean
spectrum_is_ready(HosSpectrum *self)
{
  return (self->buf == NULL) ? FALSE : TRUE;
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

static GList*   spectra_ready          = NULL;
static GMutex*  spectrum_queue_lock    = NULL;
static GThreadPool* traversal_pool     = NULL;

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
      traversal_pool = g_thread_pool_new ((GFunc)spectrum_traverse_internal,
					  user_data,
					  max_threads,
					  exclusive,
					  &error);
      g_assert(error == NULL);
    }

  if (spectrum_queue_lock == NULL)
    spectrum_queue_lock = g_mutex_new();
}

static void
queue_ready_push(HosSpectrum* spectrum)
{
  ensure_traversal_setup();
  g_mutex_lock(spectrum_queue_lock);

  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));
  g_object_ref(G_OBJECT(spectrum));

  spectra_ready = g_list_prepend(spectra_ready, spectrum);

  signal_spectra_ready();

  g_mutex_unlock(spectrum_queue_lock);
}

/*
 * returns: next 'ready' spectrum, or NULL if none is ready.
 */
static HosSpectrum*
queue_ready_fetch(void)
{
  ensure_traversal_setup();
  g_mutex_lock(spectrum_queue_lock);

  HosSpectrum *result = NULL;

  if (g_list_length(spectra_ready) > 0)
    {
      result = g_list_nth_data(spectra_ready, 0);
      spectra_ready = g_list_delete_link(spectra_ready, g_list_first(spectra_ready));
    }

  g_mutex_unlock(spectrum_queue_lock);

  return result;
}

static gboolean
idle_spectra_ready(gpointer not_used)
{
  HosSpectrum *next = queue_ready_fetch();
  if (next == NULL)
    return FALSE;

  spectrum_signal_ready(next);
  g_object_unref(G_OBJECT(next));

  return TRUE;
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

/*
 * Fill the buffer of 'self'.
 */
static void
spectrum_traverse_internal(HosSpectrum* self)
{
  g_return_if_fail(HOS_IS_SPECTRUM(self));
  HosSpectrumPrivate *priv = SPECTRUM_GET_PRIVATE(self);
  if (priv->status == LATENT)
    {
      g_mutex_lock(priv->traversal_lock);
     
      if (priv->status == LATENT)
	{
	  priv->status = TRAVERSING;

	  /* create a destination buffer for the spectral data. */
	  g_assert(self->buf == NULL);
	  int total_np = spectrum_np_total(self);
	  gdouble *buf = g_new(gdouble, total_np);
	  /* 	  self->buf = g_new(gdouble, total_np); */

	  int i;
	  for (i = 0; i < total_np; ++i)
	    buf[i] = DATUM_UNKNOWN_VALUE;
	  
	  gint accumulate_idx[spectrum_ndim(self)];
	  memset(accumulate_idx, 0, spectrum_ndim(self) * sizeof(gint));
	  gdouble* accumulate_dest = buf;
	  
	  gint tickle_idx[spectrum_ndim(self)];
	  
#define ALREADY_INSTANTIATED(x) DATUM_IS_KNOWN(x)
	  
	  /* outer loop -- 'accumulate' */
	  while (1)
	    {
	      /* inner loop -- tickle remaining points */
	      if (!ALREADY_INSTANTIATED(*accumulate_dest))
		{

		  memcpy(tickle_idx, accumulate_idx, spectrum_ndim(self) * sizeof(gint));
		  gdouble* tickle_dest = accumulate_dest;
		  while (1)
		    {
		      if (!ALREADY_INSTANTIATED(*tickle_dest))
			spectrum_tickle(self, self, tickle_idx, tickle_dest);
		      ++tickle_dest;
		      if (spectrum_bump_idx(self, tickle_idx))
			break;
		    }
		}
	      
	      if (!ALREADY_INSTANTIATED(*accumulate_dest))
		*accumulate_dest = spectrum_accumulate(self, self, accumulate_idx);
	      
	      ++accumulate_dest;
	      if (spectrum_bump_idx(self, accumulate_idx))
		break;
	      
	    }

	  g_atomic_pointer_set(&self->buf, buf);
	  priv->status = COMPLETE;
	  queue_ready_push(self);
	}
      g_mutex_unlock(priv->traversal_lock);
    }
}

/*
 * Return point index 'idx' of spectrum 'self' as
 * the one-dimensional index into self->buf.
 */
static gint
spectrum_collapse_idx(HosSpectrum *self, guint* idx)
{
  gint stride = 1;
  gint result = 0;
  GList* dimensions;

  for (dimensions = SPECTRUM_PRIVATE(self, dimensions); dimensions != NULL; dimensions = dimensions->next)
    {
      result += stride * *idx;
      stride *= ((dimension_t*)(dimensions->data))->np;
      ++idx;
    }

  return result;
}

/*
 * Returns:
 *   TRUE  - point was available, '*dest' now contains value of point 'idx'; 
 *   FALSE - point not available yet; '*dest' is unchanged.
 */
gboolean
spectrum_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  /* 'idx' already instantiated? just return. */
  gdouble* buf = g_atomic_pointer_get(&self->buf);
  if (buf != NULL)
    {
      *dest = buf[spectrum_collapse_idx(self, idx)];
      return TRUE;
    }
  else
    {
      /* FIXME 'idx' cached? return the cache value. */
      
      /* Dispatch to class-specific method. */
      
      /* 
       * FIXME if it's too expensive looking up the method every time,
       * consider storing it in some transient 'traversal' object?
       */
      HosSpectrumClass *class = HOS_SPECTRUM_GET_CLASS(self);
      g_return_if_fail(class->tickle != NULL);
      gboolean result =  class->tickle(self, root, idx, dest);
      if (result) DATUM_ENSURE_KNOWN(*dest);
      return result;
    }
}

/*
 * Caches an intermediate result, so that multiple calls are guaranteed
 * to converge eventually to an answer.
 *
 * Returns:
 *   TRUE  - point was available, '*dest' now contains value of point 'idx'; 
 *   FALSE - point not available yet; '*dest' is unchanged.
 *
 */
gdouble
spectrum_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  /* 'idx' already instantiated? just return. */
  gdouble* buf = g_atomic_pointer_get(&self->buf);
  if (buf != NULL)
    return buf[spectrum_collapse_idx(self, idx)];
  else
    {
      /* FIXME 'idx' cached? return the cache value. */
      
      /* Dispatch to class-specific method. */
      
      /* 
       * FIXME if it's too expensive looking up the method every time,
       * consider storing it in some transient 'traversal' object?
       */
      HosSpectrumClass *class = HOS_SPECTRUM_GET_CLASS(self);
      g_return_if_fail(class->accumulate != NULL);
      return class->accumulate(self, root, idx);
    }
}

/*
 * Instantiate 'spec', asynchronously.
 * Returns either the spectral data or 'NULL' if not ready yet.
 * Does not block.
 * Spectrum will emit the 'ready' signal when instantiated.
 */
gdouble*
spectrum_traverse(HosSpectrum *spec)
{
  if (SPECTRUM_PRIVATE(spec, status) == COMPLETE)
    return spec->buf;
  else
    {
      ensure_traversal_setup();
      g_thread_pool_push(traversal_pool, spec, NULL);
      return NULL;
    }
}
