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
#include <burrow/spectrum.h>
#include "hosdimension.h"
#include "hosdimensionblock.h"
#include "ticket.h"
#include "hosbackingcache.h"

/*
 * A general-purpose structure that various callbacks use
 * during traversal with g_list_foreach.  This is an experiment
 * with defining one such structure for the whole file, rather
 * than a bunch of little structures appropriate to each
 * traversal.
 */
struct _foreach_data
{
  GList *target_1;
  GList *target_2;

  gdouble cost;
  gint cost_mode;

  gdouble *potato;

  HosBacking *backing;

  HosBacking *original_backing;
  HosBacking *new_backing;

  gdouble orig;
  gdouble giro;

};

enum {
  READY,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_READY
};

#define HOS_SPECTRUM_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM, HosSpectrumPrivate))
#define SPECTRUM_PRIVATE(o, field) ((HOS_SPECTRUM_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumPrivate HosSpectrumPrivate;

struct _HosSpectrumPrivate
{
  GMutex *traversal_lock;
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

static void          set_buffer_stride          (GList*, guint *stride);
static gint          compare_cost               (GList *A, GList *B);
static void          g_list_foreach_recursive   (GList *list,
					         guint lvl, GFunc callback, gpointer data);
static void          check_replace_backing      (HosDimension *dimen, struct _foreach_data* data);
static void          replace_backing            (HosDimension *dimen, struct _foreach_data* data);
static void          g_object_ref_data          (GObject *obj, gpointer data);
static HosSpectrum*  spectrum_copy              (HosSpectrum *src);
static gdouble*      dimension_traverse_internal(GList*, gdouble*, GList*);
static void          dimension_extract_cb_ppm   (HosDimension* dimen, struct _foreach_data* data);
static void          dimension_extract_cb       (HosDimension* dimen, struct _foreach_data* data);
static void          spectrum_invalidate_cache  (HosSpectrum *self);
static void          spectrum_traverse_internal (HosSpectrum* spec);
static gboolean      spectrum_signal_ready      (HosSpectrum* self);
static guint         dimen_list_lookup_nth      (GList* list, guint n);
static GList*        dimen_list_get_nth         (GList* dimens, guint idx);
static HosDimension* dimen_list_get_nth_first   (GList* dimens, guint idx);
static gpointer      g_list_nth_first           (GList* list, guint n);
static gboolean      spectrum_is_ready          (HosSpectrum *self);


static void          ensure_traversal_setup     (void);
static void          queue_pending_push         (HosSpectrum* spectrum);
static void          queue_ready_push           (HosSpectrum* spectrum);
static HosSpectrum*  queue_pending_fetch        (void);
static HosSpectrum*  queue_ready_fetch          (void);
static void          signal_spectra_ready       (void);
static gboolean      idle_spectra_ready         (gpointer not_used);
static gpointer      traversal_thread_func      (gpointer not_used);

G_DEFINE_TYPE (HosSpectrum, hos_spectrum, G_TYPE_OBJECT)

/*
 * Add this dimension's backing to this list,
 * if the list does not already contain the backing.
 */
static void
append_backing(HosDimension* dimen, GList** list)
{
  if (!g_list_find(*list, dimen->backing))
    *list = g_list_append(*list, dimen->backing);
}

gsize
spectrum_ndim(HosSpectrum *spec)
{
  GList* dimens = spec->dimensions;
  guint result = 0;

  for (dimens = spec->dimensions; dimens != NULL; dimens = dimens->next)
    {
      HosDimension *dimen = HOS_DIMENSION(g_list_nth_first(dimens, 0));
      if (!dimen->integrated)
	++result;
    }
  return result;
}

gdouble*
spectrum_traverse_blocking(HosSpectrum *spec)
{
  spectrum_traverse_internal(spec);
  return spec->buf;
}

/*
 * If the spectrum contents are ready, immediately returns
 * the buffer containing the contents.
 * If the contents are not ready, returns NULL, but when the
 * contents are ready, the spectrum will emit the 'ready' signal.
 */
gdouble*
spectrum_traverse(HosSpectrum *spec)
{
  if (spec->buf == NULL)
    queue_pending_push(spec);
  return (spec->buf);
}

static void
spectrum_traverse_internal(HosSpectrum* self)
{
  g_return_if_fail(HOS_IS_SPECTRUM(self));

  g_mutex_lock(SPECTRUM_PRIVATE(self, traversal_lock));

  if (self->buf != NULL)
    return;

  guint spectrum_size = 1;    /* total size in number of points */
  GList* backing_list = NULL;
  gdouble *buffer = NULL;
  HosSpectrum* spec = spectrum_copy(self);

  g_list_foreach(spec->dimensions, (GFunc)set_buffer_stride, &spectrum_size);

  /* quit if spectrum is empty. */
  if (spectrum_size == 0)
    {
      g_object_unref(spec);
      return;
    }

  buffer = g_renew(gdouble, buffer, spectrum_size);
  assert(buffer);
  spec->buf = buffer;
  memset(buffer, 0, spectrum_size * sizeof(gdouble));

  g_list_foreach_recursive(spec->projections, 2, (GFunc)append_backing, &backing_list);
  g_list_foreach_recursive(spec->dimensions, 2, (GFunc)append_backing, &backing_list);
  g_list_foreach(backing_list, (GFunc)backing_reset, NULL);

  spec->dimensions = g_list_sort(spec->dimensions, (GCompareFunc)compare_cost);

  /*
   * Perform the actual traversal--
   * reset all dimensions,
   * then call recursive traverser function.
   */
  g_list_foreach_recursive(spec->projections, 2, (GFunc)dimension_prime, NULL);
  g_list_foreach_recursive(spec->dimensions, 2, (GFunc)dimension_prime, NULL);
  dimension_traverse_internal(spec->dimensions, spec->buf, backing_list);

  self->buf = spec->buf;
  spec->buf = 0;

  g_object_unref(spec);

  g_mutex_unlock(SPECTRUM_PRIVATE(self, traversal_lock));

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

static guint
spectrum_total_points(HosSpectrum *spec)
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
  gdouble* buf;
  int spec_size;
  gdouble result;

  buf = spectrum_traverse_blocking(spec);
  spec_size = spectrum_total_points(spec);

  assert(spec_size > n);

  qsort(buf, spec_size, sizeof(gdouble), (sortfunc)compare_gdoubles);
  result = buf[n];
  spectrum_invalidate_cache(spec);
  return result;

}

gdouble
spectrum_mean(HosSpectrum *spec)
{
  gdouble* buf = spectrum_traverse_blocking(spec);
  int spec_size = spectrum_total_points(spec);
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
  int spec_size = spectrum_total_points(spec);
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
  int spec_size = spectrum_total_points(spec);

  /* FIXME fail more gracefully */
  assert(idx < spec_size);

  return buf[idx];

}

gdouble
spectrum_get_max(HosSpectrum *spec)
{
  return spectrum_get_ranked(spec, spectrum_total_points(spec) - 1);
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
  return spectrum_get_ranked(spec, percentile * spectrum_total_points(spec));
}

/*
 * Use this as a callback in iteration over a list of
 * dimensions to ensure that the buffer stride in each
 * dimension is set properly; also, the 'stride' parameter
 * acts as an accumulator which, after the traversal,
 * contains the total number of points in the spectrum.
 *
 * Note: This function expects a 'list of dimensions';
 * only the buffer stride in the head element of the
 * list will be set.  So don't sort the list after calling
 * this function!
 */
static void
set_buffer_stride(GList *list, guint *stride)
{
  HosDimension *dimen = HOS_DIMENSION(g_list_nth_data(list, 0));

  if (dimen->integrated)
    dimen->buffer_stride = 0;
  else
    {
      dimen->buffer_stride = *stride;
      *stride *= dimen->np;
    }

}

/*
 * Compare the cost of dimension list A to the cost of
 * dimension list B.
 *
 * The cost of the list is the sum of the costs of its
 * members.
 *
 * note: a comparison function must return a negative
 * value if the first item comes before the second
 */
static gint
compare_cost(GList *A, GList *B)
{
  gdouble cost_A = 0;
  gdouble cost_B = 0;

  g_list_foreach(A, (GFunc)dimension_cost, &cost_A);
  g_list_foreach(B, (GFunc)dimension_cost, &cost_B);

  if (cost_A > cost_B)
    return -1;

  if (cost_A < cost_B)
    return 1;

  return 0;

}

/* typedef void (*DoubleTraverseFunc)(gpointer self, gpointer data1, gpointer data2); */

/*
 * Use this function to traverse over 'lists of lists'-- visiting each
 * data member once.
 * Use is constrained to lists where the depth is the same for every node.
 * Set 'lvl' to the depth of the tree.
 */
static void
g_list_foreach_recursive(GList *list, guint lvl, GFunc callback, gpointer data)
{
  assert(lvl > 0);

  while (list != NULL)
    {
      if (lvl > 1)
	g_list_foreach_recursive((GList*)(list->data), lvl - 1, callback, data);
      else
	callback(list->data, data);

      list = list->next;
    }
}



/*
 * retrieve the backing object from 'dimen'; ensure that it
 * does not occur in either target_1 or target_2.
 */
static void
check_replace_backing(HosDimension *dimen, struct _foreach_data* data)
{
  struct _foreach_data child_data;

  child_data.original_backing = dimen->backing;
  child_data.new_backing = NULL;

  g_list_foreach_recursive(data->target_1, 2, (GFunc)replace_backing, &child_data);
  g_list_foreach_recursive(data->target_2, 2, (GFunc)replace_backing, &child_data);

}

/*
 * Used as a callback in double traversal.
 * If this dimension's backing matches the one we are trying to replace,
 * then replace it with the new one.
 * If the new one hasn't been created yet, first create it by making
 * a copy of the old one.
 */
static void
replace_backing(HosDimension *dimen, struct _foreach_data* data)
{
  if (dimen->backing != data->original_backing)
    return;

  if (data->new_backing == NULL)
   data->new_backing = backing_copy(data->original_backing);

  dimen->backing = data->new_backing;

}

/*
 * Returns: the convolution of spectrum A and spectrum B.
 *
 * Definition:  let C <- spectrum_convolute(A, B);
 *   then  C(a', b', a, b) = A(a', b') * B(a, b)
 *
 * Care must be taken to preserve referential transparency
 * of spectra A, B, and C; to achieve this, the dimensions
 * of spectrum B are copied.  If any backing object in
 * spectrum A appears in spectrum B, that backing object is
 * replaced with a copy in spectrum B.
 */
HosSpectrum*
spectrum_convolute(HosSpectrum *spec_A, HosSpectrum *spec_B)
{
  HosSpectrum *result = spectrum_copy(spec_A);
  GList *B_projections = g_list_copy(spec_B->projections);
  GList *B_dimensions = g_list_copy(spec_B->dimensions);
  GList *list_iter;
  guint dims_a = spectrum_ndim(spec_A);
  guint dims_b = spectrum_ndim(spec_B);

  /* replace B dimensions with copies. */
  for (list_iter = B_projections; list_iter != NULL; list_iter = list_iter->next)
    list_iter->data = dimen_list_copy((GList*)(list_iter->data));

  for (list_iter = B_dimensions; list_iter != NULL; list_iter = list_iter->next)
    list_iter->data = dimen_list_copy((GList*)(list_iter->data));

  /* ensure no backing objects are shared between A and B. */
  {

    struct _foreach_data data;

    data.target_1 = B_projections;
    data.target_2 = B_dimensions;

    g_list_foreach_recursive(spec_A->projections, 2,
			     (GFunc)check_replace_backing,
			     &data);

    g_list_foreach_recursive(spec_A->dimensions, 2,
			     (GFunc)check_replace_backing,
			     &data);

  }

  /* tack B dimensions onto the end of A dimensions */
  result->projections = g_list_concat(result->projections, B_projections);
  result->dimensions = g_list_concat(result->dimensions, B_dimensions);

  result->negated = spec_A->negated && spec_B->negated;

  assert(spectrum_ndim(result) == dims_a + dims_b);

  return result;

}

/*
 * g_object_ref which can be used as a callback in list traversal.
 */
static void
g_object_ref_data(GObject *obj, gpointer data)
{
  g_object_ref(obj);
}

/*
 * Performs a shallow copy of a spectrum;
 * the object itself as well as the underlying projection
 * and dimension lists are copied.
 * However, the dimensions themselves, and their underlying
 * backing objects, are not copied.
 */
static HosSpectrum*
spectrum_copy(HosSpectrum *src)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM, NULL);

  result->negated = src->negated;
  result->buf = NULL;
  result->projections = g_list_copy(src->projections);
  result->dimensions = g_list_copy(src->dimensions);

  /*
   * FIXME is it appropriate to reference all dimensions here?
   * I think so...
   */

  g_list_foreach_recursive(result->projections, 2, (GFunc)g_object_ref_data, NULL);
  g_list_foreach_recursive(result->dimensions, 2, (GFunc)g_object_ref_data, NULL);

  return result;

}

/*
 * Destroy spectrum's cached contents, forcing
 * subsequent traversals to access the underlying data source.
 */
static void
spectrum_invalidate_cache(HosSpectrum *self)
{
  if (self->buf != NULL)
    g_free(self->buf);
  self->buf = NULL;
  self->status = LATENT;
}

static GList*
dimen_list_get_nth(GList* dimens, guint idx)
{
  return
    g_list_nth_data(dimens, dimen_list_lookup_nth(dimens, idx));
}

static HosDimension*
dimen_list_get_nth_first(GList* dimens, guint idx)
{
  return HOS_DIMENSION(g_list_nth_data(dimen_list_get_nth(dimens, idx), 0));
}

/*
 * Return a specific member of the 'list of lists'; 
 */
static HosDimension*
dimen_list_get_raw(GList* dimens, guint idx_0, guint idx_1)
{
  GList* list_1 = (GList*)(g_list_nth_data(dimens, idx_0));
  return HOS_DIMENSION(g_list_nth_data(list_1, idx_1));
}

/*
 * returns the index in list of the nth non-integrated
 * dimension in the list
 */
static guint
dimen_list_lookup_nth(GList* list, guint n)
{
  guint result = 0;
  guint sanity = 0;

  while (1)
    {
      /* skip integrated dims */
      while (1)
	{
	  assert(list != NULL);
	  {
	    HosDimension* dimen = HOS_DIMENSION(((GList*)(list->data))->data);
	    if (!(dimen->integrated))
	      break;
	    ++result;
	    list = list->next;
	  }
	}
      if (n == 0)
	return result;
      else
	{
	  --n;
	  list = list->next;
	  ++result;
	}
      assert(++sanity < 1000);
    }
}

/*
 * The first active dimension is simply not traversed;
 * this corresponds to retaining only the first point
 * of the first dimension.
 *
 * If you want some other slice, first set the lower
 * bound of this dimension.
 */
HosSpectrum*
spectrum_project(HosSpectrum *self)
{
  HosSpectrum *result;
  if (!HOS_IS_SPECTRUM(self)) return NULL;
  result = spectrum_copy(self);

  {
    guint idx = dimen_list_lookup_nth(result->dimensions, 0);
  
    result->projections = g_list_append(result->projections,
					g_list_nth_data(result->dimensions, idx));

    result->dimensions = g_list_delete_link(result->dimensions,
					    g_list_nth(result->dimensions, idx));
  }

  return result;

}

HosSpectrum*
spectrum_transpose(HosSpectrum *self, const guint idx)
{
  HosSpectrum *result;
  if (!HOS_IS_SPECTRUM(self)) return NULL;
  result = spectrum_copy(self);

  {
    GList *new_headliner = dimen_list_get_nth(result->dimensions, idx);
 
    result->dimensions = g_list_remove(result->dimensions, new_headliner);
    result->dimensions = g_list_prepend(result->dimensions, new_headliner);

    assert(g_list_length(self->dimensions) == g_list_length(result->dimensions));
  }

  return result;

}

/*
 * Note: Unlike other spectrum operators, this one is destructive--
 * the argument spectrum is mutated.
 * There should only be a need to 'unfold' a dimension once.  It's
 * a property of the spectrum itself, not a property of a particular
 * view of a spectrum.
 *
 * !! Call this just after creating a block spectrum; do not
 * call on spectra that are the result of manipulations like projection
 * and extraction.
 */
void
spectrum_unfold(HosSpectrum* self,
		const guint idx,
		const guint downfield,
		const guint upfield,
		const gboolean negate_on_fold)
{
  HosDimension *dimen = dimen_list_get_nth_first(self->dimensions, idx);

  g_return_if_fail(HOS_IS_DIMENSION_BLOCK(dimen));

  dimension_block_unfold(HOS_DIMENSION_BLOCK(dimen), downfield, upfield, negate_on_fold);

}

static void
check_dim_count(HosSpectrum* spec, const guint dim)
{
  assert(HOS_IS_SPECTRUM(spec));
  assert(dim < g_list_length(spec->dimensions));
}

/*
 * This little wrapper is needed for all the metadata selectors.
 * It gets the 'first element of the nth element' of its argument.
 */
static gpointer
g_list_nth_first(GList* list, guint n)
{
  return g_list_nth_data(g_list_nth_data(list, n), 0);
}

gsize
spectrum_np(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  /* FIXME
   * to handle integrated dimensions...
   * define new selector (instead of g_list_nth_first) to skip
   * dimensions that aren't integrated??
   */
  return dimension_np(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)));
}

gdouble
spectrum_sw(HosSpectrum* spec, const guint dim)
{
  g_return_val_if_fail(spec, 0);
  check_dim_count(spec, dim);
  return dimension_sw(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)));
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
  check_dim_count(spec, dim);
  return dimension_sf(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)));
}

gdouble
spectrum_orig(HosSpectrum* spec, const guint dim)
{
  check_dim_count(spec, dim);
  return dimension_orig(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)));
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
  return dimension_ppm2pt(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)),
			  ppm);
}

gdouble
spectrum_pt2ppm(HosSpectrum* spec, guint dim, gdouble pt)
{
  return dimension_pt2ppm(HOS_DIMENSION(dimen_list_get_nth_first(spec->dimensions, dim)),
			  pt);

}

/*
 * The first dimension is clipped to the requested limits,
 * _in Hz_.
 */
HosSpectrum*
spectrum_extract(HosSpectrum* self, const gdouble A, const gdouble B)
{

  HosSpectrum *result = spectrum_copy(self);
  guint idx = dimen_list_lookup_nth(result->dimensions, 0);
  GList* dimen_list_0 = (GList*)dimen_list_get_nth(result->dimensions, 0);
  GList* new_list = dimen_list_copy(dimen_list_0);

  {

    struct _foreach_data data;

    data.orig = A;
    data.giro = B;

    g_list_foreach(new_list, (GFunc)dimension_extract_cb, &data);

  }

  /* FIXME unref old dimensions? */
  (g_list_nth(result->dimensions, idx))->data = new_list;

  return result;
}

static void
dimension_clip_lower_cb(HosDimension* self, gpointer data)
{
  dimension_clip_lower(self, GPOINTER_TO_UINT(data));
}

/*
 * The first dimension is projected at the requested point index.
 */
HosSpectrum*
spectrum_project_pt(HosSpectrum* self, const guint pt)
{
  HosSpectrum *result = spectrum_copy(self);
  HosSpectrum *old_ref = NULL;
  guint idx = dimen_list_lookup_nth(result->dimensions, 0);

  GList* list_orig = dimen_list_get_nth(result->dimensions, 0);
  GList* dimen_list_0 = dimen_list_copy((GList*)list_orig);

  g_list_foreach(dimen_list_0, (GFunc)dimension_clip_lower_cb, GUINT_TO_POINTER(pt));
  /* FIXME unref old dimensions? */
  (g_list_nth(result->dimensions, idx))->data = dimen_list_0;
  /* (g_list_nth(result->dimensions, 0))->data = dimen_list_0; */

  old_ref = result;
  result = spectrum_project(result);
  g_object_unref(G_OBJECT(old_ref));

  return result;
}

/*
 * Set the integration flag on the first dimension.
 */
HosSpectrum*
spectrum_integrate(HosSpectrum* self)
{
  HosSpectrum *result = spectrum_cache(spectrum_copy(self));
  guint idx = dimen_list_lookup_nth(result->dimensions, 0);

  GList* list_orig = dimen_list_get_nth(result->dimensions, 0);
  GList* dimen_list_0 = dimen_list_copy((GList*)list_orig);

  (HOS_DIMENSION(g_list_nth_data(dimen_list_0, 0)))->integrated = TRUE;
  /* FIXME unref old dimensions? */
  (g_list_nth(result->dimensions, idx))->data = dimen_list_0;

  return result;
}

HosSpectrum*
spectrum_project_ppm(HosSpectrum* self, const gdouble ppm)
{
  return spectrum_project_pt(self,
			     spectrum_ppm2pt(self, 0, ppm));
}

HosSpectrum*
spectrum_extract_ppm(HosSpectrum* self, const gdouble A, const gdouble B)
{
  HosDimension* dimen = HOS_DIMENSION(dimen_list_get_nth_first(self->dimensions, 0));
  gdouble sf = dimen->sf;

  return spectrum_extract(self, A * sf, B * sf);

}

static void
dimension_extract_cb(HosDimension* dimen, struct _foreach_data* data)
{
  dimension_extract(dimen, data->orig, data->giro);
}

static void
dimension_extract_cb_ppm(HosDimension* dimen, struct _foreach_data* data)
{
  dimension_extract_ppm(dimen, data->orig, data->giro);
}


/*
 * First two dimensions are combined (synchronized),
 * with a sweep width equal to the intersection of the
 * sweeps of the two dimensions.
 * The 1th (not the 0th) dimension controls the
 * digital resolution.
 * Can be thought of as: the 0th dimension is 'projected
 * away', leaving the 1th dimension unscathed.
 */
HosSpectrum*
spectrum_diagonal_project(HosSpectrum* self)
{
  HosSpectrum *result = spectrum_copy(self);
  guint idx_0 = dimen_list_lookup_nth(result->dimensions, 0);
  guint idx_1 = dimen_list_lookup_nth(result->dimensions, 1);

  GList* dimen_list_0 = dimen_list_copy((GList*)g_list_nth_data(result->dimensions, idx_0));
  GList* dimen_list_1 = dimen_list_copy((GList*)g_list_nth_data(result->dimensions, idx_1));

  /*
    #define MIN(a, b) (a) > (b) ? (b) : (a)
    #define MAX(a, b) (a) > (b) ? (a) : (b)
  */

  /* FIXME comparison should be based on ppm, not hz! */

  gdouble orig_new = MIN(spectrum_orig_ppm(result, 0),
			 spectrum_orig_ppm(result, 1));
  gdouble giro_new = MAX(spectrum_giro_ppm(result, 0),
			 spectrum_giro_ppm(result, 1));


  /* 
   * sanity checks- ranges of interested dimensions must overlap
   */
  g_return_val_if_fail(orig_new > giro_new, NULL);

  {

    struct _foreach_data data;

    data.orig = orig_new;
    data.giro = giro_new;

    g_list_foreach(dimen_list_0, (GFunc)dimension_extract_cb_ppm, &data);
    g_list_foreach(dimen_list_1, (GFunc)dimension_extract_cb_ppm, &data);

  }

  g_list_foreach(dimen_list_0,
		 (GFunc)dimension_interpolate,
		 GUINT_TO_POINTER(dimension_np(HOS_DIMENSION(g_list_nth_data(dimen_list_1, 0)))));

  /* Cut out the old dimensions, add the new synchronized ones */
  dimen_list_0 = g_list_concat(dimen_list_0, dimen_list_1);

  /* FIXME unref old dimensions? */
  result->dimensions = g_list_delete_link(result->dimensions,
					  g_list_nth(result->dimensions, idx_0));
  if (idx_1 > idx_0)
    --idx_1;
  result->dimensions = g_list_delete_link(result->dimensions,
					  g_list_nth(result->dimensions, idx_1));

  result->dimensions = g_list_prepend(result->dimensions, dimen_list_0);

  return result;

}

HosSpectrum*
spectrum_cache(HosSpectrum* self)
{
  int i;
  int cumulative_stride = 1;

  /* create new spectrum; point its buf to old buf */
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM, NULL);
  result->buf = NULL;

  /* create backing_cache object to store data */
  HosBackingCache *backing_cache = g_object_new(HOS_TYPE_BACKING_CACHE, NULL);

  /* traverse src spectrum */
  spectrum_traverse(self);

  /* set backing's data to spec's buf */
  backing_cache->data = self->buf;

  /* the new backing object holds a reference to the old spectrum */
  g_object_ref(self);

  /* create block dims and tie to backing_cache */
  for (i = 0; i < spectrum_ndim(self); ++i)
    {

      HosDimensionBlock* dimen_block = g_object_new(HOS_TYPE_DIMENSION_BLOCK, NULL);
      HosDimension* dimen = HOS_DIMENSION(dimen_block);

      dimen->backing = HOS_BACKING(backing_cache);
      dimen_block->schedule = NULL;
      dimen_block->np_physical = spectrum_np(self, i);
      dimen_block->sw_physical = spectrum_sw(self, i);

      dimen->sw = spectrum_sw(self, i);
      dimen->sf = spectrum_sf(self, i);
      dimen->np = spectrum_np(self, i);
      dimen->orig = spectrum_orig(self, i);

      dimen_block->initial_offset = 0;
      dimen_block->fold_allowed = TRUE;
      dimen_block->negated_initially = FALSE;
      dimen_block->negate_on_fold = FALSE;

      dimen_block->stride = cumulative_stride;
      cumulative_stride *= spectrum_np(self, i);

      /* note: dimensions are a 'list of lists' */
      result->dimensions = g_list_append(result->dimensions,
					 g_list_append(NULL, dimen_block));
    }

  return result;
}

/*
 * internal traverser; must return a pointer into the next
 * available buffer slot.
 * list is a 'list of lists of dimensions'
 * backing_list is a list of backing objects
 *
 * FIXME: handle integrated dimensions!
 *
 */
static gdouble*
dimension_traverse_internal(GList *list, gdouble *buf, GList *backing_list)
{
  GList *dimen_list;
  HosDimension *dimen;
  gboolean done;

  if (list == NULL)
    {
	  gdouble sum = 1.0;
	  g_list_foreach(backing_list, (GFunc)backing_accumulate, &sum);
	  *buf += sum;
	  return buf;
    }
  else
    {
      dimen_list = (GList*)list->data;
      dimen = (HosDimension*)g_list_nth_data(dimen_list, 0);
      done = FALSE;
      
      while (1)
	{
	  if (list->next == NULL)
	    {
	      gdouble sum = 1.0;
	      g_list_foreach(backing_list, (GFunc)backing_accumulate, &sum);
	      *buf += sum;
	    }
	  else
	    dimension_traverse_internal(list->next, buf, backing_list);
	  
	  g_list_foreach(dimen_list, (GFunc)dimension_increment, &done);
	  
	  if (done == TRUE)
	    {
	      g_list_foreach(dimen_list, (GFunc)dimension_reset, NULL);
	      return buf;
	    }
	  buf += dimen->buffer_stride;
	}
    }

  /* UNREACHABLE */
  return NULL;

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
  spectrum->status = LATENT;
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

/******* the spectrum queue   *****/

static GList*   spectra_pending        = NULL;
static GList*   spectra_ready          = NULL;
static GMutex*  spectrum_queue_lock    = NULL;
static GThread* traversal_thread       = NULL;
static GError*  traversal_thread_error = NULL;
static GCond*   spectrum_pending_cond  = NULL;

static void
ensure_traversal_setup()
{
  if (!g_thread_supported ()) g_thread_init (NULL);
  if (spectrum_queue_lock == NULL)
    spectrum_queue_lock = g_mutex_new();
  if (traversal_thread == NULL)
    {
      traversal_thread =
	g_thread_create((GThreadFunc)traversal_thread_func,
			NULL,
			FALSE,
			&traversal_thread_error);
      g_assert(traversal_thread != NULL);
    }
  if (spectrum_pending_cond == NULL)
    spectrum_pending_cond = g_cond_new();
  g_assert(traversal_thread_error == NULL);
}

static void
queue_pending_push(HosSpectrum* spectrum)
{
  ensure_traversal_setup();
  g_mutex_lock(spectrum_queue_lock);

  /* push the argument to the head of the list. */
  GList* prev = g_list_find(spectra_pending, (gpointer)spectrum);
  if (prev)
    spectra_pending = g_list_delete_link(spectra_pending, prev);
  spectra_pending = g_list_prepend(spectra_pending, spectrum);
  g_cond_signal(spectrum_pending_cond);

  g_mutex_unlock(spectrum_queue_lock);
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
 * returns: next 'pending' spectrum--
 * will block until one is available
 */
static HosSpectrum*
queue_pending_fetch(void)
{
  ensure_traversal_setup();
  g_mutex_lock(spectrum_queue_lock);
  while (g_list_length(spectra_pending) == 0)
    g_cond_wait(spectrum_pending_cond, spectrum_queue_lock);

  g_assert(g_list_length(spectra_pending) != 0);
  HosSpectrum* result = (HosSpectrum*)g_list_nth_data(spectra_pending, 0);
  spectra_pending = g_list_delete_link(spectra_pending, g_list_first(spectra_pending));

  g_mutex_unlock(spectrum_queue_lock);

  g_assert(result != NULL);
  return result;
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


/******* the traversal thread *****/

/*
 * signal the main thread that spectra have been traversed and are ready
 * for use.
 */
static void
signal_spectra_ready(void)
{
  g_idle_add((GSourceFunc)idle_spectra_ready, NULL);
}

static gpointer
traversal_thread_func(gpointer not_used)
{
  while (1)
    {
      HosSpectrum* next = queue_pending_fetch();
      spectrum_traverse_internal(next);
      queue_ready_push(next);
    }
}
