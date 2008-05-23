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

#include <string.h>
#include "spectrum_integrated.h"
#include "spectrum_priv.h"
#include "utils.h"

#define SPECTRUM_INTEGRATED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedPrivate))
#define SPECTRUM_INTEGRATED_PRIVATE(o, field) ((SPECTRUM_INTEGRATED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumIntegratedPrivate HosSpectrumIntegratedPrivate;

struct _HosSpectrumIntegratedPrivate
{
  HosSpectrum *integrand;
  gdouble     *accumulator;  /* cache for spectrum_integrate_accumulate */
  guint       integrand_np;  /* vector length of 'accumulator' */

  guint       *integrand_idx;
  guint       integrand_ndim;
};

struct integrated_iterator
{
  struct spectrum_iterator      parent;
  HosSpectrumIntegratedPrivate *priv;
  struct spectrum_iterator     *integrand;
};

static void     spectrum_integrated_increment (struct spectrum_iterator *self, guint dim, gint delta);
static gboolean spectrum_integrated_tickle    (struct spectrum_iterator *self, gdouble *dest);
static gdouble  spectrum_integrated_wait      (struct spectrum_iterator *self);
static void     spectrum_integrated_mark      (struct spectrum_iterator *self);

static struct spectrum_iterator* spectrum_integrated_construct_iterator (HosSpectrum *self);
static void                      spectrum_integrated_free_iterator      (struct spectrum_iterator *self);

static void   spectrum_integrated_dispose  (GObject *object);
static void   spectrum_integrated_finalize (GObject *object);
static void   initialize_integrand_idx     (HosSpectrumIntegratedPrivate *priv, guint* idx);


G_DEFINE_TYPE (HosSpectrumIntegrated, hos_spectrum_integrated, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_integrated_class_init(HosSpectrumIntegratedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_integrated_dispose;
  gobject_class->finalize    = spectrum_integrated_finalize;

  spectrum_class->construct_iterator = spectrum_integrated_construct_iterator;
  spectrum_class->free_iterator      = spectrum_integrated_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumIntegratedPrivate));
}

static void
hos_spectrum_integrated_init(HosSpectrumIntegrated* self)
{
  /* FIXME */
  /* anything? */
}

static void
spectrum_integrated_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_INTEGRATED_PRIVATE(object, integrand));
  G_OBJECT_CLASS(hos_spectrum_integrated_parent_class)->dispose (object);
}

static void
spectrum_integrated_finalize(GObject *object)
{
  HosSpectrumIntegrated *spectrum_integrated = HOS_SPECTRUM_INTEGRATED(object);
  HosSpectrumIntegratedPrivate *priv  = SPECTRUM_INTEGRATED_GET_PRIVATE(object);
  /* FIXME free the accumulation buffer? */
  g_free(priv->integrand_idx);

  G_OBJECT_CLASS(hos_spectrum_integrated_parent_class)->finalize (object);
}

/*
 * Fill 'dest' with the value of integrated spectrum 'self' at location
 * 'idx', if possible.
 *
 * Unlike 'spectrum_integrate_accumulate', not guaranteed to eventually
 * succeed upon multiple calls.
 *
 * Returns:
 *   TRUE  - point was available; 
 *   FALSE - point not available yet; '*dest' is unchanged.
 */
static gboolean
spectrum_integrated_tickle_depr(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumIntegrated *spectrum_integrated = HOS_SPECTRUM_INTEGRATED(self);
  HosSpectrumIntegratedPrivate *priv = SPECTRUM_INTEGRATED_GET_PRIVATE(self);

  initialize_integrand_idx(priv, idx);

  gboolean success = TRUE;
  gint i;
  for (i = 0; i < priv->integrand_np; ++i)
    {
      priv->integrand_idx[0] = i;
      gboolean pt_found =
	spectrum_tickle(priv->integrand, root, priv->integrand_idx, &priv->accumulator[i]);
      success = success && pt_found;
    }

  if (success)
    {
      gdouble result = 0;
      for (i = 0; i < priv->integrand_np; ++i)
	result += priv->accumulator[i];
      *dest = result;
    }

  return success;

}


/*
 * Returns point 'idx' in spectrum 'self', possibly blocking.
 */
static gdouble
spectrum_integrated_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumIntegratedPrivate *priv = SPECTRUM_INTEGRATED_GET_PRIVATE(self);
  initialize_integrand_idx(priv, idx);

  gint i;
  for (i = 0; i < priv->integrand_np; ++i) priv->accumulator[i] = DATUM_UNKNOWN_VALUE;

  for (i = 0; i < priv->integrand_np; ++i)
    {
      if (!DATUM_IS_KNOWN(priv->accumulator[i]))
	{
	  gint j;
	  for (j = i + 1; j < priv->integrand_np; ++j)
	    {
	      if (!DATUM_IS_KNOWN(priv->accumulator[j]))
		{
		  priv->integrand_idx[0] = j;
		  gboolean found = spectrum_tickle(priv->integrand, root, priv->integrand_idx, &priv->accumulator[j]);
		  if (found)
		    DATUM_ENSURE_KNOWN(priv->accumulator[j]);
		}
	    }
	  priv->integrand_idx[0] = i;
	  gdouble next = spectrum_accumulate(priv->integrand, root, priv->integrand_idx);
	  priv->accumulator[i] = next;
	}
    }

  gdouble result = 0;
  for (i = 0; i < priv->integrand_np; ++i)
    result += priv->accumulator[i];
  return result;
}

/*
 * Returns:
 *  S' where S'(j, k) = Sum_i(self(i, j, k))
 */
HosSpectrum*
spectrum_integrate (HosSpectrum* self)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_INTEGRATED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);

  g_free(g_list_nth_data(dimensions, 0));
  dimensions = g_list_delete_link(dimensions, dimensions);
  spectrum_set_dimensions(result, dimensions);

  guint integrand_ndim = spectrum_ndim(self);

  g_object_ref(self);

  /* set up accumulation buffer */
  HosSpectrumIntegratedPrivate *priv = SPECTRUM_INTEGRATED_GET_PRIVATE(result);
  priv->integrand      = self;
  priv->integrand_np   = spectrum_np(self, 0);
  priv->integrand_ndim = integrand_ndim;
  priv->accumulator    = g_new0(gdouble, priv->integrand_np);
  priv->integrand_idx  = g_new(guint, integrand_ndim);

  return result;
}

static void
initialize_integrand_idx(HosSpectrumIntegratedPrivate *priv, guint* idx)
{
  //  memcpy(priv->integrand_idx + 1, idx, (priv->integrand_ndim - 1) * sizeof(guint));
}

static void
spectrum_integrated_increment(struct spectrum_iterator *self, guint dim, gint delta)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
  iterator_increment(integrated_iterator->integrand, dim + 1, delta);
}

static gboolean
spectrum_integrated_tickle(struct spectrum_iterator *self, gdouble *dest)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
  /* FIXME how to deal with the 'blocked' issue? */
  gdouble sum = 0;
  gboolean result = TRUE;
  gint i;
  for (i = 0; i < integrated_iterator->integrand->np[0]; ++i)
    {
      gdouble value = 0;
      gboolean hit = iterator_tickle(integrated_iterator->integrand, &value);
      if (hit == TRUE)
	sum += value;
      else
	result = FALSE;
      iterator_increment(integrated_iterator->integrand, 0, 1);
    }

  iterator_increment(integrated_iterator->integrand, 0, -integrated_iterator->integrand->np[0]);

  if (result == TRUE)
    *dest = sum;
  return result;
}

static gdouble
spectrum_integrated_wait(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;

  gdouble sum = 0;
  gint i;
  for (i = 0; i < integrated_iterator->integrand->np[0]; ++i)
    {
      sum += iterator_wait(integrated_iterator->integrand);
      iterator_increment(integrated_iterator->integrand, 0, 1);
    }

  iterator_increment(integrated_iterator->integrand, 0, -integrated_iterator->integrand->np[0]);

  return sum;
}

static void
spectrum_integrated_mark(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
  iterator_mark(integrated_iterator->integrand);
}

static struct spectrum_iterator*
spectrum_integrated_construct_iterator(HosSpectrum *self)
{
  struct integrated_iterator *result           = g_new0(struct integrated_iterator, 1);
  struct spectrum_iterator   *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv      = SPECTRUM_INTEGRATED_GET_PRIVATE(self);
  result->integrand = spectrum_construct_iterator(result->priv->integrand);

  /* FIXME */
  spectrum_iterator->tickle     = spectrum_integrated_tickle;
  spectrum_iterator->wait       = spectrum_integrated_wait;
  spectrum_iterator->increment  = spectrum_integrated_increment;
  spectrum_iterator->mark       = spectrum_integrated_mark;

  return spectrum_iterator;
}

static void
spectrum_integrated_free_iterator(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
}


