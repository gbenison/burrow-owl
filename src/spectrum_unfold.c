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
#include "burrow/spectrum.h"
#include "spectrum_priv.h"
#include "utils.h"

/* no new public fields */
typedef HosSpectrum      HosSpectrumUnfolded;
typedef HosSpectrumClass HosSpectrumUnfoldedClass;

#define HOS_TYPE_SPECTRUM_UNFOLDED              (hos_spectrum_unfolded_get_type())

#define SPECTRUM_UNFOLDED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_UNFOLDED, HosSpectrumUnfoldedPrivate))
#define SPECTRUM_UNFOLDED_PRIVATE(o, field) ((SPECTRUM_UNFOLDED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumUnfoldedPrivate HosSpectrumUnfoldedPrivate;

struct _HosSpectrumUnfoldedPrivate
{
  HosSpectrum *base;
  
  guint    base_ndim;
  guint    base_idx;
  guint    base_np;

  gboolean negate_on_fold;
  gboolean negate_first;
};

static gdouble  spectrum_unfolded_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_unfolded_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

static void   spectrum_unfolded_dispose  (GObject *object);
static void   spectrum_unfolded_finalize (GObject *object);


G_DEFINE_TYPE (HosSpectrumUnfolded, hos_spectrum_unfolded, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_unfolded_class_init(HosSpectrumUnfoldedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_unfolded_dispose;
  gobject_class->finalize    = spectrum_unfolded_finalize;

  //  spectrum_class->accumulate = spectrum_unfolded_accumulate;
  //  spectrum_class->tickle     = spectrum_unfolded_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumUnfoldedPrivate));
}

static void
hos_spectrum_unfolded_init(HosSpectrumUnfolded* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_unfolded_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(self);
  guint new_idx[priv->base_ndim];
  gint i;
  for (i = 0; i < priv->base_ndim; ++i)
    new_idx[i] = idx[i];

  gboolean negated = FALSE;
  if (priv->negate_on_fold)
    {
      gboolean is_even_segment = (((idx[priv->base_idx] / priv->base_np) % 2) == 0);
      if (is_even_segment && priv->negate_first)   negated = TRUE;
      if (!is_even_segment && !priv->negate_first) negated = TRUE;
    }

  new_idx[priv->base_idx] %= priv->base_np;

  gdouble result = spectrum_accumulate(priv->base, root, new_idx);
  if (negated) result = -result;
  return result;
}

static gboolean
spectrum_unfolded_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(self);
  guint new_idx[priv->base_ndim];
  gint i;
  for (i = 0; i < priv->base_ndim; ++i)
    new_idx[i] = idx[i];

  gboolean negated = FALSE;
  if (priv->negate_on_fold)
    {
      gboolean is_even_segment = (((idx[priv->base_idx] / priv->base_np) % 2) == 0);
      if (is_even_segment && priv->negate_first)   negated = TRUE;
      if (!is_even_segment && !priv->negate_first) negated = TRUE;
    }

  new_idx[priv->base_idx] %= priv->base_np;

  gboolean result = spectrum_tickle(priv->base, root, new_idx, dest);
  if (result && negated) { *dest = -*dest; }
  return result;
}

static void
spectrum_unfolded_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_UNFOLDED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_unfolded_parent_class)->dispose (object);
}

static void
spectrum_unfolded_finalize(GObject *object)
{
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(object);
  G_OBJECT_CLASS(hos_spectrum_unfolded_parent_class)->finalize (object);
}

/*
 * Unfold dimension 'idx' of 'self' by appending identical copies
 * of 'self', possibly negating every other one.
 * e.g.
 * spectrum_np(S', idx) = spectrum_np(S, idx) * (1 + downfield + upfield)
 */
HosSpectrum*
spectrum_unfold(HosSpectrum* self,
		const guint idx,
		const guint downfield,
		const guint upfield,
		const gboolean negate_on_fold)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_UNFOLDED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(result);

  priv->base      = self;
  priv->base_ndim = spectrum_ndim(self);
  g_object_ref(self);

  priv->base_idx  = idx;
  priv->base_np   = spectrum_np(self, idx);

  priv->negate_on_fold = negate_on_fold;
  priv->negate_first = ((downfield % 2) == 1);
  
  dimension_t* unfolded_dimension = g_list_nth_data(dimensions, idx);
  unfolded_dimension->np   *= (1 + downfield + upfield);
  unfolded_dimension->sw   *= (1 + downfield + upfield);
  unfolded_dimension->orig += downfield * spectrum_sw(self, idx);

  spectrum_set_dimensions(result, dimensions);

  return result;
}



