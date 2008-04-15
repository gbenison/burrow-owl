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

/* no new public fields */
typedef HosSpectrum      HosSpectrumTransposed;
typedef HosSpectrumClass HosSpectrumTransposedClass;

#define HOS_TYPE_SPECTRUM_TRANSPOSED              (hos_spectrum_transposed_get_type())
#define HOS_IS_SPECTRUM_TRANSPOSED(obj)  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_TRANSPOSED))

#define SPECTRUM_TRANSPOSED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_TRANSPOSED, HosSpectrumTransposedPrivate))
#define SPECTRUM_TRANSPOSED_PRIVATE(o, field) ((SPECTRUM_TRANSPOSED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumTransposedPrivate HosSpectrumTransposedPrivate;

struct _HosSpectrumTransposedPrivate
{
  HosSpectrum *base;
  guint  base_ndim;
  guint  *map;
};

static gdouble  spectrum_transposed_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_transposed_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

static void   spectrum_transposed_dispose  (GObject *object);
static void   spectrum_transposed_finalize (GObject *object);


G_DEFINE_TYPE (HosSpectrumTransposed, hos_spectrum_transposed, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_transposed_class_init(HosSpectrumTransposedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_transposed_dispose;
  gobject_class->finalize    = spectrum_transposed_finalize;

  spectrum_class->accumulate = spectrum_transposed_accumulate;
  spectrum_class->tickle     = spectrum_transposed_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumTransposedPrivate));
}

static void
hos_spectrum_transposed_init(HosSpectrumTransposed* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_transposed_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumTransposedPrivate *priv = SPECTRUM_TRANSPOSED_GET_PRIVATE(self);

  guint new_idx[priv->base_ndim];
  gint i;
  for (i = 0; i < priv->base_ndim; ++i)
    new_idx[priv->map[i]] = idx[i];

  return spectrum_accumulate(priv->base, root, new_idx);
}

static gboolean
spectrum_transposed_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumTransposedPrivate *priv = SPECTRUM_TRANSPOSED_GET_PRIVATE(self);

  guint new_idx[priv->base_ndim];
  gint i;
  for (i = 0; i < priv->base_ndim; ++i)
    new_idx[priv->map[i]] = idx[i];

  return spectrum_tickle(priv->base, root, new_idx, dest);
}

static void
spectrum_transposed_dispose(GObject *object)
{
  g_object_unref(SPECTRUM_TRANSPOSED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_transposed_parent_class)->dispose (object);
}

static void
spectrum_transposed_finalize(GObject *object)
{
  HosSpectrumTransposedPrivate *priv = SPECTRUM_TRANSPOSED_GET_PRIVATE(object);
  g_free(priv->map);
  G_OBJECT_CLASS(hos_spectrum_transposed_parent_class)->finalize (object);
}

/*
 * Transpose 'self' by bumping dimension 'idx' up to first place,
 * e.g.
 * S'(i, j, k, idx, q, ...) = S(idx, i, j, k, q, ...)
 */
HosSpectrum*
spectrum_transpose(HosSpectrum *self, guint idx)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_TRANSPOSED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);
  HosSpectrumTransposedPrivate *priv = SPECTRUM_TRANSPOSED_GET_PRIVATE(result);

  priv->map  = (guint*)g_new0(guint, spectrum_ndim(self));
  priv->base_ndim = spectrum_ndim(self);

  g_object_ref(self);

  gint i;
  if (HOS_IS_SPECTRUM_TRANSPOSED(self))
    {
      HosSpectrumTransposedPrivate *self_priv = SPECTRUM_TRANSPOSED_GET_PRIVATE(self);
      for (i = 0; i < spectrum_ndim(self); ++i)
	priv->map[i] = self_priv->map[i];
      priv->base = self_priv->base;
    }
  else
    {
      for (i = 0; i < spectrum_ndim(self); ++i)
	priv->map[i] = i;
      priv->base = self;
    }

  /* swap map entries */
  guint tmp = priv->map[0];
  priv->map[0] = priv->map[idx];
  for (i = idx; i > 1; --i)
    priv->map[i] = priv->map[i - 1];
  priv->map[1] = tmp;

  /* swap dimension list entries */
  GList* first_dimension = g_list_nth(dimensions, idx);
  dimensions = g_list_remove_link(dimensions, first_dimension);
  dimensions = g_list_concat(first_dimension, dimensions);

  spectrum_set_dimensions(result, dimensions);

  return result;

}
