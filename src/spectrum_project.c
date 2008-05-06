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
typedef HosSpectrum      HosSpectrumProjected;
typedef HosSpectrumClass HosSpectrumProjectedClass;

#define HOS_TYPE_SPECTRUM_PROJECTED              (hos_spectrum_projected_get_type())

#define SPECTRUM_PROJECTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_PROJECTED, HosSpectrumProjectedPrivate))
#define SPECTRUM_PROJECTED_PRIVATE(o, field) ((SPECTRUM_PROJECTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumProjectedPrivate HosSpectrumProjectedPrivate;

struct _HosSpectrumProjectedPrivate
{
  HosSpectrum *base;
  
  guint  base_ndim;
  guint *base_idx;
};

static gdouble  spectrum_projected_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_projected_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

static void   spectrum_projected_dispose  (GObject *object);
static void   spectrum_projected_finalize (GObject *object);


G_DEFINE_TYPE (HosSpectrumProjected, hos_spectrum_projected, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_projected_class_init(HosSpectrumProjectedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_projected_dispose;
  gobject_class->finalize    = spectrum_projected_finalize;

  spectrum_class->accumulate = spectrum_projected_accumulate;
  spectrum_class->tickle     = spectrum_projected_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumProjectedPrivate));
}

static void
hos_spectrum_projected_init(HosSpectrumProjected* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_projected_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(self);
  memcpy(priv->base_idx + 1, idx, (priv->base_ndim - 1) * sizeof(guint));
  return spectrum_accumulate(priv->base, root, priv->base_idx);
}

static gboolean
spectrum_projected_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(self);
  memcpy(priv->base_idx + 1, idx, (priv->base_ndim - 1) * sizeof(guint));
  return spectrum_tickle(priv->base, root, priv->base_idx, dest);
}

static void
spectrum_projected_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_PROJECTED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_projected_parent_class)->dispose (object);
}

static void
spectrum_projected_finalize(GObject *object)
{
  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(object);

  g_free(priv->base_idx);

  G_OBJECT_CLASS(hos_spectrum_projected_parent_class)->finalize (object);
}

/*
 * Return the projection of 'self', i.e. 
 * S'(i, j, k, ...) = S(idx, i, j, k, ...)
 */
HosSpectrum*
spectrum_project(HosSpectrum *self, guint idx)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_PROJECTED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);

  g_free(g_list_nth_data(dimensions, 0));
  dimensions = g_list_delete_link(dimensions, dimensions);

  g_object_ref(self);

  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(result);

  priv->base      = self;
  priv->base_ndim = spectrum_ndim(self);
  priv->base_idx  = g_new(guint, priv->base_ndim);
  priv->base_idx[0] = idx;

  spectrum_set_dimensions(result, dimensions);

  return result;

}
