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
typedef HosSpectrum      HosSpectrumDiagonal;
typedef HosSpectrumClass HosSpectrumDiagonalClass;

#define HOS_TYPE_SPECTRUM_DIAGONAL              (hos_spectrum_diagonal_get_type())

#define SPECTRUM_DIAGONAL_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_DIAGONAL, HosSpectrumDiagonalPrivate))
#define SPECTRUM_DIAGONAL_PRIVATE(o, field) ((SPECTRUM_DIAGONAL_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumDiagonalPrivate HosSpectrumDiagonalPrivate;

struct _HosSpectrumDiagonalPrivate
{
  HosSpectrum *base;
  
  guint  base_ndim;
  guint *schedule;
  guint  dim_1_offset;
  guint *base_idx;
};

static gdouble  spectrum_diagonal_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_diagonal_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

static void   spectrum_diagonal_dispose  (GObject *object);
static void   spectrum_diagonal_finalize (GObject *object);


G_DEFINE_TYPE (HosSpectrumDiagonal, hos_spectrum_diagonal, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_diagonal_class_init(HosSpectrumDiagonalClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_diagonal_dispose;
  gobject_class->finalize    = spectrum_diagonal_finalize;

  spectrum_class->accumulate = spectrum_diagonal_accumulate;
  spectrum_class->tickle     = spectrum_diagonal_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumDiagonalPrivate));
}

static void
hos_spectrum_diagonal_init(HosSpectrumDiagonal* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_diagonal_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumDiagonalPrivate *priv = SPECTRUM_DIAGONAL_GET_PRIVATE(self);
  memcpy(priv->base_idx + 1, idx, (priv->base_ndim - 1) * sizeof(guint));
  priv->base_idx[1] += priv->dim_1_offset;
  priv->base_idx[0] = priv->schedule[idx[0]];
  return spectrum_accumulate(priv->base, root, priv->base_idx);
}

static gboolean
spectrum_diagonal_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumDiagonalPrivate *priv = SPECTRUM_DIAGONAL_GET_PRIVATE(self);
  memcpy(priv->base_idx + 1, idx, (priv->base_ndim - 1) * sizeof(guint));
  priv->base_idx[1] += priv->dim_1_offset;
  priv->base_idx[0] = priv->schedule[idx[0]];
  return spectrum_tickle(priv->base, root, priv->base_idx, dest);
}

static void
spectrum_diagonal_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_DIAGONAL_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_diagonal_parent_class)->dispose (object);
}

static void
spectrum_diagonal_finalize(GObject *object)
{
  HosSpectrumDiagonalPrivate *priv = SPECTRUM_DIAGONAL_GET_PRIVATE(object);

  g_free(priv->base_idx);
  g_free(priv->schedule);

  G_OBJECT_CLASS(hos_spectrum_diagonal_parent_class)->finalize (object);
}

/*
 * Return the diagonal projection of 'self', i.e. 
 * S'(j, k, ...) = S(i*, j, k, ...)
 *
 * where i* is nearest to 'j' in chemical shift.
 */
HosSpectrum*
spectrum_diagonal_project(HosSpectrum *self)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_DIAGONAL, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);
  HosSpectrumDiagonalPrivate *priv = SPECTRUM_DIAGONAL_GET_PRIVATE(result);

  g_free(g_list_nth_data(dimensions, 0));
  dimensions = g_list_delete_link(dimensions, dimensions);

  dimension_t* first_dimension = (dimension_t*)(dimensions->data);

  g_object_ref(self);

  priv->base      = self;
  priv->base_ndim = spectrum_ndim(self);
  priv->base_idx  = g_new(guint, priv->base_ndim);

  /* calculate the schedule */
  gdouble orig_new = MIN(spectrum_orig_ppm(self, 0),
			 spectrum_orig_ppm(self, 1));
  gdouble giro_new = MAX(spectrum_giro_ppm(self, 0),
			 spectrum_giro_ppm(self, 1));

  g_return_val_if_fail(orig_new > giro_new, NULL);

  priv->dim_1_offset    = spectrum_ppm2pt(self, 1, orig_new);
  first_dimension->np   = spectrum_ppm2pt(self, 1, giro_new) - priv->dim_1_offset;

  priv->schedule  = g_new0(guint, first_dimension->np);

  gdouble pt2hz = spectrum_sw(self, 1) / spectrum_np(self, 1);

  first_dimension->orig = first_dimension->orig - priv->dim_1_offset * pt2hz;
  first_dimension->sw   = first_dimension->np * pt2hz;

  gint i;
  for (i = 0; i < first_dimension->np; ++i)
    {
      gint    idx = i + priv->dim_1_offset;
      gdouble ppm = spectrum_pt2ppm(self, 1, idx);

      priv->schedule[i] = spectrum_ppm2pt(self, 0, ppm);

    }

  spectrum_set_dimensions(result, dimensions);

  return result;

}
