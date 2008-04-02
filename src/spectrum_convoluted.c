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

#include "burrow/spectrum_convoluted.h"
#include "spectrum_priv.h"

#define SPECTRUM_CONVOLUTED_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_CONVOLUTED, HosSpectrumConvolutedPrivate))
#define SPECTRUM_CONVOLUTED_PRIVATE(o, field) ((SPECTRUM_CONVOLUTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumConvolutedPrivate HosSpectrumConvolutedPrivate;

struct _HosSpectrumConvolutedPrivate
{
  HosSpectrum *A;
  HosSpectrum *B;
  guint A_ndim;
};

static gdouble  spectrum_convoluted_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_convoluted_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble *dest);

static void   spectrum_convoluted_dispose  (GObject *object);
static void   spectrum_convoluted_finalize (GObject *object);

G_DEFINE_TYPE (HosSpectrumConvoluted, hos_spectrum_convoluted, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_convoluted_class_init(HosSpectrumConvolutedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_convoluted_dispose;
  gobject_class->finalize    = spectrum_convoluted_finalize;

  spectrum_class->accumulate = spectrum_convoluted_accumulate;
  spectrum_class->tickle     = spectrum_convoluted_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumConvolutedPrivate));
}

static void
hos_spectrum_convoluted_init(HosSpectrumConvoluted *self)
{
}

static void
spectrum_convoluted_dispose(GObject *object)
{
  g_object_unref(SPECTRUM_CONVOLUTED_PRIVATE(object, A));
  g_object_unref(SPECTRUM_CONVOLUTED_PRIVATE(object, B));
  G_OBJECT_CLASS(hos_spectrum_convoluted_parent_class)->dispose (object);
}

static void
spectrum_convoluted_finalize(GObject *object)
{
}

static gdouble
spectrum_convoluted_accumulate(HosSpectrum* self, HosSpectrum *root, guint* idx)
{
  gdouble A, B;
  guint* idx_A = idx;
  guint* idx_B = idx + SPECTRUM_CONVOLUTED_PRIVATE(self, A_ndim);

  gboolean result_A = spectrum_tickle(SPECTRUM_CONVOLUTED_PRIVATE(self, A), root, idx_A, &A);
  gboolean result_B = spectrum_tickle(SPECTRUM_CONVOLUTED_PRIVATE(self, B), root, idx_B, &B);

  if (!result_A)
    A = spectrum_accumulate(SPECTRUM_CONVOLUTED_PRIVATE(self, A), root, idx_A);

  if (!result_B)
    B = spectrum_accumulate(SPECTRUM_CONVOLUTED_PRIVATE(self, B), root, idx_B);

  return A * B;

}

static gboolean
spectrum_convoluted_tickle(HosSpectrum *self, HosSpectrum *root, guint *idx, gdouble *dest)
{
  gdouble A, B;
  guint* idx_A = idx;
  guint* idx_B = idx + SPECTRUM_CONVOLUTED_PRIVATE(self, A_ndim);

  gboolean result_A = spectrum_tickle(SPECTRUM_CONVOLUTED_PRIVATE(self, A), root, idx_A, &A);
  gboolean result_B = spectrum_tickle(SPECTRUM_CONVOLUTED_PRIVATE(self, B), root, idx_B, &B);

  if (result_A && result_B)
    {
      *dest = A * B;
      return TRUE;
    }
  else
    return FALSE;
}

/*
 * Returns: the convolution of spectrum A and spectrum B.
 *
 * Definition:  let C <- spectrum_convolute(A, B);
 *   then  C(a', b', a, b) = A(a', b') * B(a, b)
 *
 */
HosSpectrum*
spectrum_convolute (HosSpectrum *A, HosSpectrum *B)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_CONVOLUTED, NULL);
  guint ndim_A = spectrum_ndim(A);
  guint ndim_B = spectrum_ndim(B);

  spectrum_set_ndim(result, ndim_A + ndim_B);
  gint i;
  for (i = 0; i < ndim_A; ++i)
    spectrum_set_np(result, i, spectrum_np(A, i));
  for (i = 0; i < ndim_B; ++i)
    spectrum_set_np(result, i + ndim_A, spectrum_np(B, i));

  SPECTRUM_CONVOLUTED_PRIVATE(result, A) = A;
  SPECTRUM_CONVOLUTED_PRIVATE(result, B) = B;
  SPECTRUM_CONVOLUTED_PRIVATE(result, A_ndim) = spectrum_ndim(A);

  g_object_ref(A);
  g_object_ref(B);

  return result;
}
