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

#include "burrow/spectrum_integrate.h"

#define SPECTRUM_INTEGRATED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedPrivate))
#define SPECTRUM_INTEGRATED_PRIVATE(o, field) ((SPECTRUM_INTEGRATED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumIntegratedPrivate HosSpectrumIntegratedPrivate;

struct _HosSpectrumIntegratedPrivate
{
  HosSpectrum *integrand;
  gdouble     *accumulator;  /* cache for spectrum_integrate_accumulate */
  guint       integrand_np;  /* vector length of 'accumulator' */
};

static gboolean spectrum_integrated_accumulate (HosSpectrum* self, guint* idx, gdouble* dest);
static gboolean spectrum_integrated_tickle     (HosSpectrum* self, guint* idx, gdouble* dest);

G_DEFINE_TYPE (HosSpectrumIntegrated, hos_spectrum_integrated, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_integrated_class_init(HosSpectrumIntegratedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_integrated_dispose;
  gobject_class->finalize    = spectrum_integrated_finalize;

  spectrum_class->accumulate = spectrum_integrated_accumulate;
  spectrum_class->tickle     = spectrum_integrated_tickle;
}

static void
hos_spectrum_integrated_init(HosSpectrumIntegrated* self)
{
  /* FIXME */
  /* anything? */
}

static void
hos_spectrum_integrated_dispose(GObject *object)
{
  /* FIXME unref the integrand */
  g_object_unref();
  G_OBJECT_CLASS(hos_spectrum_integrated_parent_class)->dispose (object);
}

static void
hos_spectrum_integrated_finalize(GObject *object)
{
  HosSpectrumIntegrated *spectrum_integrated = HOS_SPECTRUM_INTEGRATED(object);
  /* FIXME free the accumulation buffer? */
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
spectrum_integrated_tickle(HosSpectrum* self, guint* idx, gdouble* dest)
{
  /* FIXME */
  /* tickle all child spectrum points; if all available, then success */
}

/*
 * Instantiate point 'idx' in spectrum 'self'.  Not guaranteed to succeed on the first call,
 * but caches intermediate results such that repeated calls will eventually result in success.
 *
 * Returns:
 *   TRUE  - point was available, '*dest' now contains value of point 'idx'; 
 *   FALSE - point not available yet; '*dest' is unchanged.
 */
static gboolean
spectrum_integrated_accumulate(HosSpectrum* self, guint* idx, gdouble* dest)
{
  /* FIXME */
  
}

/*
 * Returns:
 *  S' where S'(j, k) = Sum_i(self(i, j, k))
 */
HosSpectrum*
spectrum_integrate (HosSpectrum* self)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_INTEGRATED, NULL);
  result->ndim = self->ndim - 1;
  SPECTRUM_INTEGRATED_PRIVATE(result, integrand) = self;
  g_object_ref(self);

  /* set up accumulation buffer */
  SPECTRUM_INTEGRATED_PRIVATE(result, integrand_np) = spectrum_np(self, 0);
  SPECTRUM_INTEGRATED_PRIVATE(result, accumulator)  = g_new(gdouble, SPECTRUM_INTEGRATED_PRIVATE(result, integrand_np));

  return result;
}
