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

#include "include/spectrum_integrate.h"

#define SPECTRUM_INTEGRATED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedPrivate))
#define SPECTRUM_INTEGRATED_PRIVATE(o, field) ((SPECTRUM_INTEGRATED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumIntegratedPrivate HosSpectrumIntegratedPrivate;

struct _HosSpectrumIntegratedPrivate
{
  HosSpectrum *integrand;
  gdouble     *accumulator;  /* cache for spectrum_integrate_accumulate */
  guint       integrate_np;  /* vector length of 'accumulator' */
};


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
spectrum_integrate_tickle(HosSpectrum* self, guint* idx, gdouble* dest)
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
spectrum_integrate_accumulate(HosSpectrum* self, guint* idx, gdouble* dest)
{
  /* FIXME */
  
}

