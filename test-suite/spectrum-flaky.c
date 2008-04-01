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

#include "spectrum_priv.h"
#include "spectrum-flaky.h"

static gdouble   spectrum_flaky_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean  spectrum_flaky_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

G_DEFINE_TYPE (HosSpectrumFlaky, hos_spectrum_flaky, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_flaky_class_init(HosSpectrumFlakyClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->tickle     = spectrum_flaky_tickle;
  spectrum_class->accumulate = spectrum_flaky_accumulate;
}

static void
hos_spectrum_flaky_init(HosSpectrumFlaky *self)
{
}

static gdouble
spectrum_flaky_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  return spectrum_accumulate(HOS_SPECTRUM_FLAKY(self)->flakand, root, idx);
}

static gboolean
spectrum_flaky_tickle (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  if (g_random_double_range(0, 1.0) < HOS_SPECTRUM_FLAKY(self)->flake_factor)
    return spectrum_tickle(HOS_SPECTRUM_FLAKY(self)->flakand, root, idx, dest);
  else
    return FALSE;
}

/*
 * Return a version of 'self' that tickles unreliably.
 * i.e. tickle(spectrum_flakify(self)) only succeeds 1/(flake_factor) of the time
 * that tickle(self) succeeds, at random.
 */
HosSpectrum*
spectrum_flakify(HosSpectrum *self, gdouble flake_factor)
{
  HosSpectrum *result = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_FLAKY, NULL));

  spectrum_set_ndim(result, spectrum_ndim(self));

  HOS_SPECTRUM_FLAKY(result)->flake_factor = flake_factor;

  guint i;
  for (i = 0; i < spectrum_ndim(self); ++i)
    spectrum_set_np(result, i, spectrum_np(self, i));

  HOS_SPECTRUM_FLAKY(result)->flakand = self;
  g_object_ref(self);
  return result;
}

