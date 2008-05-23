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

static gdouble   spectrum_flaky_wait   (struct spectrum_iterator* self);
static gboolean  spectrum_flaky_tickle (struct spectrum_iterator* self, gdouble *dest);

static struct spectrum_iterator* spectrum_flaky_construct_iterator (HosSpectrum *self);
static void                      spectrum_flaky_free_iterator      (struct spectrum_iterator* self);

G_DEFINE_TYPE (HosSpectrumFlaky, hos_spectrum_flaky, HOS_TYPE_SPECTRUM)

struct flaky_iterator
{
  struct spectrum_iterator  parent;

  gdouble flake_factor;
  struct spectrum_iterator* flakand;
};

static void
hos_spectrum_flaky_class_init(HosSpectrumFlakyClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->construct_iterator = spectrum_flaky_construct_iterator;
  spectrum_class->free_iterator      = spectrum_flaky_free_iterator;
}

static void
hos_spectrum_flaky_init(HosSpectrumFlaky *self)
{
}

static gdouble
spectrum_flaky_wait (struct spectrum_iterator* self)
{
  gdouble result = iterator_wait(((struct flaky_iterator*)self)->flakand);
  return result;
}

static gboolean
spectrum_flaky_tickle (struct spectrum_iterator* self, gdouble *dest)
{
  if (g_random_double_range(0, 1.0) < ((struct flaky_iterator*)self)->flake_factor)
    return iterator_tickle(((struct flaky_iterator*)self)->flakand, dest);
  else
    return FALSE;
}

static void
spectrum_flaky_increment(struct spectrum_iterator* self, guint dim, gint delta)
{
  struct flaky_iterator* flaky_iterator = (struct flaky_iterator*)self;
  iterator_increment(flaky_iterator->flakand, dim, delta);
  g_assert(self->idx[dim] == flaky_iterator->flakand->idx[dim]);
}

static void
spectrum_flaky_mark(struct spectrum_iterator* self)
{
  iterator_mark(((struct flaky_iterator*)self)->flakand);
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

  spectrum_set_dimensions(result,
			  spectrum_copy_dimensions(self));

  HOS_SPECTRUM_FLAKY(result)->flake_factor = flake_factor;

  HOS_SPECTRUM_FLAKY(result)->flakand = self;
  g_object_ref(self);
  return result;
}

static struct spectrum_iterator*
spectrum_flaky_construct_iterator(HosSpectrum *self)
{
  struct flaky_iterator    *result = g_new0(struct flaky_iterator, 1);
  struct spectrum_iterator *spectrum_iterator = (struct spectrum_iterator*)result;

  spectrum_iterator->tickle     = spectrum_flaky_tickle;
  spectrum_iterator->wait       = spectrum_flaky_wait;
  spectrum_iterator->increment  = spectrum_flaky_increment;
  spectrum_iterator->mark       = spectrum_flaky_mark;

  result->flake_factor  = HOS_SPECTRUM_FLAKY(self)->flake_factor;
  result->flakand       = spectrum_construct_iterator(HOS_SPECTRUM_FLAKY(self)->flakand);

  return spectrum_iterator;
}

static void
spectrum_flaky_free_iterator(struct spectrum_iterator* self)
{
  iterator_free(((struct flaky_iterator*)self)->flakand);
  g_free(self);
}
