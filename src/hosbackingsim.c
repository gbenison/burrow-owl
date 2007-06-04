/*
 *  Copyright (C) 2006 Greg Benison
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

#include <assert.h>
#include <math.h>
#include "hosbackingsim.h"
#include "hosdimensionsim.h"

static HosBackingClass *parent_class = NULL;

static void hos_backing_sim_class_init (HosBackingSimClass *klass);
static void hos_backing_sim_init(HosBackingSim  *self);
static void backing_sim_set_ndim(HosBackingSim *self, guint ndim);
static void backing_sim_copy(HosBacking *src, HosBacking *dest);
static gdouble backing_sim_get_pos(HosBackingSim *self, guint idx);
static gdouble backing_sim_peek(HosBackingSim* self);
static gdouble sim_peak_value(SimPeak *self);
static gdouble peak_product_value(SimPeak *self);
static gdouble sim_profile_value(SimProfile *self, gdouble pos);
static gdouble gaussian_value(SimProfile *self, gdouble position);

/*****************************************/

GType
hos_backing_sim_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosBackingSimClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_backing_sim_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBackingSim),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_backing_sim_init,
      };

      type = g_type_register_static (HOS_TYPE_BACKING,
				     "HosBackingSim",
				     &info,
				     0);
    }

  return type;
}

static void
hos_backing_sim_class_init (HosBackingSimClass *klass)
{
  HosBackingClass *backing_class = HOS_BACKING_CLASS(klass);
  
  parent_class = g_type_class_peek_parent (klass);

  backing_class->peek = (PeekFunc)backing_sim_peek;
  backing_class->copy = backing_sim_copy;
  
}

static void
hos_backing_sim_init(HosBackingSim  *self)
{
  /* FIXME */
}

static void
backing_sim_set_ndim(HosBackingSim *self, guint ndim)
{
  g_return_if_fail(HOS_IS_BACKING_SIM(self));
  self->ndim = ndim;
  self->position = g_new0(gdouble, ndim);
}

HosBackingSim*
backing_sim_new()
{
  HosBackingSim *result = g_object_new(HOS_TYPE_BACKING_SIM, NULL);
  backing_sim_set_ndim(result, 0);
  g_object_ref(G_OBJECT(result));

  return result;
}

void
backing_sim_append_dimension(HosBackingSim* self, HosDimensionSim* dimen_sim)
{
  HosDimension* dimen = NULL;

  g_return_if_fail(HOS_IS_DIMENSION_SIM(dimen_sim));
  g_return_if_fail(HOS_IS_BACKING_SIM(self));

  dimen = HOS_DIMENSION(dimen_sim);

  g_object_ref(dimen_sim);

  dimen->backing = HOS_BACKING(self);
  dimen_sim->backing_dim = self->ndim;
  backing_sim_set_ndim(self, self->ndim + 1);

  self->dimensions = g_list_append(self->dimensions, g_list_append(NULL, dimen));

}

static void
backing_sim_copy(HosBacking *src, HosBacking *dest)
{
  if (HOS_BACKING_CLASS(parent_class)->copy != NULL)
    HOS_BACKING_CLASS(parent_class)->copy(src, dest);

  HOS_BACKING_SIM(dest)->peaks = g_list_copy(HOS_BACKING_SIM(src)->peaks);

  backing_sim_set_ndim(HOS_BACKING_SIM(dest), HOS_BACKING_SIM(src)->ndim);

}

/*
 * A simulated backing object maintains state in the form of a 'position pointer'
 * which is a current location in ppm units.
 * The current location pointer is used to calculate the backing object's value
 * during backing_peek() operation.
 */
void
backing_sim_set_pos(HosBackingSim *self, guint idx, gdouble pos)
{
  g_return_if_fail(HOS_IS_BACKING_SIM(self));
  g_return_if_fail(idx < self->ndim);

  assert(self->position != NULL);

  (self->position)[idx] = pos;
}

void
backing_sim_append_peak(HosBackingSim *self, SimPeak *peak)
{
  g_return_if_fail(HOS_IS_BACKING_SIM(self));
  self->peaks = g_list_append(self->peaks, peak);
  peak->backing = self;
}

SimPeak*
sim_peak_product_new(gdouble intensity)
{
  SimPeakProduct* result = g_new0(SimPeakProduct, 1);
  SimPeak* peak = (SimPeak*)result;

  result->intensity = intensity;
  peak->value = peak_product_value;

  return peak;
}

static gdouble
backing_sim_get_pos(HosBackingSim *self, guint idx)
{
  assert(HOS_IS_BACKING_SIM(self));
  assert(idx < self->ndim);

  assert(self->position != NULL);

  return (self->position)[idx];
}

static gdouble
backing_sim_peek(HosBackingSim* self)
{
  gdouble result = 0;
  GList *iter;

  /* iterate through self->peaks */
  for (iter = self->peaks; iter != NULL; iter = iter->next)
    {
      SimPeak *peak = (SimPeak*)(iter->data);
      result += sim_peak_value(peak);
    }

  return result;
}

/****** peak models ******/

static gdouble
sim_peak_value(SimPeak *self)
{
  assert(self->backing != NULL);
  assert(self->value != NULL);
  return self->value(self);
}

static gdouble
peak_product_value(SimPeak *self)
{
  gdouble result = 0;
  SimPeakProduct *peak_product = (SimPeakProduct*)self;
  GList *iter;
  guint profile_idx = 0;
  HosBackingSim *backing = self->backing;

  result = peak_product->intensity;

  for (iter = peak_product->profiles; iter != NULL; iter = iter->next)
    {
      SimProfile *profile = (SimProfile*)(iter->data);

      result *= sim_profile_value(profile, backing_sim_get_pos(backing, profile_idx));
      ++profile_idx;
    }

  return result;
}

static gdouble
sim_profile_value(SimProfile *self, gdouble pos)
{
  return self->value(self, pos);
}

static gdouble
gaussian_value(SimProfile *self, gdouble position)
{
  SimProfileGaussian *gaussian = (SimProfileGaussian*)self;

  gdouble delta = (position - gaussian->center);

  return exp(- delta * delta / (2 * gaussian->std_dev * gaussian->std_dev));

}

SimProfile*
sim_profile_gaussian_new(gdouble center, gdouble std_dev)
{
  SimProfileGaussian *result = g_new0(SimProfileGaussian, 1);

  ((SimProfile*)result)->value = gaussian_value;

  result->center = center;
  result->std_dev = std_dev;

  return (SimProfile*)result;
}

void
sim_peak_product_append_profile(SimPeak *self, SimProfile *profile)
{
  SimPeakProduct *peak_product = NULL;

  assert(self != NULL);
  assert(profile != NULL);

  peak_product = (SimPeakProduct*)self;

  peak_product->profiles = g_list_append(peak_product->profiles, profile);
}

/*************/

HosSpectrum*
backing_sim_generate_spectrum(HosBackingSim* self)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM, NULL);

  result->dimensions = self->dimensions;

  return result;
}
