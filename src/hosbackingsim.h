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

#ifndef _HOS_HAVE_BACKING_SIM_H
#define _HOS_HAVE_BACKING_SIM_H

#include "hosbacking.h"
#include "hosdimensionsim.h"
#include <burrow/spectrum.h>

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_BACKING_SIM              (hos_backing_sim_get_type())
#define HOS_BACKING_SIM(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_BACKING_SIM, HosBackingSim))
#define HOS_BACKING_SIM_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_BACKING_SIM, HosBackingSimClass))
#define HOS_IS_BACKING_SIM(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_BACKING_SIM))
#define HOS_IS_BACKING_SIM_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_BACKING_SIM))
#define HOS_BACKING_SIM_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_BACKING_SIM, HosBackingSimClass))
					
typedef struct _HosBackingSim       HosBackingSim;
typedef struct _HosBackingSimClass  HosBackingSimClass;

struct _HosBackingSim
{
  HosBacking parent_instance;

  guint ndim;
  gdouble *position;

  GList *dimensions;
  GList *peaks;

};

struct _HosBackingSimClass
{
  HosBackingClass parent_class;
};


typedef struct _sim_peak SimPeak;
typedef struct _sim_peak_product SimPeakProduct;
typedef struct _sim_profile SimProfile;
typedef struct _sim_profile_gaussian SimProfileGaussian;

struct _sim_peak
{
  HosBackingSim *backing;
  gdouble (*value)(SimPeak *self);
};

struct _sim_peak_product
{
  struct _sim_peak peak;
 
  gdouble intensity;

  GList *profiles;
};

struct _sim_profile
{
  gdouble (*value)(SimProfile *self, gdouble position);
};

struct _sim_profile_gaussian
{
  struct _sim_profile profile;

  gdouble center;
  gdouble std_dev;

};

HosBackingSim* backing_sim_new();
void backing_sim_append_dimension(HosBackingSim* self, HosDimensionSim* dimen_sim);
HosSpectrum* backing_sim_generate_spectrum(HosBackingSim* self);
void backing_sim_set_pos(HosBackingSim *self, guint idx, gdouble pos);
void backing_sim_append_peak(HosBackingSim *self, SimPeak *peak);

SimPeak* sim_peak_product_new(gdouble intensity);
SimProfile* sim_profile_gaussian_new(gdouble center, gdouble std_dev);
void sim_peak_product_append_profile(SimPeak *self, SimProfile *profile);

GType hos_backing_sim_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */



#endif /*  _HOS_HAVE_BACKING_SIM_H */
