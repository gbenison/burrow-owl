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

#ifndef _HOS_HAVE_DIMENSION_SIM_H
#define _HOS_HAVE_DIMENSION_SIM_H

#include "hosdimension.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_DIMENSION_SIM              (hos_dimension_sim_get_type())
#define HOS_DIMENSION_SIM(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_DIMENSION_SIM, HosDimensionSim))
#define HOS_DIMENSION_SIM_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_DIMENSION_SIM, HosDimensionSimClass))
#define HOS_IS_DIMENSION_SIM(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_DIMENSION_SIM))
#define HOS_IS_DIMENSION_SIM_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_DIMENSION_SIM))
#define HOS_DIMENSION_SIM_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_DIMENSION_SIM, HosDimensionSimClass))
					
typedef struct _HosDimensionSim       HosDimensionSim;
typedef struct _HosDimensionSimClass  HosDimensionSimClass;

struct _HosDimensionSim
{
  HosDimension parent_instance;

  guint backing_dim;

};

struct _HosDimensionSimClass
{
  HosDimensionClass parent_class;
};

GType hos_dimension_sim_get_type (void);

HosDimensionSim* dimension_sim_new(gdouble min, gdouble max, guint np);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_DIMENSION_SIM_H  */
