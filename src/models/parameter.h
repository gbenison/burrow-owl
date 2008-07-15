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

#ifndef _HAVE_PARAMETER_H
#define _HAVE_PARAMETER_H

#include "model.h"

G_BEGIN_DECLS

#define HOS_TYPE_PARAMETER            (hos_parameter_get_type())
#define HOS_PARAMETER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_PARAMETER, HosParameter))
#define HOS_PARAMETER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_PARAMETER, HosParameterClass))
#define HOS_IS_PARAMETER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_PARAMETER))
#define HOS_IS_PARAMETER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_PARAMETER))
#define HOS_PARAMETER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_PARAMETER, HosParameterClass))

typedef struct _HosParameter      HosParameter;
typedef struct _HosParameterClass HosParameterClass;

struct _HosParameter
{
  HosModel   parent_instance;
  gdouble    value;

  gdouble    prior_mean;
  gdouble    prior_stddev;
};

struct _HosParameterClass
{
  HosModelClass parent_class;
};

HosParameter* CONSTRUCTOR parameter_new(gdouble value, gdouble prior_mean, gdouble prior_stddev);

G_END_DECLS

#endif /* not _HAVE_PARAMETER_H */
