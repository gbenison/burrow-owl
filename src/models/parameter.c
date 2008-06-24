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

#include "parameter.h"

G_DEFINE_TYPE (HosParameter, hos_parameter, HOS_TYPE_MODEL)

static void parameter_iterator_fill(model_iterator_t* self, gdouble *dest);

static void
hos_parameter_class_init(HosParameterClass *klass)
{
  GObjectClass  *gobject_class = G_OBJECT_CLASS(klass);
  HosModelClass *model_class   = HOS_MODEL_CLASS(klass);

  model_class->iterator_fill   = parameter_iterator_fill;
}

static void
hos_parameter_init(HosParameter *self)
{
}

static void
parameter_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  *dest = HOS_PARAMETER(self->root)->value;
}

HosParameter*
parameter_new(gdouble value, gdouble prior_mean, gdouble prior_stddev)
{
  HosParameter *result = g_object_new(HOS_TYPE_PARAMETER, NULL);

  result->value        = value;
  result->prior_mean   = prior_mean;
  result->prior_stddev = prior_stddev;
}

