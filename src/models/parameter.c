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

enum {
  PROP_0,
  PROP_VALUE,
  PROP_PRIOR_MEAN,
  PROP_PRIOR_STDDEV
};


static void hos_parameter_set_property (GObject         *object,
					guint            prop_id,
					const GValue    *value,
					GParamSpec      *pspec);
static void hos_parameter_get_property (GObject         *object,
					guint            prop_id,
					GValue          *value,
					GParamSpec      *pspec);

G_DEFINE_TYPE (HosParameter, hos_parameter, HOS_TYPE_MODEL)

static void parameter_iterator_fill(model_iterator_t* self, gdouble *dest);
static void parameter_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np);

static void
hos_parameter_class_init(HosParameterClass *klass)
{
  GObjectClass  *gobject_class = G_OBJECT_CLASS(klass);
  HosModelClass *model_class   = HOS_MODEL_CLASS(klass);

  gobject_class->set_property = hos_parameter_set_property;
  gobject_class->get_property = hos_parameter_get_property;

#define STD_P_SPEC(name, blurb)   g_param_spec_double (name, name, blurb, -G_MAXDOUBLE, G_MAXDOUBLE, 0, G_PARAM_READABLE | G_PARAM_WRITABLE)

  g_object_class_install_property(gobject_class, PROP_VALUE,      STD_P_SPEC("value",  "value of the parameter"));
  g_object_class_install_property(gobject_class, PROP_PRIOR_MEAN, STD_P_SPEC("prior-mean", "mean of prior distribution"));
  g_object_class_install_property(gobject_class, PROP_PRIOR_STDDEV, STD_P_SPEC("prior-stddev", "standard deviation of prior distribution"));

  model_class->iterator_fill   = parameter_iterator_fill;
  model_class->iterator_init   = parameter_iterator_init;
}

static void
hos_parameter_set_property (GObject         *object,
			    guint            prop_id,
			    const GValue    *value,
			    GParamSpec      *pspec)
{
  HosParameter *par = HOS_PARAMETER(object);

  switch (prop_id)
    {
    case PROP_VALUE:
      par->value = g_value_get_double(value);
      break;
    case PROP_PRIOR_MEAN:
      par->prior_mean = g_value_get_double(value);
      break;
    case PROP_PRIOR_STDDEV:
      par->prior_stddev = g_value_get_double(value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_parameter_get_property (GObject         *object,
			    guint            prop_id,
			    GValue          *value,
			    GParamSpec      *pspec)
{
  HosParameter *par = HOS_PARAMETER(object);

  switch (prop_id)
    {
    case PROP_VALUE:
      g_value_set_double (value, par->value);
      break;
    case PROP_PRIOR_MEAN:
      g_value_set_double (value, par->prior_mean);
      break;
    case PROP_PRIOR_STDDEV:
      g_value_set_double (value, par->prior_stddev);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
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

static void
parameter_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  self->np = 1;
}

HosParameter*
parameter_new(gdouble value, gdouble prior_mean, gdouble prior_stddev)
{
  HosParameter *result = g_object_new(HOS_TYPE_PARAMETER, NULL);

  result->value        = value;
  result->prior_mean   = prior_mean;
  result->prior_stddev = prior_stddev;
}

