/*
 *  Copyright (C) 2006, 2008 Greg Benison
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

#include <math.h>
#include "contour.h"

enum {
  CONFIGURATION_CHANGED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_THRESHOLD,
  PROP_FACTOR,
  PROP_NLVL,
  PROP_DRAW_NEGATIVE
};

static void contour_configuration_changed (HosContour *contour);
static void sync_params(HosContour* self);
static void hos_contour_class_init(HosContourClass *klass);
static void hos_contour_init(HosContour *contour);

static gdouble contour_get_threshold(HosContour* contour);

static guint contour_signals[LAST_SIGNAL] = { 0 };

static void hos_contour_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_contour_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);

G_DEFINE_TYPE (HosContour, hos_contour, G_TYPE_OBJECT)

static void
hos_contour_class_init(HosContourClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  klass->configuration_changed = contour_configuration_changed;

  gobject_class->set_property = hos_contour_set_property;
  gobject_class->get_property = hos_contour_get_property;

  contour_signals[CONFIGURATION_CHANGED] =
    g_signal_new ("configuration-changed",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET(HosContourClass, configuration_changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  g_object_class_install_property
    (gobject_class,
     PROP_THRESHOLD,
     g_param_spec_double ("thres",
			  "Threshold",
			  "Level of lowest contour line",
			  0,
			  G_MAXDOUBLE,
			  0.0,
			  G_PARAM_READWRITE));

  g_object_class_install_property
    (gobject_class,
     PROP_FACTOR,
     g_param_spec_double ("factor",
			  "Factor",
			  "ratio between successive contour levels",
			  1.0,
			  G_MAXDOUBLE,
			  1.2,
			  G_PARAM_READWRITE));

  g_object_class_install_property
    (gobject_class,
     PROP_NLVL,
     g_param_spec_uint ("nlvl",
			"nlvl",
			"number of contour levels",
			0,
			G_MAXINT,
			20,
			G_PARAM_READWRITE));

  g_object_class_install_property
    (gobject_class,
     PROP_DRAW_NEGATIVE,
     g_param_spec_boolean ("draw-negative",
			   "draw-negative",
			   "true: include both positive and negative levels",
			   FALSE,
			   G_PARAM_READWRITE));
}

static void
hos_contour_init(HosContour *self)
{
  self->levels = NULL;
  self->lines = NULL;

  /* set some reasonable default contouring parameters */
  self->threshold        = 6.0;
  self->factor           = 1.2;
  self->number_of_levels = 20;

  /* set some default pleasing hues */
  contour_set_color_positive(self, 5000, 65000,  0, 0,  40000, 60000);
  contour_set_color_negative(self, 30000, 60000, 5000, 40000, 0, 0);

  sync_params(self);
}

static void
hos_contour_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_NLVL:
      HOS_CONTOUR(object)->number_of_levels = g_value_get_uint(value);
      sync_params(HOS_CONTOUR(object));
      break;
    case PROP_FACTOR:
      HOS_CONTOUR(object)->factor = g_value_get_double(value);
      sync_params(HOS_CONTOUR(object));
      break;
    case PROP_THRESHOLD:
      HOS_CONTOUR(object)->threshold = g_value_get_double(value);
      sync_params(HOS_CONTOUR(object));
      break;
    case PROP_DRAW_NEGATIVE:
      HOS_CONTOUR(object)->draw_negative = g_value_get_boolean(value);
      sync_params(HOS_CONTOUR(object));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_contour_get_property (GObject         *object,
			  guint            prop_id,
			  GValue          *value,
			  GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_NLVL:
      g_value_set_uint(value, HOS_CONTOUR(object)->number_of_levels);
      break;
    case PROP_THRESHOLD:
      g_value_set_double(value, HOS_CONTOUR(object)->threshold);
      break;
    case PROP_FACTOR:
      g_value_set_double(value, HOS_CONTOUR(object)->factor);
      break;
    case PROP_DRAW_NEGATIVE:
      g_value_set_boolean(value, HOS_CONTOUR(object)->draw_negative);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
contour_set_color_positive(HosContour* self, guint16 red_min,   guint16 red_max,
                                              guint16 green_min, guint16 green_max,
                                              guint16 blue_min,  guint16 blue_max)
{
  self->red_min_pos    =  red_min;
  self->red_max_pos 	  =  red_max;
  self->blue_min_pos	  =  blue_min;
  self->blue_max_pos	  =  blue_max;
  self->green_min_pos  =  green_min;
  self->green_max_pos  =  green_max;

  sync_params(self);
}

void
contour_set_color_negative(HosContour* self, guint16 red_min,   guint16 red_max,
                                              guint16 green_min, guint16 green_max,
                                              guint16 blue_min,  guint16 blue_max)
{
  self->red_min_neg    =  red_min;
  self->red_max_neg 	  =  red_max;
  self->blue_min_neg	  =  blue_min;
  self->blue_max_neg	  =  blue_max;
  self->green_min_neg  =  green_min;
  self->green_max_neg  =  green_max;

  sync_params(self);
}

/*
 * 'configuration-changed' signal is emitted upon any change
 * in this contour's drawing parameters (n-lvl, thres, etc.)
 */
static void
contour_configuration_changed (HosContour *contour)
{
}

/*
 * Make contour drawing parameters internally consistent;
 * usually called after a parameter changes.
 * e.g. if the contour threshold changes, then all the levels
 * must be recalculated.
 *
 * The 'configuration-changed' signal is emitted after updates are complete.
 */
static void
sync_params(HosContour* self)
{
  int n_contours, index;
  guint n_lvl;

  g_return_if_fail(HOS_IS_CONTOUR(self));

  n_lvl = self->number_of_levels;
  if (n_lvl <= 0)
    return;

  n_contours = self->draw_negative ? n_lvl * 2 : n_lvl;
  self->levels = (gdouble*)g_realloc(self->levels, n_contours * sizeof(gdouble));
  self->lines = (struct contour_linestyle_struct*)
    g_realloc(self->lines, n_contours * sizeof(struct contour_linestyle_struct));

  if (self->draw_negative)
    {
      guint16 delta_red = (self->red_max_neg - self->red_min_neg) / n_lvl;
      guint16 delta_blue = (self->blue_max_neg - self->blue_min_neg) / n_lvl;
      guint16 delta_green = (self->green_max_neg - self->green_min_neg) / n_lvl;

      index = n_lvl - 1;

      self->levels[index] = -contour_get_threshold(self);
      self->lines[index].red = self->red_min_neg;
      self->lines[index].blue = self->blue_min_neg;
      self->lines[index].green = self->green_min_neg;

      for (; index > 0; --index)
	{
	  self->levels[index - 1] = self->levels[index] * self->factor;
	  self->lines[index - 1].red = self->lines[index].red + delta_red;
	  self->lines[index - 1].blue = self->lines[index].blue + delta_blue;
	  self->lines[index - 1].green = self->lines[index].green + delta_green;
	}
    }

  {

    guint16 delta_red = (self->red_max_pos - self->red_min_pos) / n_lvl;
    guint16 delta_blue = (self->blue_max_pos - self->blue_min_pos) / n_lvl;
    guint16 delta_green = (self->green_max_pos - self->green_min_pos) / n_lvl;
    
    index = self->draw_negative ? n_lvl : 0;

    self->levels[index] = contour_get_threshold(self);
    self->lines[index].red = self->red_min_pos;
    self->lines[index].blue = self->blue_min_pos;
    self->lines[index].green = self->green_min_pos;

    for (; index < n_contours - 1; index++)
      {
	self->levels[index + 1] = self->levels[index] * self->factor;
	self->lines[index + 1].red = self->lines[index].red + delta_red;
	self->lines[index + 1].blue = self->lines[index].blue + delta_blue;
	self->lines[index + 1].green = self->lines[index].green + delta_green;
      }
  }
  g_signal_emit (self, contour_signals[CONFIGURATION_CHANGED], 0);
}

void
contour_set_draw_negative(HosContour *self, gboolean draw_negative)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (draw_negative == self->draw_negative)
    return;
  self->draw_negative = draw_negative;

  sync_params(self);

}

static gdouble
contour_get_threshold(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), 0);
  return (pow (10, contour->threshold));
}

gdouble*
contour_get_levels(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), NULL);
  return contour->levels;
}

guint
contour_get_n_contours(HosContour *contour)
{
  g_return_if_fail(HOS_IS_CONTOUR(contour));
  guint result = contour->number_of_levels;
  if (contour->draw_negative)
    result *= 2;
  return result;
}



