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

/**
 * @defgroup HosContour
 * @brief    parameters for contour plots
 *
 * The parameters needed to represent a ::HosSpectrum as a contour plot are
 * encapsulated in a HosContour object: threshold, level multiplier factor,
 * number of levels.
 *
 * Parent Class
 * - GObject
 *
 * Subclasses
 * - ::HosContourColor
 *
 * @{
 */

enum contour_signals {
  CONFIGURATION_CHANGED,  /**< contour parameters have changed */
  LAST_SIGNAL
};

enum contour_properties {
  PROP_0,
  PROP_THRESHOLD,        /**< level of the lowest contour */
  PROP_FACTOR,           /**< multiplier between contour levels */
  PROP_NLVL,             /**< number of contour levels */
  PROP_DRAW_NEGATIVE     /**< if true: draw positive and negative contours */
};

static void    contour_configuration_changed (HosContour *contour);
static gdouble contour_get_threshold         (HosContour* contour);

static guint signals[LAST_SIGNAL] = { 0 };

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

  signals[CONFIGURATION_CHANGED] =
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
     g_param_spec_double ("threshold",
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

  /* set some reasonable default contouring parameters */
  self->threshold        = 6.0;
  self->factor           = 1.2;
  self->number_of_levels = 20;

  contour_configure(self);
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
      contour_configure(HOS_CONTOUR(object));
      break;
    case PROP_FACTOR:
      HOS_CONTOUR(object)->factor = g_value_get_double(value);
      contour_configure(HOS_CONTOUR(object));
      break;
    case PROP_THRESHOLD:
      HOS_CONTOUR(object)->threshold = g_value_get_double(value);
      contour_configure(HOS_CONTOUR(object));
      break;
    case PROP_DRAW_NEGATIVE:
      contour_set_draw_negative(HOS_CONTOUR(object), g_value_get_boolean(value));
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

/*
 * 'configuration-changed' signal is emitted upon any change
 * in this contour's drawing parameters (n-lvl, thres, etc.)
 */
static void
contour_configuration_changed (HosContour *self)
{
  guint n_lvl, n_contours, index;

  g_return_if_fail(HOS_IS_CONTOUR(self));

  n_lvl = self->number_of_levels;
  if (n_lvl <= 0)
    return;

  n_contours = self->draw_negative ? n_lvl * 2 : n_lvl;
  self->levels = (gdouble*)g_realloc(self->levels, n_contours * sizeof(gdouble));

  if (self->draw_negative)
    {
      index = n_lvl - 1;
      self->levels[index] = -contour_get_threshold(self);
      for (; index > 0; --index)
	self->levels[index - 1] = self->levels[index] * self->factor;
    }

  index = self->draw_negative ? n_lvl : 0;
  self->levels[index] = contour_get_threshold(self);
  
  for (; index < n_contours - 1; index++)
    self->levels[index + 1] = self->levels[index] * self->factor;

}

void
contour_set_draw_negative(HosContour *self, gboolean draw_negative)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (draw_negative == self->draw_negative)
    return;
  self->draw_negative = draw_negative;

  contour_configure(self);
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

/**
 * @brief signal contour parameter re-configuration
 *
 * Signal that the configuration of 'self' has changed,
 * indicating that any visualizations of it need to be redrawn
 */
void
contour_configure(HosContour* self)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  g_signal_emit(self, signals[CONFIGURATION_CHANGED], 0);
}

/**
 * @}
 */
