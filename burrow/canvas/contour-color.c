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

#include "contour-color.h"

enum contour_color_properties {
  PROP_0,
  PROP_COLOR_NEGATIVE_LOW  /**< GdkColor Color of the lowest negative contour */
};

static void    contour_color_configuration_changed (HosContour *self);

static void    hos_contour_color_set_property (GObject      *object,
					       guint         prop_id,
					       const GValue *value,
					       GParamSpec   *pspec);

static void    hos_contour_color_get_property (GObject      *object,
					       guint         prop_id,
					       GValue       *value,
					       GParamSpec   *pspec);


G_DEFINE_TYPE (HosContourColor, hos_contour_color, HOS_TYPE_CONTOUR)

static void
hos_contour_color_class_init (HosContourColorClass *klass)
{
  GObjectClass     *gobject_class = G_OBJECT_CLASS (klass);
  HosContourClass  *contour_class = HOS_CONTOUR_CLASS (klass);

  hos_contour_color_parent_class = g_type_class_peek_parent (klass);

  gobject_class->get_property = hos_contour_color_get_property;
  gobject_class->set_property = hos_contour_color_set_property;

  contour_class->configuration_changed = contour_color_configuration_changed;

  g_object_class_install_property
    (gobject_class,
     PROP_COLOR_NEGATIVE_LOW,
     g_param_spec_boxed ("color-negative-low",
			 "color-negative-low",
			 "Color of the lowest negative contour",
			 GDK_TYPE_COLOR,
			 G_PARAM_READWRITE));


}

static void
contour_color_set_default_colors(HosContourColor *self)
{
}

static void
hos_contour_color_init (HosContourColor *self)
{
  /* FIXME set default colors */
  contour_color_set_default_colors(self);
}

static void
hos_contour_color_get_property (GObject      *object,
				guint         prop_id,
				GValue       *value,
				GParamSpec   *pspec)
{
  switch (prop_id)
    {
    case PROP_COLOR_NEGATIVE_LOW:
      g_value_set_boxed(value, HOS_CONTOUR_COLOR(object)->color_negative_low);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_contour_color_set_property (GObject      *object,
				guint         prop_id,
				const GValue *value,
				GParamSpec   *pspec)
{
  switch (prop_id)
    {
    case PROP_COLOR_NEGATIVE_LOW:
      HOS_CONTOUR_COLOR(object)->color_negative_low = gdk_color_copy(g_value_get_boxed(value));
      contour_configure(HOS_CONTOUR(object));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
contour_color_configuration_changed (HosContour *self)
{
  HOS_CONTOUR_CLASS(hos_contour_color_parent_class)->configuration_changed(self);

  HosContourColor *contour_color = HOS_CONTOUR_COLOR(self);
  /* FIXME sync colors 


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

     guint i;
     for (i = 0; i < nlvl; ++i)
{
}
   */
}

static GdkColor default_contour_color = {0, 0xaaaa, 0x9999, 0xaaaa};

/**
 * @brief query 'self' for color of contour level 'lvl'
 *
 * Return the GdkColor corresponding to contour level 'lvl' of 'self'.
 * Colors for positive contours are interpolated between
 * COLOR_POSITIVE_LOW and COLOR_POSITIVE_HIGH; similarly for negative levels.
 *
 * If 'self' is not a color contour, return a default color.
 */
GdkColor*
contour_get_color(HosContour *self, gint lvl)
{
  if (!HOS_IS_CONTOUR_COLOR(self))
    return &default_contour_color;
  else
    {
      /* FIXME */
      return &default_contour_color;
    }
}

