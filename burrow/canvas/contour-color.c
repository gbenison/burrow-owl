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
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->get_property = hos_contour_color_get_property;
  gobject_class->set_property = hos_contour_color_set_property;

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

