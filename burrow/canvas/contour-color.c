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
  PROP_COLOR_NEGATIVE_LOW,   /**< GdkColor Color of the lowest negative contour  */
  PROP_COLOR_NEGATIVE_HIGH,  /**< GdkColor Color of the highest negative contour */
  PROP_COLOR_POSITIVE_LOW,   /**< GdkColor Color of the lowest positive contour  */
  PROP_COLOR_POSITIVE_HIGH   /**< GdkColor Color of the highest positive contour */
};

#define CONTOUR_COLOR_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_CONTOUR_COLOR, HosContourColorPrivate))
#define CONTOUR_COLOR_PRIVATE(o, field) ((CONTOUR_COLOR_GET_PRIVATE(o))->field)
typedef struct _HosContourColorPrivate HosContourColorPrivate;
struct _HosContourColorPrivate
{
  GArray *colors;
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

  g_object_class_install_property
    (gobject_class,
     PROP_COLOR_NEGATIVE_HIGH,
     g_param_spec_boxed ("color-negative-high",
			 "color-negative-high",
			 "Color of the highest negative contour",
			 GDK_TYPE_COLOR,
			 G_PARAM_READWRITE));

  g_object_class_install_property
    (gobject_class,
     PROP_COLOR_POSITIVE_LOW,
     g_param_spec_boxed ("color-positive-low",
			 "color-positive-low",
			 "Color of the lowest positive contour",
			 GDK_TYPE_COLOR,
			 G_PARAM_READWRITE));

  g_object_class_install_property
    (gobject_class,
     PROP_COLOR_POSITIVE_HIGH,
     g_param_spec_boxed ("color-positive-high",
			 "color-positive-high",
			 "Color of the highest positive contour",
			 GDK_TYPE_COLOR,
			 G_PARAM_READWRITE));


  g_type_class_add_private(gobject_class, sizeof(HosContourColorPrivate));

}

static void
contour_color_set_default_colors(HosContourColor *self)
{
  static GdkColor default_positive_low  = {0, 0x800,  0x0, 0x6000};
  static GdkColor default_positive_high = {0, 0x8000, 0x0, 0x8000};

  static GdkColor default_negative_low  = {0, 0x4000, 0x800,  0x0};
  static GdkColor default_negative_high = {0, 0x8000, 0x6000, 0x0};

  self->color_negative_low  = gdk_color_copy(&default_negative_low);
  self->color_negative_high = gdk_color_copy(&default_negative_high);

  self->color_positive_low  = gdk_color_copy(&default_positive_low);
  self->color_positive_high = gdk_color_copy(&default_positive_high);
}

static void
hos_contour_color_init (HosContourColor *self)
{
  contour_color_set_default_colors(self);
  CONTOUR_COLOR_PRIVATE(self, colors) = g_array_new(TRUE, TRUE, sizeof(GdkColor));
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
    case PROP_COLOR_NEGATIVE_HIGH:
      g_value_set_boxed(value, HOS_CONTOUR_COLOR(object)->color_negative_high);
      break;
    case PROP_COLOR_POSITIVE_LOW:
      g_value_set_boxed(value, HOS_CONTOUR_COLOR(object)->color_positive_low);
      break;
    case PROP_COLOR_POSITIVE_HIGH:
      g_value_set_boxed(value, HOS_CONTOUR_COLOR(object)->color_positive_high);
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
    case PROP_COLOR_NEGATIVE_HIGH:
      HOS_CONTOUR_COLOR(object)->color_negative_high = gdk_color_copy(g_value_get_boxed(value));
      contour_configure(HOS_CONTOUR(object));
      break;
    case PROP_COLOR_POSITIVE_LOW:
      HOS_CONTOUR_COLOR(object)->color_positive_low = gdk_color_copy(g_value_get_boxed(value));
      contour_configure(HOS_CONTOUR(object));
      break;
    case PROP_COLOR_POSITIVE_HIGH:
      HOS_CONTOUR_COLOR(object)->color_positive_high = gdk_color_copy(g_value_get_boxed(value));
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
  GArray *colors = CONTOUR_COLOR_PRIVATE(self, colors);

  guint n_lvl = self->number_of_levels;
  guint n_contours = self->draw_negative ? n_lvl * 2 : n_lvl;
  guint index;

  /* ensure color array is allocated */
  g_array_set_size(colors, self->draw_negative ? n_lvl * 2 : n_lvl);

  /* interpolate negative colors */
  if (self->draw_negative)
    {
      gdouble delta_red   = (1.0 * contour_color->color_negative_high->red
			     - contour_color->color_negative_low->red)   / n_lvl;
      gdouble delta_blue  = (1.0 * contour_color->color_negative_high->blue
			     - contour_color->color_negative_low->blue)  / n_lvl;
      gdouble delta_green = (1.0 * contour_color->color_negative_high->green
			     - contour_color->color_negative_low->green) / n_lvl;

      index = n_lvl - 1;

#define COLOR_SLOT(idx) g_array_index(colors, GdkColor, idx)

      COLOR_SLOT(index).red   = contour_color->color_negative_low->red;
      COLOR_SLOT(index).blue  = contour_color->color_negative_low->blue;
      COLOR_SLOT(index).green = contour_color->color_negative_low->green;

      for (; index > 0; --index)
	{
	  COLOR_SLOT(index - 1).red    = COLOR_SLOT(index).red + delta_red;
	  COLOR_SLOT(index - 1).blue   = COLOR_SLOT(index).blue + delta_blue;
	  COLOR_SLOT(index - 1).green  = COLOR_SLOT(index).green + delta_green;
	}

    }
  
  /* interpolate positive colors */
  {
      gdouble delta_red   = (1.0 * contour_color->color_positive_high->red
			     - contour_color->color_positive_low->red)   / n_lvl;
      gdouble delta_blue  = (1.0 * contour_color->color_positive_high->blue
			     - contour_color->color_positive_low->blue)  / n_lvl;
      gdouble delta_green = (1.0 * contour_color->color_positive_high->green
			     - contour_color->color_positive_low->green) / n_lvl;

      index = self->draw_negative ? n_lvl : 0;

      COLOR_SLOT(index).red   = contour_color->color_positive_low->red;
      COLOR_SLOT(index).blue  = contour_color->color_positive_low->blue;
      COLOR_SLOT(index).green = contour_color->color_positive_low->green;

    for (; index < n_contours - 1; index++)
      {
	COLOR_SLOT(index + 1).red   = COLOR_SLOT(index).red + delta_red;
	COLOR_SLOT(index + 1).blue  = COLOR_SLOT(index).blue + delta_blue;
	COLOR_SLOT(index + 1).green = COLOR_SLOT(index).green + delta_green;
      }
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
      GArray *colors = CONTOUR_COLOR_PRIVATE(self, colors);
      if (colors == NULL)
	return &default_contour_color;

      if (colors->len <= lvl)
	return &default_contour_color;

      return &(g_array_index(colors, GdkColor, lvl));
    }
}

