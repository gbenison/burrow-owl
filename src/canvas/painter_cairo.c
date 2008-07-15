/*
 *  Copyright (C) 2007 Greg Benison
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <glib-object.h>
#include "painter_cairo.h"

enum {
  PROP_0
};

static HosPainterClass *parent_class = NULL;

static void hos_painter_cairo_init(HosPainterCairo  *painter);
static void hos_painter_cairo_class_init (HosPainterCairoClass *klass);
static void hos_painter_cairo_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_painter_cairo_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);

static void cairo_trace_line(HosPainterCairo*, struct hos_point*, const gint, gint, gboolean);


GType
hos_painter_cairo_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosPainterCairoClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_painter_cairo_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosPainterCairo),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_painter_cairo_init,
      };

      type = g_type_register_static (HOS_TYPE_PAINTER,
				     "HosPainterCairo",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_painter_cairo_class_init (HosPainterCairoClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  HosPainterClass *painter_class = HOS_PAINTER_CLASS(klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_painter_cairo_set_property;
  gobject_class->get_property = hos_painter_cairo_get_property;

  painter_class->trace_line = (trace_func)cairo_trace_line;

  
/*

  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}

static void
hos_painter_cairo_init(HosPainterCairo  *painter)
{
  /* FIXME */
}

static void
hos_painter_cairo_set_property (GObject         *object,
			       guint            prop_id,
			       const GValue    *value,
			       GParamSpec      *pspec)
{
  /* HosPainterCairo *gdk = HOS_PAINTER_CAIRO(object); */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_painter_cairo_get_property (GObject         *object,
			       guint            prop_id,
			       GValue          *value,
			       GParamSpec      *pspec)
{
  HosPainterCairo *self = HOS_PAINTER_CAIRO(object);

  self=self; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_IMAGE:
      g_value_set_object (value, (GObject *)priv->image);
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
painter_cairo_set_context(HosPainterCairo *self, cairo_t *context)
{
  g_return_if_fail(HOS_IS_PAINTER_CAIRO(self));
  self->context = context;
}

static void
cairo_trace_line(HosPainterCairo *self, struct hos_point* points, const gint n_point, gint lvl, gboolean closed)
{
  HosPainter *painter = HOS_PAINTER(self);

  /*
   * The next assertion would be nice, but would require
   * either breaking into the painter's structure (grokking out
   * the adjustment) or creating a selector for the painter's
   * n_lvl; does not seem worth it to be able to do this
   * assertion.
   */
  /* assert(lvl < painter->n_lvl); */

  GdkColor color;
  color.red = CONTOUR_GET_RED(painter->contour, lvl);
  color.blue = CONTOUR_GET_BLUE(painter->contour, lvl);
  color.green = CONTOUR_GET_GREEN(painter->contour, lvl);

  gdk_cairo_set_source_color(self->context, &color);
  cairo_set_line_width(self->context, 1.0);
  cairo_new_path(self->context);

  g_assert(n_point > 0);
  cairo_move_to(self->context, points[0].x, points[0].y);
  int i;
  for (i = 1; i < n_point; ++i)
    cairo_line_to(self->context, points[i].x, points[i].y);
  if (closed)
    cairo_close_path(self->context);
  cairo_stroke(self->context);

}

/*
 * Constructor for painter type.
 */
HosPainterCairo*
painter_cairo_new(void)
{
  HosPainterCairo *result = NULL;
  result = g_object_new(HOS_TYPE_PAINTER_CAIRO, NULL);

  return result;

}





