/*
 *  Copyright (C) 2005, 2006 Greg Benison
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
#include "painter_gdk.h"

enum {
  PROP_0
};

static void hos_painter_gdk_set_property (GObject         *object,
					  guint            prop_id,
					  const GValue    *value,
					  GParamSpec      *pspec);
static void hos_painter_gdk_get_property (GObject         *object,
					  guint            prop_id,
					  GValue          *value,
					  GParamSpec      *pspec);

static void gdk_trace_line(HosPainterGdk*, struct hos_point*, const gint, gint, gboolean);

G_DEFINE_TYPE(HosPainterGdk, hos_painter_gdk, HOS_TYPE_PAINTER)

static void
hos_painter_gdk_class_init (HosPainterGdkClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  HosPainterClass *painter_class = HOS_PAINTER_CLASS(klass);
  
  gobject_class->set_property = hos_painter_gdk_set_property;
  gobject_class->get_property = hos_painter_gdk_get_property;

  painter_class->trace_line = (trace_func)gdk_trace_line;
  
}

static void
hos_painter_gdk_init(HosPainterGdk  *painter)
{
}

static void
hos_painter_gdk_set_property (GObject         *object,
			       guint            prop_id,
			       const GValue    *value,
			       GParamSpec      *pspec)
{
  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_painter_gdk_get_property (GObject         *object,
			      guint            prop_id,
			      GValue          *value,
			      GParamSpec      *pspec)
{
  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
painter_gdk_set_drawable_gc(HosPainterGdk *self, GdkDrawable *drawable, GdkGC *gc)
{
  self->drawable = drawable;
  self->gc = gc;
}

static void
gdk_trace_line(HosPainterGdk *self, struct hos_point* points, const gint n_point, gint lvl, gboolean closed)
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

  /* set color */
  {
    GdkColor *color = contour_get_color(painter->contour, lvl);

    /* FIXME adjustable linewidth, etc. */
    gdk_gc_set_rgb_fg_color(self->gc, color);
    gdk_gc_set_line_attributes(self->gc,
			       1, /* width */
			       GDK_LINE_SOLID,
			       GDK_CAP_BUTT,
			       GDK_JOIN_MITER);

  }

  {
    GdkPoint gdk_points[n_point];
    guint i;

    /*
     * Points must be copied from a glib-style array to a
     * C-style array for use in gdk_draw_lines.
     */
    for (i = 0; i < n_point; ++i)
      {
	gdk_points[i].x = points[i].x;
	gdk_points[i].y = points[i].y;
      }

    if (closed)
      gdk_draw_polygon(self->drawable, self->gc, FALSE, gdk_points, n_point);
    else
      gdk_draw_lines(self->drawable, self->gc, gdk_points, n_point);
  }
}






