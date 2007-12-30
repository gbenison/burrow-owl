/*
 *  Copyright (C) 2005, 2006, 2007 Greg Benison
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
#include <glib-object.h>

#ifdef _VERBOSE
#include <stdio.h>
#endif

#include <assert.h>
#include <math.h>
#include "marshal.h"
#include "cursor.h"
#include <burrow/spectrum.h>

#define CLICK_RADIUS 5

enum {
  PROP_0,
  PROP_POSITION
};

enum {
  MOVED,
  DROPPED,
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

static void hos_cursor_set_property (GObject         *object,
				     guint            prop_id,
				     const GValue    *value,
				     GParamSpec      *pspec);
static void hos_cursor_get_property (GObject         *object,
				     guint            prop_id,
				     GValue          *value,
				     GParamSpec      *pspec);

static void       cursor_paint                  (HosOrnament *self, HosCanvas *canvas);
static GdkRegion* cursor_calculate_region       (HosOrnament *self);
static void       cursor_release_method         (HosOrnament *self);
static void       cursor_move_relative          (HosOrnament *self, gdouble dx, gdouble dy);
static void       cursor_adjustment_value_changed (GtkAdjustment *adjustment,
						   HosCursor *cursor);

G_DEFINE_TYPE (HosCursor, hos_cursor, HOS_TYPE_ORNAMENT)

static void
hos_cursor_class_init (HosCursorClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  
  gobject_class->set_property = hos_cursor_set_property;
  gobject_class->get_property = hos_cursor_get_property;

  ornament_class->paint = cursor_paint;
  ornament_class->calculate_region = cursor_calculate_region;
  ornament_class->move_relative = cursor_move_relative;
  ornament_class->release = cursor_release_method;

  g_object_class_install_property (gobject_class,
                                   PROP_POSITION,
                                   g_param_spec_double ("Position",
							"Position",
							"coordinate (ppm)",
							-G_MAXDOUBLE,
							G_MAXDOUBLE,
							0.0,
							G_PARAM_READWRITE));

  signals[DROPPED] =
    g_signal_new("dropped",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCursorClass, dropped),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__DOUBLE,
		 G_TYPE_NONE, 1,
		 G_TYPE_DOUBLE);

  signals[MOVED] =
    g_signal_new("moved",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCursorClass, moved),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__DOUBLE,
		 G_TYPE_NONE, 1,
		 G_TYPE_DOUBLE);


}

static void
hos_cursor_init(HosCursor  *cursor)
{
}

static void
hos_cursor_set_property (GObject         *object,
			 guint            prop_id,
			 const GValue    *value,
			 GParamSpec      *pspec)
{
  HosCursor *cursor = HOS_CURSOR(object);

  cursor=cursor; /* to eliminate warning */

  switch (prop_id)
    {
    case PROP_POSITION:
      /* FIXME */
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_cursor_get_property (GObject         *object,
			 guint            prop_id,
			 GValue          *value,
			 GParamSpec      *pspec)
{
  HosCursor *cursor = HOS_CURSOR(object);

  cursor=cursor; /* to eliminate warning */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
cursor_paint(HosOrnament *self, HosCanvas *canvas)
{

  g_return_if_fail(HOS_IS_CURSOR(self));
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(GTK_WIDGET_DRAWABLE(canvas));

  HosCursor *cursor = HOS_CURSOR(self);
  GtkWidget *widget = GTK_WIDGET(canvas);
  GdkGC *gc = canvas->gc;

  gdouble pos = cursor_get_position(cursor);

  {
    GdkColor color = {0, 0xFFFF, 0xFFFF, 0xFFFF};
    gdk_gc_set_rgb_fg_color(gc, &color);
  }
  gdk_gc_set_line_attributes(gc,
			     1, /* width */
			     GDK_LINE_SOLID,
			     GDK_CAP_BUTT,
			     GDK_JOIN_MITER);

  if (cursor->orientation == VERTICAL)
    {
      canvas_world2view(canvas, &pos, NULL);
      gdk_draw_line(widget->window,
		    gc, pos, 0, pos, 1000000);
    }
  else
    {
      assert(cursor->orientation == HORIZONTAL);
      canvas_world2view(canvas, NULL, &pos);
      gdk_draw_line(widget->window,
		    gc, 0, pos, 1000000, pos);
    }
}

/*
 * x, y in world coordinates
 */
static void
cursor_move_relative(HosOrnament *self, gdouble x, gdouble y)
{
  HosCursor *cursor = HOS_CURSOR(self);
  gdouble delta = cursor->orientation == VERTICAL ? x : y;
  if (GTK_IS_ADJUSTMENT(cursor->adjustment))
    gtk_adjustment_set_value(cursor->adjustment,
			     gtk_adjustment_get_value(cursor->adjustment) +
			     delta);
}

static void
cursor_release_method(HosOrnament *self)
{
  HosCursor *cursor = HOS_CURSOR(self);
  g_signal_emit_by_name(cursor, "dropped", cursor_get_position(cursor));

  if (HOS_ORNAMENT_CLASS(hos_cursor_parent_class)->release)
    (HOS_ORNAMENT_CLASS(hos_cursor_parent_class)->release)(self);

}

void
cursor_set_orientation(HosCursor *cursor, guint orientation)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  if (orientation != cursor->orientation)
    {
      cursor->orientation = orientation;
      ornament_configure(HOS_ORNAMENT(cursor));
    }
}

GtkAdjustment*
cursor_get_adjustment(HosCursor *cursor)
{
  if (!HOS_IS_CURSOR(cursor))
    return NULL;

  return cursor->adjustment;
}

void
cursor_set_adjustment(HosCursor *cursor, GtkAdjustment *adjustment)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));

  if (cursor->adjustment != adjustment)
    {

      if (cursor->adjustment)
        {
	  g_signal_handlers_disconnect_by_func (cursor->adjustment,
						cursor_adjustment_value_changed,
						cursor);
	  g_object_unref (cursor->adjustment);
        }
      cursor->adjustment = adjustment;
      if (adjustment)
        {
	  g_object_ref (adjustment);
	  gtk_object_sink (GTK_OBJECT (adjustment));
	  g_signal_connect (adjustment, "value_changed",
			    G_CALLBACK (cursor_adjustment_value_changed),
			    cursor);
	  /* cursor_adjustment_value_changed(adjustment, cursor); */
        }
      ornament_configure(HOS_ORNAMENT(cursor));
    }
}

/*
 * Callback triggered when a cursor's adjustment changes value.
 */
static void
cursor_adjustment_value_changed(GtkAdjustment *adjustment, HosCursor *cursor)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  ornament_configure(HOS_ORNAMENT(cursor));
  g_signal_emit_by_name(cursor, "moved", gtk_adjustment_get_value(adjustment));
}

static GdkRegion*
cursor_calculate_region(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_CURSOR(self));
  HosCursor *cursor = HOS_CURSOR(self);

  GtkAdjustment *adjustment = cursor->adjustment;
  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  if (GTK_IS_ADJUSTMENT(adjustment))
    {

      /* recalculate the update region */
      GdkRectangle rect;
      gdouble pos = gtk_adjustment_get_value(adjustment);
      if (cursor->orientation == VERTICAL)
	{
	  canvas_world2view(canvas, &pos, NULL);
	  rect.x = pos - (CLICK_RADIUS / 2);
	  rect.width = CLICK_RADIUS;
	  rect.y = 0;
	  rect.height = G_MAXINT;
	}
      else /* HORIZONTAL */
	{
	  canvas_world2view(canvas, NULL, &pos);
	  rect.y = pos - (CLICK_RADIUS / 2);
	  rect.height = CLICK_RADIUS;
	  rect.x = 0;
	  rect.width = G_MAXINT;
	}

      return(gdk_region_rectangle(&rect));
    }
  else
    return gdk_region_new();
}

void
cursor_set_pos(HosCursor *cursor, gdouble position)
{

  g_return_if_fail(HOS_IS_CURSOR(cursor));
  g_return_if_fail(GTK_IS_ADJUSTMENT(cursor->adjustment));

  gtk_adjustment_set_value(cursor->adjustment, position);

}

gdouble
cursor_get_position(HosCursor *cursor)
{

  if (!HOS_IS_CURSOR(cursor)) return 0;
  if (!GTK_IS_ADJUSTMENT(cursor->adjustment)) return 0;

  return gtk_adjustment_get_value(cursor->adjustment);
}

HosCursor*
canvas_add_cursor(HosCanvas *canvas, guint orientation)
{
  HosCursor* result = g_object_new(HOS_TYPE_CURSOR, NULL);

  cursor_set_orientation(result, orientation);

  cursor_set_adjustment(result,
			orientation == VERTICAL ?
			adjustment_for_canvas_x(canvas) :
			adjustment_for_canvas_y(canvas));

  canvas_add_item(canvas, HOS_CANVAS_ITEM(result));

  return result;

}

HosCursor*
canvas_add_cursor_horizontal(HosCanvas *canvas)
{
  return canvas_add_cursor(canvas, HORIZONTAL);
}

HosCursor*
canvas_add_cursor_vertical(HosCanvas *canvas)
{
  return canvas_add_cursor(canvas, VERTICAL);
}



