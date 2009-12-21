/*
 *  Copyright (C) 2005-2009 Greg Benison
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

#define CLICK_RADIUS 5

/**
 * @defgroup  HosCursor
 * @brief     horizontal or vertical line ornaments
 *
 * Cursors are horizontal or vertical movable bars on a HosCanvas that are
 * used to mark a position in one dimension.  The cursor position is tied
 * to a GtkAdjustment.
 *
 * Parent Class
 * - ::HosOrnament
 *
 * @{
 */

enum cursor_properties {
  PROP_0,
  PROP_POSITION,    /**< the position in world coordinates */
  PROP_ADJUSTMENT,  /**< the GtkAdjustment tied to the position */
  PROP_ORIENTATION  /**< orientation: horizontal or vertical */
};

enum cursor_signals {
  MOVED,        /**< emitted when the position changes */
  DROPPED,      /**< emitted when the cursor loses mouse focus */
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
                                   g_param_spec_double ("position",
							"Position",
							"coordinate (world)",
							-G_MAXDOUBLE,
							G_MAXDOUBLE,
							0.0,
							G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
				   PROP_ADJUSTMENT,
				   g_param_spec_object ("adjustment",
							"Adjustment",
							"adjustment object controlling the cursor's position",
							GTK_TYPE_ADJUSTMENT,
							G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class,
				   PROP_ORIENTATION,
				   g_param_spec_enum ("orientation",
						      "Orientation",
						      "horizontal or vertical",
						      HOS_TYPE_ORIENTATION_TYPE,
						      HOS_HORIZONTAL,
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
  switch (prop_id)
    {
    case PROP_POSITION:
      gtk_adjustment_set_value(HOS_CURSOR(object)->adjustment,
			       g_value_get_double(value));
      break;
    case PROP_ADJUSTMENT:
      cursor_set_adjustment(HOS_CURSOR(object), GTK_ADJUSTMENT(g_value_get_object(value)));
      break;
    case PROP_ORIENTATION:
      cursor_set_orientation(HOS_CURSOR(object), g_value_get_enum(value));
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
  switch (prop_id)
    {
    case PROP_POSITION:
      g_value_set_double(value, cursor_get_position(HOS_CURSOR(object)));
      break;
    case PROP_ADJUSTMENT:
      g_value_set_object(value, G_OBJECT(cursor_get_adjustment(HOS_CURSOR(object))));
      break;
    case PROP_ORIENTATION:
      g_value_set_enum(value, HOS_CURSOR(object)->orientation);
      break;
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

  if (cursor->orientation == HOS_VERTICAL)
    {
      canvas_world2view(canvas, &pos, NULL);
      gdk_draw_line(widget->window,
		    gc, pos, 0, pos, 1000000);
    }
  else
    {
      assert(cursor->orientation == HOS_HORIZONTAL);
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
  gdouble delta = cursor->orientation == HOS_VERTICAL ? x : y;
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

/**
 * @brief Add a cursor to a HosCanvas
 */
HosCursor*
canvas_add_cursor(HosCanvas *canvas, HosOrientationType orientation)
{
  HosCursor* result = g_object_new(HOS_TYPE_CURSOR, NULL);

  cursor_set_orientation(result, orientation);

  cursor_set_adjustment(result,
			orientation == HOS_VERTICAL ?
			adjustment_for_canvas_x(canvas) :
			adjustment_for_canvas_y(canvas));

  canvas_add_item(canvas, HOS_CANVAS_ITEM(result));

  return result;

}

/**
 * @}
 */

void
cursor_set_orientation(HosCursor *cursor, HosOrientationType orientation)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  if (orientation != cursor->orientation)
    {
      cursor->orientation = orientation;
      canvas_item_configure(HOS_CANVAS_ITEM(cursor));
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
      canvas_item_configure(HOS_CANVAS_ITEM(cursor));
    }
}

/*
 * Callback triggered when a cursor's adjustment changes value.
 */
static void
cursor_adjustment_value_changed(GtkAdjustment *adjustment, HosCursor *cursor)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  canvas_item_configure(HOS_CANVAS_ITEM(cursor));
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
      if (cursor->orientation == HOS_VERTICAL)
	{
	  canvas_world2view(canvas, &pos, NULL);
	  rect.x = pos - (CLICK_RADIUS / 2);
	  rect.width = CLICK_RADIUS;
	  rect.y = 0;
	  rect.height = G_MAXINT;
	}
      else /* HOS_HORIZONTAL */
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




