/*
 *  Copyright (C) 2005 Greg Benison
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
#include <math.h>
#include <glib-object.h>

#ifdef _VERBOSE
#include <stdio.h>
#endif

#include <assert.h>
#include "marshal.h"
#include "marker.h"
#include <burrow/spectrum.h>

/* #define _VERBOSE 1 */

enum {
  PROP_0,
  PROP_X,
  PROP_Y
};

enum {
  GRABBED,
  MOVED,
  DROPPED,
  LAST_SIGNAL
};

static guint marker_signals[LAST_SIGNAL] = { 0 };

static void hos_marker_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_marker_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);


static void marker_paint(HosOrnament *self, HosCanvas *canvas);
static void marker_set_pos_method(HosOrnament *self, gdouble x, gdouble y);
static void marker_acquire_method(HosOrnament *self);
static void marker_motion_event(HosOrnament *self, gdouble x, gdouble y);
static void marker_release_method(HosOrnament *self);

static GdkRegion* marker_calculate_region(HosOrnament *self);
static void marker_adjustment_value_changed(GtkAdjustment *adjustment, HosMarker *marker);

G_DEFINE_TYPE (HosMarker, hos_marker, HOS_TYPE_ORNAMENT)

static void
hos_marker_class_init (HosMarkerClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  
  hos_marker_parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_marker_set_property;
  gobject_class->get_property = hos_marker_get_property;

  ornament_class->paint = marker_paint;
  ornament_class->calculate_region = marker_calculate_region;
  ornament_class->acquire = marker_acquire_method;
  ornament_class->release = marker_release_method;
  ornament_class->motion_event = marker_motion_event;
  
  g_object_class_install_property (gobject_class,
                                   PROP_X,
                                   g_param_spec_double ("X",
							"X",
							"X coordinate (ppm)",
							-G_MAXDOUBLE,
							G_MAXDOUBLE,
							0.0,
							G_PARAM_READWRITE));

  marker_signals[GRABBED] =
    g_signal_new("grabbed",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosMarkerClass, grabbed),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__UINT,
		 G_TYPE_NONE, 1,
		 G_TYPE_UINT);
  
  marker_signals[DROPPED] =
    g_signal_new("dropped",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosMarkerClass, grabbed),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE,
		 G_TYPE_NONE, 2,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);

  marker_signals[MOVED] =
    g_signal_new("moved",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosMarkerClass, grabbed),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE,
		 G_TYPE_NONE, 2,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);


}

gboolean
marker_get_pos(HosMarker *self, gdouble *x, gdouble *y)
{
  g_return_if_fail(HOS_IS_MARKER(self));
  if (x)
    {
      if (GTK_IS_ADJUSTMENT(self->adjustment_x))
	*x = gtk_adjustment_get_value(self->adjustment_x);
      else
	return FALSE;
    }
  if (y)
    {
      if (GTK_IS_ADJUSTMENT(self->adjustment_y))
	*y = gtk_adjustment_get_value(self->adjustment_y);
      else
	return FALSE;
    }
  return TRUE;
}

static void
marker_paint(HosOrnament *self, HosCanvas *canvas)
{

  g_return_if_fail(HOS_IS_MARKER(self));
  g_return_if_fail(HOS_IS_CANVAS(canvas));

  HosMarker *marker = HOS_MARKER(self);
  GtkWidget *widget = GTK_WIDGET(canvas);
  gdouble x;
  gdouble y;
  GdkGC *gc;

  marker_get_pos(marker, &x, &y);

  if (!(GTK_WIDGET_DRAWABLE(widget)))
    return;

  gc = canvas->gc;
  {
    GdkColor color = {0, 0xFFFF, 0xFFFF, 0xFFFF};
    gdk_gc_set_rgb_fg_color(gc, &color);
  }
  gdk_gc_set_line_attributes(gc,
			     2, /* width */
			     GDK_LINE_SOLID,
			     GDK_CAP_BUTT,
			     GDK_JOIN_MITER);

  canvas_world2view(canvas, &x, &y);

  gdk_draw_line(widget->window,
		gc,
		x - marker->size,
		y,
		x + marker->size,
		y);

  gdk_draw_line(widget->window,
		gc,
		x,
		y - marker->size,
		x,
		y + marker->size);

}

static void
marker_set_pos_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosMarker *marker = HOS_MARKER(self);
  gtk_adjustment_set_value(marker->adjustment_x, x);
  gtk_adjustment_set_value(marker->adjustment_y, y);
}

static GdkRegion*
marker_calculate_region(HosOrnament *self)
{
  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;
  HosMarker *marker = HOS_MARKER(self);
  gdouble x, y;

  if (marker_get_pos(HOS_MARKER(self), &x, &y))
    {
      canvas_world2view(canvas, &x, &y);

      GdkRectangle rect;
      rect.x = x - marker->size;
      rect.y = y - marker->size;
      rect.width = marker->size * 2;
      rect.height = marker->size * 2;
      
      return (gdk_region_rectangle(&rect));
    }
  else
    return gdk_region_new();
}

static void
marker_acquire_method(HosOrnament *self)
{
  HosMarker *marker = HOS_MARKER(self);
  marker->active = TRUE;

  if (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->acquire)
    (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->acquire)(self);

}

static void
marker_motion_event(HosOrnament *self, gdouble x, gdouble y)
{
  /* HosMarker *marker = HOS_MARKER(self); */
  /* FIXME */
  /* Maybe emit a 'moved' signal?? */
  if (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->motion_event)
    (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->motion_event)(self, x, y);
  g_signal_emit_by_name(self, "moved", x, y);

}

static void
marker_release_method(HosOrnament *self)
{
  gdouble x, y;
  HosMarker *marker = HOS_MARKER(self);

  marker->active = FALSE;
  marker_get_pos(marker, &x, &y);
  g_signal_emit_by_name(marker, "dropped", x, y);

  if (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->release)
    (HOS_ORNAMENT_CLASS(hos_marker_parent_class)->release)(self);

}


static void
hos_marker_init(HosMarker *marker)
{
  /* Set a reasonable default size */
  marker->size = 10;
  /* Set a reasonable default style */
  marker->style = MARKER_CROSS;

  marker->movable = TRUE;
}

static void
hos_marker_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  HosMarker *marker = HOS_MARKER(object);

  marker=marker; /* to eliminate warning */

  switch (prop_id)
    {
    case PROP_X:
      /* FIXME */
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_marker_get_property (GObject         *object,
			  guint            prop_id,
			  GValue          *value,
			  GParamSpec      *pspec)
{
  HosMarker *marker = HOS_MARKER(object);

  marker=marker; /* to eliminate warning */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
marker_set_size(HosMarker *marker, guint size)
{
  g_return_if_fail(HOS_IS_MARKER(marker));
  marker->size = size;
}

void
marker_set_movable(HosMarker *marker, gboolean movable)
{
  g_return_if_fail(HOS_IS_MARKER(marker));
  marker->movable = movable;
}

void
marker_set_adjustments(HosMarker *marker, GtkAdjustment *adjustment_x, GtkAdjustment *adjustment_y)
{
  gdouble new_x;
  gdouble new_y;

  new_x = GTK_IS_ADJUSTMENT(adjustment_x) ? gtk_adjustment_get_value(adjustment_x) : 0;
  new_y = GTK_IS_ADJUSTMENT(adjustment_y) ? gtk_adjustment_get_value(adjustment_y) : 0;

  g_return_if_fail(HOS_IS_MARKER(marker));
  if (marker->adjustment_x != adjustment_x)
    {

      if (marker->adjustment_x)
        {
	  g_signal_handlers_disconnect_by_func (marker->adjustment_x,
						marker_adjustment_value_changed,
						marker);
	  g_object_unref (marker->adjustment_x);
        }
      marker->adjustment_x = adjustment_x;
      if (adjustment_x)
        {
	  g_object_ref (adjustment_x);
	  gtk_object_sink (GTK_OBJECT (adjustment_x));
	  g_signal_connect (adjustment_x, "value_changed",
			    G_CALLBACK (marker_adjustment_value_changed),
			    marker);
        }

    }
  if (marker->adjustment_y != adjustment_y)
    {

      if (marker->adjustment_y)
        {
	  g_signal_handlers_disconnect_by_func (marker->adjustment_y,
						marker_adjustment_value_changed,
						marker);
	  g_object_unref (marker->adjustment_y);
        }
      marker->adjustment_y = adjustment_y;
      if (adjustment_y)
        {
	  g_object_ref (adjustment_y);
	  gtk_object_sink (GTK_OBJECT (adjustment_y));
	  g_signal_connect (adjustment_y, "value_changed",
			    G_CALLBACK (marker_adjustment_value_changed),
			    marker);
        }
    }
  ornament_configure(HOS_ORNAMENT(marker));
}

/*
 * Create a new marker with default adjustments appropriate
 * for this canvas; add to the canvas.
 */
HosMarker*
canvas_add_marker(HosCanvas *canvas)
{
  HosSpectrum *spectrum = canvas_get_spectrum(canvas);

  HosMarker* result = g_object_new(HOS_TYPE_MARKER, NULL);
  marker_set_adjustments(result,
			 adjustment_for_canvas_x(canvas),
			 adjustment_for_canvas_y(canvas));
  canvas_add_item(canvas, HOS_CANVAS_ITEM(result));

  return result;
}

GtkAdjustment*
marker_get_x_adjustment(HosMarker *marker)
{
  return marker->adjustment_x;
}

GtkAdjustment*
marker_get_y_adjustment(HosMarker *marker)
{
  return marker->adjustment_y;
}

static void
marker_adjustment_value_changed(GtkAdjustment *adjustment, HosMarker *marker)
{
  gdouble x, y;

  if (!marker_get_pos(marker, &x, &y))
    return;

  ornament_configure(HOS_ORNAMENT(marker));
  g_signal_emit_by_name(marker, "moved", x, y);

}



