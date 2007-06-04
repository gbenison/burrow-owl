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
#include "hosspectrum.h"

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

static GObjectClass *parent_class = NULL;
static guint marker_signals[LAST_SIGNAL] = { 0 };


static void hos_marker_init(HosMarker  *marker);
static void hos_marker_class_init (HosMarkerClass *klass);
static void hos_marker_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_marker_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);


static void marker_paint_method(HosOrnament *self);
static void marker_set_pos_method(HosOrnament *self, gdouble x, gdouble y);
static gboolean marker_overlap_region_method(HosOrnament *self,
					     gdouble x1, gdouble y1,
					     gdouble xn, gdouble yn);
static gboolean marker_point_overlap_method(HosOrnament *self,
					    gdouble x_ppm, gdouble y_ppm);
static void marker_acquire_method(HosOrnament *self);
static void marker_motion_event_method(HosOrnament *self, gdouble x, gdouble y);
static void marker_release_method(HosOrnament *self);

static void marker_sync_region(HosOrnament *self);
static void marker_value_changed(GtkAdjustment *adjustment, HosMarker *marker);

GType
hos_marker_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosMarkerClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_marker_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosMarker),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_marker_init,
      };

      type = g_type_register_static (HOS_TYPE_ORNAMENT,
				     "HosMarker",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_marker_class_init (HosMarkerClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_marker_set_property;
  gobject_class->get_property = hos_marker_get_property;

  ornament_class->paint = marker_paint_method;
  ornament_class->set_pos = marker_set_pos_method;
  ornament_class->overlap_region = marker_overlap_region_method;
  ornament_class->point_overlap = marker_point_overlap_method;
  ornament_class->acquire = marker_acquire_method;
  ornament_class->motion_event = marker_motion_event_method;
  ornament_class->release = marker_release_method;
  ornament_class->sync_region = marker_sync_region;

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
marker_paint_method(HosOrnament *self)
{

  HosMarker *marker = HOS_MARKER(self);
  HosCanvas *canvas;
  GtkWidget *widget;
  gdouble x;
  gdouble y;
  GdkGC *gc;

  canvas = HOS_CANVAS(self->canvas);
  
  if (canvas == NULL)
    return;

  marker_get_pos(marker, &x, &y);

  widget = GTK_WIDGET(canvas);

  if (!(GTK_WIDGET_MAPPED(widget)))
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

  canvas_ppm2view(canvas, &x, &y);

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

void
marker_set_pos(HosMarker *self, gdouble x, gdouble y)
{
  marker_set_pos_method(HOS_ORNAMENT(self), x, y);
}

static void
marker_set_pos_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosMarker *marker = HOS_MARKER(self);
  gtk_adjustment_set_value(marker->adjustment_x, x);
  gtk_adjustment_set_value(marker->adjustment_y, y);
}

static gboolean
marker_overlap_region_method(HosOrnament *self,
			     gdouble x1,
			     gdouble y1,
			     gdouble xn,
			     gdouble yn)
{
  /* FIXME */
  return TRUE;
}

static gboolean
marker_point_overlap_method(HosOrnament *self,
			    gdouble x_ppm, gdouble y_ppm)
{
  gdouble x_self, y_self;
  HosMarker *marker = HOS_MARKER(self);
  HosCanvas *canvas = HOS_CANVAS(self->canvas);

  if (marker->movable == FALSE)
    return FALSE;

  marker_get_pos(marker, &x_self, &y_self);

  canvas_ppm2view(canvas, &x_ppm, &y_ppm);
  canvas_ppm2view(canvas, &x_self, &y_self);

  if (((fabs (x_self - x_ppm)) > marker->size)
      || ((fabs (y_self - y_ppm)) > marker->size))
    return FALSE;

  return TRUE;

}

static void
marker_sync_region(HosOrnament *self)
{
  HosCanvas *canvas = self->canvas;
  HosMarker *marker = HOS_MARKER(self);
  gdouble x, y;

  if (marker_get_pos(HOS_MARKER(self), &x, &y))
    {
      ornament_invalidate_region(self);

      canvas_ppm2view(canvas, &x, &y);

      GdkRectangle rect;
      rect.x = x - marker->size;
      rect.y = y - marker->size;
      rect.width = marker->size * 2;
      rect.height = marker->size * 2;
      
      ornament_set_region(self, gdk_region_rectangle(&rect));
      ornament_invalidate_region(self);
    }
}

static void
marker_acquire_method(HosOrnament *self)
{
  HosMarker *marker = HOS_MARKER(self);
  marker->active = TRUE;

  if (HOS_ORNAMENT_CLASS(parent_class)->acquire)
    (HOS_ORNAMENT_CLASS(parent_class)->acquire)(self);

}

static void
marker_motion_event_method(HosOrnament *self, gdouble x, gdouble y)
{
  /* HosMarker *marker = HOS_MARKER(self); */
  /* FIXME */
  /* Maybe emit a 'moved' signal?? */
  if (HOS_ORNAMENT_CLASS(parent_class)->motion_event)
    (HOS_ORNAMENT_CLASS(parent_class)->motion_event)(self, x, y);
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

  if (HOS_ORNAMENT_CLASS(parent_class)->release)
    (HOS_ORNAMENT_CLASS(parent_class)->release)(self);

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
						marker_value_changed,
						marker);
	  g_object_unref (marker->adjustment_x);
        }
      marker->adjustment_x = adjustment_x;
      if (adjustment_x)
        {
	  g_object_ref (adjustment_x);
	  gtk_object_sink (GTK_OBJECT (adjustment_x));
	  g_signal_connect (adjustment_x, "value_changed",
			    G_CALLBACK (marker_value_changed),
			    marker);
        }

    }
  if (marker->adjustment_y != adjustment_y)
    {

      if (marker->adjustment_y)
        {
	  g_signal_handlers_disconnect_by_func (marker->adjustment_y,
						marker_value_changed,
						marker);
	  g_object_unref (marker->adjustment_y);
        }
      marker->adjustment_y = adjustment_y;
      if (adjustment_y)
        {
	  g_object_ref (adjustment_y);
	  gtk_object_sink (GTK_OBJECT (adjustment_y));
	  g_signal_connect (adjustment_y, "value_changed",
			    G_CALLBACK (marker_value_changed),
			    marker);
        }
    }
  ornament_sync_region(HOS_ORNAMENT(marker));
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
  if (spectrum != NULL)
    marker_set_adjustments(result,
			   adjustment_for_spectrum(spectrum, 0),
			   adjustment_for_spectrum(spectrum, 1));
  canvas_add_ornament(canvas, HOS_ORNAMENT(result));

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
marker_value_changed(GtkAdjustment *adjustment, HosMarker *marker)
{
  gdouble x, y;

  if (!marker_get_pos(marker, &x, &y))
    return;

  ornament_sync_region(HOS_ORNAMENT(marker));
  g_signal_emit_by_name(marker, "moved", x, y);

}



