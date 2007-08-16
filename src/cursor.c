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
  GRABBED,
  MOVED,
  DROPPED,
  LAST_SIGNAL
};

static GObjectClass *parent_class = NULL;
static guint signals[LAST_SIGNAL] = { 0 };


static void hos_cursor_init(HosCursor  *cursor);
static void hos_cursor_class_init (HosCursorClass *klass);
static void hos_cursor_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_cursor_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);

static void cursor_paint_method(HosOrnament *self);
static void cursor_set_pos_method(HosOrnament *self, gdouble x, gdouble y);
static gboolean cursor_overlap_region_method(HosOrnament *self,
					     gdouble x1,
					     gdouble y1,
					     gdouble xn,
					     gdouble yn);
static gboolean cursor_point_overlap_method(HosOrnament *self,
					    gdouble x_ppm, gdouble y_ppm);
static void cursor_acquire_method(HosOrnament *self);
static void cursor_motion_event_method(HosOrnament *self, gdouble x, gdouble y);
static void cursor_release_method(HosOrnament *self);

static void cursor_value_changed(GtkAdjustment *adjustment, HosCursor *cursor);
static void cursor_sync_region(HosOrnament *cursor);

static HosCursor* canvas_add_cursor(HosCanvas *canvas, guint orientation);

GType
hos_cursor_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosCursorClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_cursor_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosCursor),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_cursor_init,
      };

      type = g_type_register_static (HOS_TYPE_ORNAMENT,
				     "HosCursor",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_cursor_class_init (HosCursorClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_cursor_set_property;
  gobject_class->get_property = hos_cursor_get_property;

  ornament_class->paint = cursor_paint_method;
  ornament_class->set_pos = cursor_set_pos_method;
  ornament_class->overlap_region = cursor_overlap_region_method;
  ornament_class->point_overlap = cursor_point_overlap_method;
  ornament_class->acquire = cursor_acquire_method;
  ornament_class->motion_event = cursor_motion_event_method;
  ornament_class->release = cursor_release_method;
  ornament_class->sync_region = cursor_sync_region;

  g_object_class_install_property (gobject_class,
                                   PROP_POSITION,
                                   g_param_spec_double ("Position",
							"Position",
							"coordinate (ppm)",
							-G_MAXDOUBLE,
							G_MAXDOUBLE,
							0.0,
							G_PARAM_READWRITE));

  signals[GRABBED] =
    g_signal_new("grabbed",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCursorClass, grabbed),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__VOID,
		 G_TYPE_NONE, 0);
  
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
  cursor->movable = TRUE;
  cursor->enabled = TRUE;
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


/** class callbacks **/

static void
cursor_paint_method(HosOrnament *self)
{

  HosCursor *cursor = HOS_CURSOR(self);
  HosCanvas *canvas;
  GtkWidget *widget;
  gdouble pos;
  GdkGC *gc;

  canvas = HOS_CANVAS(self->canvas);
  
  if (canvas == NULL)
    return;

  pos = cursor_get_position(cursor);

  widget = GTK_WIDGET(canvas);

  if (!(GTK_WIDGET_MAPPED(widget)))
    return;

  gc = canvas->gc;
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
      canvas_ppm2view(canvas, &pos, NULL);
      gdk_draw_line(widget->window,
		    gc, pos, 0, pos, 1000000);
    }
  else
    {
      assert(cursor->orientation == HORIZONTAL);
      canvas_ppm2view(canvas, NULL, &pos);
      gdk_draw_line(widget->window,
		    gc, 0, pos, 1000000, pos);
    }
}

/*
 * x, y in ppm
 */
static void
cursor_set_pos_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosCursor *cursor = HOS_CURSOR(self);
  gdouble new_pos = (cursor->orientation == VERTICAL) ? x : y;
  if (GTK_IS_ADJUSTMENT(cursor->adjustment))
    gtk_adjustment_set_value(cursor->adjustment, new_pos);
}

static gboolean
cursor_overlap_region_method(HosOrnament *self,
			     gdouble x1,
			     gdouble y1,
			     gdouble xn,
			     gdouble yn)
{
  /* FIXME */
  return TRUE;
}

static gboolean
cursor_point_overlap_method(HosOrnament *self,
			    gdouble x_ppm, gdouble y_ppm)
{
  gdouble pos_self;
  gdouble* pos_arg;
  HosCursor *cursor = HOS_CURSOR(self);
  HosCanvas *canvas = HOS_CANVAS(self->canvas);

  pos_self = cursor_get_position(cursor);

  if (cursor->movable == FALSE)
    return FALSE;

  if (cursor->orientation == VERTICAL)
    {
      pos_arg = &x_ppm;
      canvas_ppm2view(canvas, pos_arg, NULL);
      canvas_ppm2view(canvas, &pos_self, NULL);
    }
  else
    {
      assert(cursor->orientation == HORIZONTAL);
      pos_arg = &y_ppm;
      canvas_ppm2view(canvas, NULL, pos_arg);
      canvas_ppm2view(canvas, NULL, &pos_self);
    }

  if ((fabs (*pos_arg - pos_self)) > CLICK_RADIUS)
    return FALSE;

  return TRUE;

}

static void
cursor_acquire_method(HosOrnament *self)
{
  HosCursor *cursor = HOS_CURSOR(self);
  cursor->active = TRUE;

  if (HOS_ORNAMENT_CLASS(parent_class)->acquire)
    (HOS_ORNAMENT_CLASS(parent_class)->acquire)(self);

}

static void
cursor_motion_event_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosCursor *cursor = HOS_CURSOR(self);

  if (HOS_ORNAMENT_CLASS(parent_class)->motion_event)
    (HOS_ORNAMENT_CLASS(parent_class)->motion_event)(self, x, y);

  g_signal_emit_by_name(self, "moved", cursor_get_position(cursor));

}

static void
cursor_release_method(HosOrnament *self)
{
  HosCursor *cursor = HOS_CURSOR(self);

  cursor->active = FALSE;
  g_signal_emit_by_name(cursor, "dropped", cursor_get_position(cursor));

  if (HOS_ORNAMENT_CLASS(parent_class)->release)
    (HOS_ORNAMENT_CLASS(parent_class)->release)(self);

}

/** end class callbacks **/


void
cursor_set_orientation(HosCursor *cursor, guint orientation)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  cursor->orientation = orientation;
}

GtkAdjustment*
cursor_get_adjustment(HosCursor *cursor)
{
  if (!HOS_IS_CURSOR(cursor))
    return NULL;

  return cursor->adjustment;
}

void
cursor_set_movable(HosCursor *cursor, gboolean movable)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  cursor->movable = movable;
}

void
cursor_set_enabled(HosCursor *cursor, gboolean enabled)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  cursor->enabled = enabled;
  /* FIXME redraw canvas */
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
						cursor_value_changed,
						cursor);
	  g_object_unref (cursor->adjustment);
        }
      cursor->adjustment = adjustment;
      if (adjustment)
        {
	  g_object_ref (adjustment);
	  gtk_object_sink (GTK_OBJECT (adjustment));
	  g_signal_connect (adjustment, "value_changed",
			    G_CALLBACK (cursor_value_changed),
			    cursor);
	  /* cursor_value_changed(adjustment, cursor); */
	  cursor_sync_region(cursor);
        }

    }
}

/*
 * Callback triggered when a cursor's adjustment changes value.
 */
static void
cursor_value_changed(GtkAdjustment *adjustment, HosCursor *cursor)
{
  g_return_if_fail(HOS_IS_CURSOR(cursor));
  ornament_sync_region(HOS_ORNAMENT(cursor));
  g_signal_emit_by_name(cursor, "moved", gtk_adjustment_get_value(adjustment));
}

static void
cursor_sync_region(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_CURSOR(self));
  HosCursor *cursor = HOS_CURSOR(self);
  GtkAdjustment *adjustment = cursor->adjustment;
  HosCanvas *canvas = self->canvas;

  if (GTK_IS_ADJUSTMENT(adjustment))
    {
      ornament_invalidate_region(self);

      /* recalculate the update region */
      GdkRectangle rect;
      gdouble pos = gtk_adjustment_get_value(adjustment);
      if (cursor->orientation == VERTICAL)
	{
	  canvas_ppm2view(canvas, &pos, NULL);
	  rect.x = pos - (CLICK_RADIUS / 2);
	  rect.width = CLICK_RADIUS;
	  rect.y = 0;
	  rect.height = G_MAXINT;
	}
      else /* HORIZONTAL */
	{
	  canvas_ppm2view(canvas, NULL, &pos);
	  rect.y = pos - (CLICK_RADIUS / 2);
	  rect.height = CLICK_RADIUS;
	  rect.x = 0;
	  rect.width = G_MAXINT;
	}

      ornament_set_region(self, gdk_region_rectangle(&rect));
      ornament_invalidate_region(self);
    }
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

static HosCursor*
canvas_add_cursor(HosCanvas *canvas, guint orientation)
{
  HosSpectrum *spectrum = canvas_get_spectrum(canvas);

  HosCursor* result = g_object_new(HOS_TYPE_CURSOR, NULL);
  cursor_set_orientation(result, orientation);
  if (spectrum != NULL)
    cursor_set_adjustment(result,
			  adjustment_for_spectrum(spectrum,
						  (orientation == VERTICAL) ? 0 : 1));

  canvas_add_ornament(canvas, HOS_ORNAMENT(result));

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



