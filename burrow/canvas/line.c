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

#include "line.h"
// #include <string.h>

/**
 * @defgroup HosLine
 * @brief A continuous curve drawn on a #HosCanvas
 *
 * A @HosLine draws an arbitrary curve on a #HosCanvas.
 * The curve is determined by an ordered set of (x, y) data points
 * which can be set all at once with #line_set_points,
 * or built up incrementally with #line_append_point.
 *
 * @{
 */

/* signals & properties */
enum {
  ENTER,
  LEAVE,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_COLOR,
  PROP_WIDTH
};

static guint line_signals[LAST_SIGNAL] = { 0 };

static void hos_line_set_property   (GObject         *object,
				     guint            prop_id,
				     const GValue    *value,
				     GParamSpec      *pspec);
static void hos_line_get_property   (GObject         *object,
				     guint            prop_id,
				     GValue          *value,
				     GParamSpec      *pspec);

static void     line_expose                 (HosCanvasItem *self, GdkEventExpose *event);
static void     line_set_canvas             (HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas);
static void     line_configure              (HosCanvasItem *canvas_item);
static gboolean line_point_in               (HosLine* self, gint x, gint y);
static gboolean line_canvas_motion_notify   (GtkWidget *widget, GdkEventMotion *event, HosLine* self);
static gboolean line_canvas_configure       (GtkWidget *widget, GdkEventConfigure *event, HosLine *self);
static void     line_canvas_realize         (GtkWidget *widget, HosLine* self);
static void     line_canvas_world_configure (HosCanvasItem *self,
					     HosCanvas     *canvas);

static void     line_paint_default          (HosLine* line, HosCanvas *canvas);
static void     line_set_color              (HosLine* line, GdkColor *color);


G_DEFINE_TYPE (HosLine, hos_line, HOS_TYPE_CANVAS_ITEM)

static void
hos_line_init(HosLine *self)
{
  self->points = g_array_new(FALSE, FALSE, sizeof(hos_line_point_t));
  self->width  = 1.0;
  self->color  = g_new0(GdkColor, 1);
  self->color->red   = 0xffff;
  self->color->blue  = 0xffff;
  self->color->green = 0xffff;
}

static void
hos_line_class_init (HosLineClass *klass)
{
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*)klass;
  
  gobject_class->set_property = hos_line_set_property;
  gobject_class->get_property = hos_line_get_property;

  canvas_item_class->expose     = line_expose;
  canvas_item_class->set_canvas = line_set_canvas;
  canvas_item_class->configure  = line_configure;
  canvas_item_class->canvas_world_configure = line_canvas_world_configure;

  klass->paint      = line_paint_default;

  g_object_class_install_property (gobject_class,
				   PROP_COLOR,
				   g_param_spec_boxed ("color",
						       "color",
						       "foreground color of the line",
						       GDK_TYPE_COLOR,
						       G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
				   PROP_WIDTH,
				   g_param_spec_double ("width",
							"Width",
							"line width in pixels",
							0,
							20,
							1,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  line_signals[ENTER] =
    g_signal_new ("enter",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosLineClass, enter),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  line_signals[LEAVE] =
    g_signal_new ("leave",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosLineClass, leave),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

}

static void
hos_line_set_property   (GObject         *object,
			 guint            prop_id,
			 const GValue    *value,
			 GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_COLOR:
      line_set_color(HOS_LINE(object), (GdkColor*)(g_value_get_boxed(value)));
      break;
    case PROP_WIDTH:
      HOS_LINE(object)->width = g_value_get_double(value);
      canvas_item_configure(HOS_CANVAS_ITEM(object));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_line_get_property   (GObject         *object,
			 guint            prop_id,
			 GValue          *value,
			 GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_WIDTH:
      g_value_set_double(value, HOS_LINE(object)->width);
      break;
    case PROP_COLOR:
      g_value_set_boxed(value, HOS_LINE(object)->color);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


static void
line_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  g_return_if_fail(HOS_IS_LINE(self));
  HosLine* line = HOS_LINE(self);
  HosLineClass* class = HOS_LINE_GET_CLASS(self);

  if (class->paint)
    class->paint(line, self->canvas);
}

static void
line_set_canvas(HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas)
{
  if (old_canvas)
    {
      g_signal_handlers_disconnect_matched (old_canvas,
					    G_SIGNAL_MATCH_DATA,
					    0,      /* id */
					    0,      /* detail */
					    NULL,   /* closure */
					    NULL,   /* func */
					    self);  /* data */
    }

  if (canvas)
    {
      g_signal_connect (canvas, "motion-notify-event",
			G_CALLBACK (line_canvas_motion_notify),
			self);
      g_signal_connect (canvas, "configure-event",
			G_CALLBACK (line_canvas_configure),
			self);
      g_signal_connect (canvas, "realize",
			G_CALLBACK (line_canvas_realize),
			self);
    }

}

/*
 * What to do when the line's configuration (the data, the line style etc.)
 * has changed.
 */
static void
line_configure(HosCanvasItem *canvas_item)
{
  g_return_if_fail(HOS_IS_LINE(canvas_item));
  HosLine *line = HOS_LINE(canvas_item);

  if (canvas_item->canvas)
    {
      /*
       * FIXME
       * As for ornaments, it may be slightly more efficient to calculate a union
       * of old and new 'dirty areas' for this 'line' object, then queue a redraw
       * only for that region.
       */
      gtk_widget_queue_draw(GTK_WIDGET(canvas_item->canvas));
    }
}

/*
 * Are pixel coordinates (x, y) 'within' the sensitive region
 * of this line?
 */
static gboolean
line_point_in(HosLine* self, gint x, gint y)
{
  HosLineClass *class = HOS_LINE_GET_CLASS(self);
  return (class->point_in) ? class->point_in(self, x, y) : FALSE;
}

/*
 * Connected to the canvas 'motion-notify' signal; called when the pointer
 * moves over the canvas.
 */
static gboolean
line_canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event, HosLine* self)
{
  GdkModifierType state;
  gboolean mouse_over = line_point_in(self, event->x, event->y);

  if (mouse_over == self->mouse_over)
    return FALSE;
  else
    {
      self->mouse_over = mouse_over;
      if (mouse_over)
	g_signal_emit(self, line_signals[ENTER], 0);
      else
	g_signal_emit(self, line_signals[LEAVE], 0);
    }

  return FALSE;
}

static gboolean
line_canvas_configure(GtkWidget *widget, GdkEventConfigure *event, HosLine *self)
{
  g_return_val_if_fail(HOS_IS_CANVAS(widget), FALSE);
  g_return_val_if_fail(HOS_IS_LINE(self), FALSE);

  /* FIXME */

  return FALSE;
}

static void
line_canvas_realize(GtkWidget *widget, HosLine* self)
{
  g_return_if_fail(HOS_IS_CANVAS(widget));
  g_return_if_fail(HOS_IS_LINE(self));

  /* FIXME */
}

static void
line_canvas_world_configure(HosCanvasItem *self, HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(HOS_IS_LINE(self));

  HosCanvasItemClass *parent_class =
    HOS_CANVAS_ITEM_CLASS(hos_line_parent_class);
  
  if (parent_class->canvas_world_configure)
    (parent_class->canvas_world_configure)(self, canvas);
}

static void
line_paint_default(HosLine* line, HosCanvas *canvas)
{
  cairo_t* cr = canvas_get_cairo_context(canvas);

  /* FIXME customize line style */
  cairo_set_line_width(cr, line->width);
  gdk_cairo_set_source_color(cr, line->color);

  if (line->points->len > 0)
    {
      hos_line_point_t point = g_array_index(line->points, hos_line_point_t, 0);
      double x = point.x;
      double y = point.y;
      canvas_world2view(canvas, &x, &y);
      cairo_new_path(cr);
      cairo_move_to(cr, x, y);
      int i;
      for (i = 1; i < line->points->len; ++i)
	{
	  hos_line_point_t point = g_array_index(line->points, hos_line_point_t, i);
	  double x = point.x;
	  double y = point.y;
	  canvas_world2view(canvas, &x, &y);
	  cairo_line_to(cr, x, y);
	}
      cairo_stroke(cr);
    }
}

/**
 * @brief replace the data points with a new set
 *
 * Replace the points defining #line with 
 * a new set of points determined by the arrays 'x' and 'y',
 * which both are of length 'np'.
 */
void
line_set_points(HosLine* line, double* x, double* y, guint np)
{
  g_return_if_fail(HOS_IS_LINE(line));

  g_array_set_size(line->points, 0);

  int i;
  for (i = 0; i < np; ++i)
    {
      hos_line_point_t point = {x[i], y[i]};
      g_array_append_val(line->points, point);
      /* FIXME calculate bounds here */
    }
  canvas_item_configure(HOS_CANVAS_ITEM(line));
}

/**
 * @brief    append a single point
 * @returns  number of points including the new one
 *
 * Append point #x, #y to the set of data points
 * determining the shape of #line.
 *
 */
guint
line_append_point(HosLine* line, double x, double y)
{
  g_return_if_fail(HOS_IS_LINE(line));

  hos_line_point_t point = {x, y};
  g_array_append_val(line->points, point);

  /* FIXME calculate bounds here */
  canvas_item_configure(HOS_CANVAS_ITEM(line));

  return line->points->len;
}

static void
line_set_color(HosLine* line, GdkColor *color)
{
  g_return_if_fail(HOS_IS_LINE(line));
  g_return_if_fail(color != NULL);

  line->color->red = color->red;
  line->color->blue = color->blue;
  line->color->green = color->green;

  canvas_item_configure(HOS_CANVAS_ITEM(line));
}

/** @} */
