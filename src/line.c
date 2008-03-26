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

/* signals & properties */
enum {
  ENTER,
  LEAVE,
  CONFIGURE,
  LAST_SIGNAL
};

enum {
  PROP_0
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
static gboolean line_point_in               (HosLine* self, gint x, gint y);
static gboolean line_canvas_motion_notify   (GtkWidget *widget, GdkEventMotion *event, HosLine* self);
static gboolean line_canvas_configure       (GtkWidget *widget, GdkEventConfigure *event, HosLine *self);
static void     line_canvas_realize         (GtkWidget *widget, HosLine* self);
static void     line_canvas_world_configure (GtkWidget *widget, HosLine* self);


G_DEFINE_TYPE (HosLine, hos_line, HOS_TYPE_CANVAS_ITEM)

static void
hos_line_init(HosLine *self)
{
  self->points = g_array_new(FALSE, FALSE, sizeof(hos_line_point_t));
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

  line_signals[CONFIGURE] =
    g_signal_new ("configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosLineClass, configure),
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
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


static void
line_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  /* FIXME */
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
      g_signal_connect (canvas, "world-configure",
			G_CALLBACK (line_canvas_world_configure),
			self);
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
line_canvas_world_configure(GtkWidget *widget, HosLine* self)
{
  g_return_if_fail(HOS_IS_CANVAS(widget));
  g_return_if_fail(HOS_IS_LINE(self));

  /* FIXME */
}

void
line_configure(HosLine* self)
{
  g_return_if_fail(HOS_IS_LINE(self));
  g_signal_emit(self, line_signals[CONFIGURE], 0);
}

void
line_set_points   (HosLine* line, double* x, double* y, guint np)
{
  g_return_if_fail(HOS_IS_LINE(line));

  g_array_set_size(line->points, 0);

  int i;
  for (i = 0; i < np; ++i)
    {
      hos_line_point_t point = {x[i], y[i]};
      g_array_append_val(line->points, point);
    }
}

guint
line_append_point (HosLine* line, double x, double y)
{
  g_return_if_fail(HOS_IS_LINE(line));

  hos_line_point_t point = {x, y};
  g_array_append_val(line->points, point);

  return line->points->len;
}
