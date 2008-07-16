/*
 *  Copyright (C) 2005, 2007 Greg Benison
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

#include <assert.h>
#include "hoscanvas.h"
#include "marshal.h"

#define ENSURE_ORDER_GDOUBLE(_a_, _b_) { if (_a_ > _b_) { \
                                         gdouble tmp = _a_; _a_ = _b_; _b_ = tmp; }}
enum {
  CLICKED,
  WORLD_CONFIGURE,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_X1,
  PROP_Y1,
  PROP_XN,
  PROP_YN
};

static guint canvas_signals[LAST_SIGNAL] = { 0 };

static void     hos_canvas_set_property (GObject         *object,
					 guint            prop_id,
					 const GValue    *value,
					 GParamSpec      *pspec);
static void     hos_canvas_get_property (GObject         *object,
					 guint            prop_id,
					 GValue          *value,
					 GParamSpec      *pspec);

static gboolean canvas_button_press     (GtkWidget *widget, GdkEventButton *event);
static gboolean canvas_expose_event     (GtkWidget *widget, GdkEventExpose *event);
static void     canvas_realize          (GtkWidget *widget);
static void     canvas_world_configure  (HosCanvas *self);

static gboolean canvas_is_double_buffered = TRUE;

G_DEFINE_TYPE (HosCanvas, hos_canvas, GTK_TYPE_DRAWING_AREA)

static void
hos_canvas_class_init (HosCanvasClass *klass)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;

  gobject_class = G_OBJECT_CLASS (klass);
  widget_class  = GTK_WIDGET_CLASS (klass);
  
  gobject_class->set_property = hos_canvas_set_property;
  gobject_class->get_property = hos_canvas_get_property;

  widget_class->button_press_event = canvas_button_press;
  widget_class->expose_event       = canvas_expose_event;
  widget_class->realize            = canvas_realize;

  klass->world_configure           = canvas_world_configure;

#define STD_P_SPEC(name, blurb)   g_param_spec_double (name, name, blurb, -G_MAXDOUBLE, G_MAXDOUBLE, 0, G_PARAM_READABLE | G_PARAM_WRITABLE)

  g_object_class_install_property(gobject_class, PROP_X1, STD_P_SPEC("x1", "X left limit"));
  g_object_class_install_property(gobject_class, PROP_Y1, STD_P_SPEC("y1", "Y lower limit"));

  g_object_class_install_property(gobject_class, PROP_XN, STD_P_SPEC("xn", "X right limit"));
  g_object_class_install_property(gobject_class, PROP_YN, STD_P_SPEC("yn", "Y upper limit"));

  canvas_signals[CLICKED] =
    g_signal_new("clicked",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCanvasClass, clicked),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE,
		 G_TYPE_NONE, 2,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);

  canvas_signals[WORLD_CONFIGURE] =
    g_signal_new("world-configure",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCanvasClass, world_configure),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__VOID,
		 G_TYPE_NONE, 0);

}

static void
hos_canvas_init(HosCanvas  *canvas)
{
  GtkWidget *widget = GTK_WIDGET(canvas);

  gtk_widget_add_events(widget,
			GDK_BUTTON_PRESS_MASK |
			GDK_BUTTON_RELEASE_MASK |
			GDK_POINTER_MOTION_MASK |
			GDK_POINTER_MOTION_HINT_MASK);

  gtk_widget_set_double_buffered(widget, canvas_is_double_buffered);

  {
    GdkColor bg_color;

    bg_color.red = 0xF;
    bg_color.blue = 0xF;
    bg_color.green = 0xF;

    gdk_colormap_alloc_color(gdk_colormap_get_system(),
			     &bg_color,
			     FALSE, TRUE);

    gtk_widget_modify_bg(widget, GTK_STATE_NORMAL, &bg_color);
  }

  canvas_set_world(canvas, 0, 0, 100, 100);

}

/* callback for button press events on canvas widget */
static gboolean
canvas_button_press(GtkWidget *widget, GdkEventButton *event)
{
  HosCanvas *canvas = HOS_CANVAS(widget);
  GList *ptr;
  gdouble x, y;
  gulong grab_id = 0;

  g_assert(HOS_IS_CANVAS(widget));

  x = event->x;
  y = event->y;
  canvas_view2world(canvas, &x, &y);

  g_signal_emit(canvas,
		canvas_signals[CLICKED],
		0, x, y);

  if(GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event(widget, event);
  else
    return FALSE;

}

static void
canvas_world_configure(HosCanvas *self)
{
  g_return_if_fail(HOS_IS_CANVAS(self));
  gtk_widget_queue_draw(GTK_WIDGET(self));
}

static gboolean
canvas_expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  HosCanvas *canvas = HOS_CANVAS(widget);

  /* chain up */
  if (GTK_WIDGET_CLASS(hos_canvas_parent_class)->expose_event)
    GTK_WIDGET_CLASS(hos_canvas_parent_class)->expose_event(widget, event);

  if (canvas_is_double_buffered == FALSE)
    gdk_window_clear_area(widget->window,
			  event->area.x,
			  event->area.y,
			  event->area.width,
			  event->area.height);

  GList* ptr;
  for (ptr = canvas->items; ptr != NULL; ptr = ptr->next)
    canvas_item_expose(HOS_CANVAS_ITEM(ptr->data), event);

  return FALSE;
}

static void
canvas_realize(GtkWidget *widget)
{
  HosCanvas *canvas = HOS_CANVAS(widget);

  (GTK_WIDGET_CLASS(hos_canvas_parent_class))->realize(widget);

  canvas->gc = gdk_gc_new(widget->window);

}

static void
hos_canvas_set_property (GObject         *object,
			 guint            prop_id,
			 const GValue    *value,
			 GParamSpec      *pspec)
{
  HosCanvas *canvas = HOS_CANVAS(object);

  switch (prop_id)
    {
    case PROP_X1:
      canvas_set_world(canvas, g_value_get_double(value), canvas->y1, canvas->xn, canvas->yn);
      break;
    case PROP_Y1:
      canvas_set_world(canvas, canvas->x1, g_value_get_double(value), canvas->xn, canvas->yn);
      break;
    case PROP_XN:
      canvas_set_world(canvas, canvas->x1, canvas->y1, g_value_get_double(value), canvas->yn);
      break;
    case PROP_YN:
      canvas_set_world(canvas, canvas->x1, canvas->y1, canvas->xn, g_value_get_double(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_canvas_get_property (GObject         *object,
			 guint            prop_id,
			 GValue          *value,
			 GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_X1:
      g_value_set_double(value, HOS_CANVAS(object)->x1);
      break;
    case PROP_Y1:
      g_value_set_double(value, HOS_CANVAS(object)->y1);
      break;
    case PROP_XN:
      g_value_set_double(value, HOS_CANVAS(object)->xn);
      break;
    case PROP_YN:
      g_value_set_double(value, HOS_CANVAS(object)->yn);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

HosCanvasItem*
canvas_add_item(HosCanvas *self, HosCanvasItem *canvasitem)
{
  g_return_val_if_fail(HOS_IS_CANVAS(self), NULL);
  g_return_val_if_fail(HOS_IS_CANVAS_ITEM(canvasitem), NULL);

  canvas_item_set_canvas(canvasitem, self);

  if (!g_list_find(self->items, canvasitem))
    {
      g_object_ref(canvasitem);
      self->items = g_list_append(self->items, canvasitem);
    }

  return canvasitem;
}

/*
 * returns: canvas item number 'idx' from 'self,
 * or NULL if idx out of range
 */
HosCanvasItem*
canvas_get_item
(HosCanvas *self, guint idx)
{
  g_return_val_if_fail(HOS_IS_CANVAS(self), NULL);
  return (idx < g_list_length(self->items)) ? HOS_CANVAS_ITEM(g_list_nth_data(self->items, idx)) : NULL;
}

void
canvas_invalidate_region(HosCanvas *canvas, GdkRegion *region)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  if (GTK_WIDGET_DRAWABLE(canvas))
    gdk_window_invalidate_region(GTK_WIDGET(canvas)->window, region, TRUE);
}

void
canvas_view2world(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));

  if (GTK_WIDGET_REALIZED(canvas))
    {
      gint window_width, window_height;
      gdk_window_get_size(GTK_WIDGET(canvas)->window,
			  &window_width, &window_height);
      
      if (x != NULL)
	*x = (*x / window_width)
	  * (canvas->xn - canvas->x1) + canvas->x1;
      if (y != NULL)
	*y = (*y / window_height)
	  * (canvas->yn - canvas->y1) + canvas->y1;
    }
  else
    {
      if (x != NULL) *x = 0;
      if (y != NULL) *y = 0;
    }
}

void
canvas_world2view(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));

  if (GTK_WIDGET_REALIZED(canvas))
    {
      gint window_width, window_height;
      gdk_window_get_size(GTK_WIDGET(canvas)->window,
			  &window_width, &window_height);
      
      if (x != NULL)
	*x = ((*x - canvas->x1) / (canvas->xn - canvas->x1))
	  * window_width;
      
      if (y != NULL)
	*y = ((*y - canvas->y1) / (canvas->yn - canvas->y1))
	  * window_height;
    }
  else
    {
      if (x != NULL) *x = 0;
      if (y != NULL) *y = 0;
    }

}

void
canvas_set_world(HosCanvas *canvas, gdouble x1, gdouble y1, gdouble xn, gdouble yn)
{
  canvas->x1 = x1;
  canvas->y1 = y1;
  canvas->xn = xn;
  canvas->yn = yn;
  g_signal_emit(canvas, canvas_signals[WORLD_CONFIGURE], 0);
  gtk_widget_queue_draw(GTK_WIDGET(canvas));
}

cairo_t*
canvas_get_cairo_context (HosCanvas *canvas)
{
  g_return_val_if_fail(GTK_WIDGET_DRAWABLE(canvas), NULL);
  return gdk_cairo_create(GTK_WIDGET(canvas)->window);
}

GtkAdjustment*
adjustment_for_canvas_x(HosCanvas* canvas)
{
  gdouble min = canvas->x1;
  gdouble max = canvas->xn;

  ENSURE_ORDER_GDOUBLE(min, max);

  return GTK_ADJUSTMENT(gtk_adjustment_new((min + max) / 2.0, min, max,
					   (max - min) / 2000.0,
					   0, 0));
}

GtkAdjustment*
adjustment_for_canvas_y(HosCanvas* canvas)
{
  gdouble min = canvas->y1;
  gdouble max = canvas->yn;

  ENSURE_ORDER_GDOUBLE(min, max);

  return GTK_ADJUSTMENT(gtk_adjustment_new((min + max) / 2.0, min, max,
					   (max - min) / 2000.0,
					   0, 0));
}

GdkColor*
gdk_rgb(gdouble red, gdouble green, gdouble blue)
{
  GdkColor *result = g_new0(GdkColor, 1);
  result->red   = CLAMP(red, 0, 1)   * G_MAXUINT16;
  result->blue  = CLAMP(blue, 0, 1)  * G_MAXUINT16;
  result->green = CLAMP(green, 0, 1) * G_MAXUINT16;

  return result;
}
