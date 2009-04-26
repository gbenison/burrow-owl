/*
 *  Copyright (C) 2005, 2007, 2008 Greg Benison
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
#include <math.h>
#include "canvas.h"
#include "marshal.h"

#define ENSURE_ORDER_GDOUBLE(_a_, _b_) { if (_a_ > _b_) { \
                                         gdouble tmp = _a_; _a_ = _b_; _b_ = tmp; }}


/**
 * @defgroup HosCanvas
 * @brief    A GTK+ widget for displaying NMR spectra and annotations.
 *
 * The principal way to display spectra on screen using
 * burrow-owl is with the HosCanvas widget.
 * A HosCanvas displays a collection of ::HosCanvasItem objects,
 * which can be contour plots or ornaments such as labels, cursors,
 * and grids.
 * Ornaments are generally responsive to user input (e.g.
 * mouse clicks), and so can be used, together with standard
 * GTK+ widgets like buttons and scroll bars, for creating
 * interactive user interfaces.
 *
 * In addition to its 'view coordinates' defined by its size on
 * the screen, a HosCanvas has abstract 'world coordinates' which
 * can be any requested value and do not change with widget size.
 *
 * @{
 */

enum canvas_signals {
  CLICKED,          /**< Mouse has been clicked over the canvas widget. */
  WORLD_CONFIGURE,  /**< The world coordinates of the canvas have changed. */
  LAST_SIGNAL
};

enum canvas_properties {
  PROP_0,
  PROP_X1,     /**< leftmost world coordinate     */
  PROP_Y1,     /**< bottom-most world coordinate  */
  PROP_XN,     /**< rightmost world coordinate    */
  PROP_YN,     /**< topmost world coordinate      */
  PROP_ZOOM,   /**< ratio of world domain to displayed domain */
  PROP_ZOOM_ADJUSTMENT
};

static guint signals[LAST_SIGNAL] = { 0 };

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
static void     canvas_set_scroll_adjustments (HosCanvas *self,
					       GtkAdjustment *hadhjustment,
					       GtkAdjustment *vadjustment);

static void     canvas_disconnect_scroll_adjustment  (HosCanvas *self,
					       GtkAdjustment *adjustment);
static void     canvas_connect_scroll_adjustment     (HosCanvas *self,
					       GtkAdjustment *adjustment);
static gboolean canvas_scroll_adjustment_value_changed      (GtkAdjustment *adjustment,
						      gpointer data);
static gboolean canvas_zoom_adjustment_value_changed (GtkAdjustment *adjustment,
						      gpointer data);
static gdouble  world2view              (gdouble world,
					 gdouble world_min,
					 gdouble world_max,
					 gdouble zoom,
					 gdouble focus,
					 gdouble view_range);
static gdouble  view2world              (gdouble view,
					 gdouble world_min,
					 gdouble world_max,
					 gdouble zoom,
					 gdouble focus,
					 gdouble view_range);
static gdouble  calc_zoom_min           (gdouble world_min,
					 gdouble world_max,
					 gdouble zoom,
					 gdouble focus);

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
  klass->set_scroll_adjustments    = canvas_set_scroll_adjustments;

#define STD_P_SPEC(name, blurb)   g_param_spec_double (name, name, blurb, -G_MAXDOUBLE, G_MAXDOUBLE, 0, G_PARAM_READABLE | G_PARAM_WRITABLE)

  g_object_class_install_property(gobject_class, PROP_X1, STD_P_SPEC("x1", "X left limit"));
  g_object_class_install_property(gobject_class, PROP_Y1, STD_P_SPEC("y1", "Y lower limit"));

  g_object_class_install_property(gobject_class, PROP_XN, STD_P_SPEC("xn", "X right limit"));
  g_object_class_install_property(gobject_class, PROP_YN, STD_P_SPEC("yn", "Y upper limit"));

  g_object_class_install_property(gobject_class, PROP_ZOOM, STD_P_SPEC("zoom", "zoom ratio"));
  g_object_class_install_property(gobject_class,
				  PROP_ZOOM_ADJUSTMENT,
				  g_param_spec_object ("zoom-adjustment",
						       "zoom-adjustment",
						       "GtkAdjustment tied to the 'zoom' value",
						       GTK_TYPE_ADJUSTMENT,
						       G_PARAM_READABLE | G_PARAM_WRITABLE));

  signals[CLICKED] =
    g_signal_new("clicked",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCanvasClass, clicked),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE,
		 G_TYPE_NONE, 2,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);

  signals[WORLD_CONFIGURE] =
    g_signal_new("world-configure",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosCanvasClass, world_configure),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__VOID,
		 G_TYPE_NONE, 0);

  widget_class->set_scroll_adjustments_signal =
    g_signal_new ("set_scroll_adjustments",
		  G_OBJECT_CLASS_TYPE (gobject_class),
		  G_SIGNAL_RUN_LAST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET (HosCanvasClass, set_scroll_adjustments),
		  NULL, NULL,
		  g_cclosure_user_marshal_VOID__OBJECT_OBJECT,
		  G_TYPE_NONE, 2,
		  GTK_TYPE_ADJUSTMENT,
		  GTK_TYPE_ADJUSTMENT);

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

  canvas_set_world (canvas, 0, 0, 100, 100);
  canvas->zoom = 1.0;
  canvas_set_zoom_adjustment (canvas, NULL);

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
		signals[CLICKED],
		0, x, y);

  if(GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event(widget, event);
  else
    return FALSE;

}

static void
canvas_sync_scroll_adjustment(HosCanvas *canvas, GtkAdjustment *adjustment, gdouble x1, gdouble xn, gdouble focus)
{
  if (GTK_IS_ADJUSTMENT(adjustment))
    {

      adjustment->page_size = 1;
      adjustment->page_increment = 0.9;
      adjustment->step_increment = 0.1;
      adjustment->lower = 0;
      adjustment->upper = canvas->zoom;

      gdouble zoom_min = calc_zoom_min (x1, xn, canvas->zoom, focus);
      adjustment->value = (zoom_min - x1) / (xn - x1) * canvas->zoom;

      g_signal_handlers_block_by_func (adjustment, canvas_scroll_adjustment_value_changed, canvas);
      gtk_adjustment_value_changed(adjustment);
      g_signal_handlers_unblock_by_func (adjustment, canvas_scroll_adjustment_value_changed, canvas);

    }
}

static void
canvas_world_configure(HosCanvas *self)
{
  g_return_if_fail(HOS_IS_CANVAS(self));
  GtkWidget *widget = GTK_WIDGET(self);

  canvas_sync_scroll_adjustment(self, self->horiz_scroll_adjustment, self->x1, self->xn, self->x_focus);
  canvas_sync_scroll_adjustment(self, self->vert_scroll_adjustment, self->y1, self->yn, self->y_focus);

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
    case PROP_ZOOM:
      canvas_set_zoom(canvas, g_value_get_double(value));
      break;
    case PROP_ZOOM_ADJUSTMENT:
      canvas_set_zoom_adjustment(canvas, g_value_get_object(value));
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
    case PROP_ZOOM:
      g_value_set_double(value, HOS_CANVAS(object)->zoom);
      break;
    case PROP_ZOOM_ADJUSTMENT:
      g_value_set_object(value, HOS_CANVAS(object)->zoom_adjustment);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/**
 * @brief  Add an item to a canvas widget
 *
 * Append  #canvasitem to #self.
 * Returns #canvasitem.
 */
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

/**
 * @brief    retrieve a canvasitem from a canvas
 * @returns  canvas item number 'idx' from 'self, or NULL if idx out of range
 */
HosCanvasItem*
canvas_get_item
(HosCanvas *self, guint idx)
{
  g_return_val_if_fail(HOS_IS_CANVAS(self), NULL);
  return (idx < g_list_length(self->items)) ? HOS_CANVAS_ITEM(g_list_nth_data(self->items, idx)) : NULL;
}

/** @} */

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
	*x = view2world(*x, canvas->x1, canvas->xn, canvas->zoom, canvas->x_focus, window_width);
      if (y != NULL)
	*y = view2world(*y, canvas->y1, canvas->yn, canvas->zoom, canvas->y_focus, window_height);
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
	*x = world2view(*x, canvas->x1, canvas->xn, canvas->zoom, canvas->x_focus, window_width);
      
      if (y != NULL)
	*y = world2view(*y, canvas->y1, canvas->yn, canvas->zoom, canvas->y_focus, window_height);

    }
  else
    {
      if (x != NULL) *x = 0;
      if (y != NULL) *y = 0;
    }
}

static gdouble
calc_zoom_min(gdouble world_min,
	      gdouble world_max,
	      gdouble zoom,
	      gdouble focus)
{
  gdouble world_range = world_max - world_min;
  gdouble zoom_range = world_range / zoom;
  gdouble zoom_min = focus - (zoom_range / 2);

  if (fabs(world_max - zoom_min) > fabs(world_range))
    zoom_min = world_min;
  if (fabs(zoom_min + zoom_range - world_min) > fabs(world_range))
    zoom_min = world_max - zoom_range;

  return zoom_min;
}

static gdouble
world2view(gdouble world,
	   gdouble world_min,
	   gdouble world_max,
	   gdouble zoom,
	   gdouble focus,
	   gdouble view_range)
{
  gdouble world_range = world_max - world_min;
  gdouble zoom_range  = world_range / zoom;
  gdouble zoom_min    = calc_zoom_min(world_min, world_max, zoom, focus);

  return ((world - zoom_min) / zoom_range) * view_range;
}

static gdouble
view2world(gdouble view,
	   gdouble world_min,
	   gdouble world_max,
	   gdouble zoom,
	   gdouble focus,
	   gdouble view_range)
{
  gdouble world_range = world_max - world_min;
  gdouble zoom_range  = world_range / zoom;
  gdouble zoom_min    = calc_zoom_min(world_min, world_max, zoom, focus);

  return zoom_min + (view / view_range) * zoom_range;
}

/**
 * @brief change the zoom level of 'canvas'.
 */
void
canvas_set_zoom (HosCanvas *canvas, gdouble zoom)
{
  gdouble old_zoom = canvas->zoom;

  /* sanity checks */
  if (zoom < 1.0) zoom = 1.0;
  if (zoom > 1e6) zoom = 1e6;

  if (zoom != old_zoom)
    {
      canvas->zoom = zoom;
      if (GTK_IS_ADJUSTMENT(canvas->zoom_adjustment))
	gtk_adjustment_set_value (canvas->zoom_adjustment, zoom);

      g_signal_emit(canvas, signals[WORLD_CONFIGURE], 0);
    }
}

void
canvas_set_focus(HosCanvas *canvas, gdouble x, gdouble y)
{
  if ((x != canvas->x_focus) || (y != canvas->y_focus))
    {
      canvas->x_focus = x;
      canvas->y_focus = y;
      g_signal_emit(canvas, signals[WORLD_CONFIGURE], 0);
    }
}

void
canvas_set_world(HosCanvas *canvas, gdouble x1, gdouble y1, gdouble xn, gdouble yn)
{
  canvas->x1 = x1;
  canvas->y1 = y1;
  canvas->xn = xn;
  canvas->yn = yn;
  g_signal_emit(canvas, signals[WORLD_CONFIGURE], 0);
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

static void
canvas_connect_scroll_adjustment(HosCanvas *self, GtkAdjustment *adjustment)
{
  g_return_if_fail (HOS_IS_CANVAS(self));
  if (!adjustment)
    adjustment = GTK_ADJUSTMENT (gtk_adjustment_new (0.0, 0.0, 1.0, 0.1, 0.1, 0.1));
  g_return_if_fail (GTK_IS_ADJUSTMENT(adjustment));
  g_object_ref_sink (adjustment);

  g_signal_connect (adjustment, "value_changed",
		    G_CALLBACK (canvas_scroll_adjustment_value_changed),
		    self);

}

void
canvas_set_zoom_adjustment(HosCanvas *canvas, GtkAdjustment *adjustment)
{
  g_return_if_fail (HOS_IS_CANVAS(canvas));
  if (!adjustment)
    adjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 10.0, 0.1, 0.0, 0.0));
  if (adjustment != canvas->zoom_adjustment)
    {
      if (GTK_IS_ADJUSTMENT(canvas->zoom_adjustment))
	{
	  g_signal_handlers_disconnect_by_func (canvas->zoom_adjustment,
						canvas_zoom_adjustment_value_changed,
						canvas);
	  g_object_unref(canvas->zoom_adjustment);
	}
      canvas->zoom_adjustment = adjustment;

      g_signal_connect (adjustment, "value-changed",
			G_CALLBACK (canvas_zoom_adjustment_value_changed),
			canvas);
      g_object_ref_sink (adjustment);
    }
}

static gboolean
canvas_zoom_adjustment_value_changed (GtkAdjustment *adjustment, gpointer data)
{
  g_return_val_if_fail(GTK_IS_ADJUSTMENT(adjustment), FALSE);
  HosCanvas *canvas = HOS_CANVAS(data);
  canvas_set_zoom(canvas, adjustment->value);
}

static void
canvas_disconnect_scroll_adjustment(HosCanvas *self, GtkAdjustment *adjustment)
{
  g_return_if_fail(HOS_IS_CANVAS(self));
  if (adjustment)
    {
      g_return_if_fail(GTK_IS_ADJUSTMENT(adjustment));
      g_signal_handlers_disconnect_by_func (adjustment,
					    canvas_scroll_adjustment_value_changed,
					    self);
      g_object_unref(adjustment);
    }
}

static gboolean
canvas_scroll_adjustment_value_changed (GtkAdjustment *adjustment, gpointer data)
{
  HosCanvas *canvas = HOS_CANVAS(data);
  gdouble x_focus = canvas->x_focus;
  gdouble y_focus = canvas->y_focus;

  if (adjustment == canvas->horiz_scroll_adjustment)
    x_focus = (canvas->xn - canvas->x1) * (adjustment->value + 0.5) / canvas->zoom
      + canvas->x1;
  else if (adjustment == canvas->vert_scroll_adjustment)
    y_focus = (canvas->yn - canvas->y1) * (adjustment->value + 0.5) / canvas->zoom
      + canvas->y1;
  else g_warn("canvas: internal scroll inconsistency");

  canvas_set_focus(canvas, x_focus, y_focus);
}

static void
canvas_set_scroll_adjustments (HosCanvas *self,
			       GtkAdjustment *hadjustment,
			       GtkAdjustment *vadjustment)
{
  canvas_disconnect_scroll_adjustment(self, self->horiz_scroll_adjustment);
  canvas_connect_scroll_adjustment(self, hadjustment);
  self->horiz_scroll_adjustment = hadjustment;

  canvas_disconnect_scroll_adjustment(self, self->vert_scroll_adjustment);
  canvas_connect_scroll_adjustment(self, vadjustment);
  self->vert_scroll_adjustment = vadjustment;

  g_signal_emit(self, signals[WORLD_CONFIGURE], 0);
}
