/*
 *  Copyright (C) 2006, 2007, 2008 Greg Benison
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

#include "ornament.h"
#include "marshal.h"
#include "canvas.h"

/**
 * @defgroup HosOrnament
 * @brief    Annotations for a HosCanvas
 *
 * Parent class
 * - ::HosCanvasItem
 *
 * Subclasses
 * - ::HosMarker
 * - ::HosCursor
 *
 * HosOrnaments are a class of ::HosCanvasItem which serve as annotations for
 * a ::HosCanvas, for example, assignment labels and cursors.  A HosOrnament generally
 * covers only a small portion of the canvas widget, is sensitive to mouse activity,
 * and is movable.
 *
 * @{
 *
 */

enum ornament_properties {
  PROP_0,
  PROP_MOUSE_OVER,   /**< True if the pointer is over the ornament's visible area */
  PROP_GRABBED,      /**< True if the ornament currently has mouse focus */
  PROP_VISIBLE,      /**< True if the ornament should be drawn on-screen */
  PROP_SENSITIVE     /**< True if the ornament can acquire mouse focus, i.e. it is clickable */
};

enum ornament_signals {
  ACQUIRE,           /**< Emitted upon acquiring mouse focus */
  RELEASE,           /**< Emitted upon losing mouse focus */
  ENTER,             /**< The mouse pointer has entered the visible region */
  LEAVE,             /**< The mouse pointer has left the visible region */
  CONFIGURE,         /**< The state of the ornament has changed somehow (position, size, etc.) */
  LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };


static void hos_ornament_set_property   (GObject         *object,
					 guint            prop_id,
					 const GValue    *value,
					 GParamSpec      *pspec);
static void hos_ornament_get_property   (GObject         *object,
					 guint            prop_id,
					 GValue          *value,
					 GParamSpec      *pspec);

static void       ornament_acquire_handler   (HosOrnament *self);
static void       ornament_release_handler   (HosOrnament *self);
static void       ornament_enter_method      (HosOrnament *self);
static void       ornament_leave_method      (HosOrnament *self);
static void       ornament_configure         (HosCanvasItem *self);
static void       ornament_expose            (HosCanvasItem *self, GdkEventExpose *event);
static void       ornament_set_canvas        (HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas);
static gboolean   ornament_overlap_region    (HosOrnament *self, GdkRegion *region);
static GdkRegion* ornament_calculate_region  (HosOrnament *self);
static void       ornament_save_pointer      (HosOrnament *self);

static gboolean ornament_canvas_motion_notify   (GtkWidget *widget, GdkEventMotion *event, HosOrnament* self);
static gboolean ornament_canvas_drag            (GtkWidget *widget, GdkEventMotion *event, HosOrnament* self);
static gboolean ornament_canvas_button_release  (GtkWidget *widget, GdkEventButton *event, HosOrnament* self);
static gboolean ornament_canvas_button_press    (GtkWidget *widget, GdkEventButton *event, HosOrnament* self);
static gboolean ornament_canvas_configure       (GtkWidget *widget, GdkEventConfigure *event, HosOrnament* self);
static void     ornament_canvas_realize         (GtkWidget *widget, HosOrnament* self);
static void     ornament_canvas_world_configure (HosCanvasItem *self,
						 HosCanvas     *canvas);

static void ornament_set_grabbed      (HosOrnament *self, gboolean grabbed);
static void ornament_set_mouse_over   (HosOrnament *self, gboolean mouse_over);
static void ornament_set_visible      (HosOrnament *self, gboolean visible);
static void ornament_set_sensitive    (HosOrnament *self, gboolean sensitive);

G_DEFINE_ABSTRACT_TYPE (HosOrnament, hos_ornament, HOS_TYPE_CANVAS_ITEM)

static void
hos_ornament_init(HosOrnament *self)
{
  ornament_set_sensitive(self, TRUE);
  ornament_set_visible(self, TRUE);
  self->region = gdk_region_new();
}

static void
hos_ornament_class_init (HosOrnamentClass *klass)
{
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*)klass;
  
  gobject_class->set_property = hos_ornament_set_property;
  gobject_class->get_property = hos_ornament_get_property;

  klass->acquire   = ornament_acquire_handler;
  klass->release   = ornament_release_handler;
  klass->enter     = ornament_enter_method;
  klass->leave     = ornament_leave_method;

  canvas_item_class->expose     = ornament_expose;
  canvas_item_class->set_canvas = ornament_set_canvas;
  canvas_item_class->configure  = ornament_configure;
  canvas_item_class->canvas_world_configure = ornament_canvas_world_configure;

  g_object_class_install_property (gobject_class,
                                   PROP_MOUSE_OVER,
                                   g_param_spec_boolean ("mouse-over",
							"Mouse Over",
							"If true, the pointer is in this ornament's region",
							FALSE,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_GRABBED,
                                   g_param_spec_boolean ("grabbed",
							"Grabbed",
							"If true, this ornament is being dragged by the pointer",
							FALSE,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_VISIBLE,
                                   g_param_spec_boolean ("visible",
							 "Visible",
							 "If true, this ornament will be drawn on the screen",
							 TRUE,
							 G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_SENSITIVE,
                                   g_param_spec_boolean ("sensitive",
							 "Sensitive",
							 "If true, this ornament will respond to mouse clicks",
							 TRUE,
							 G_PARAM_READABLE | G_PARAM_WRITABLE));


  signals[ACQUIRE] =
    g_signal_new ("acquire",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, acquire),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[RELEASE] =
    g_signal_new ("release",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, release),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[ENTER] =
    g_signal_new ("enter",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, enter),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[LEAVE] =
    g_signal_new ("leave",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, leave),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[CONFIGURE] =
    g_signal_new ("configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, configure),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  
}

static void
hos_ornament_set_property   (GObject         *object,
			     guint            prop_id,
			     const GValue    *value,
			     GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_MOUSE_OVER:
      ornament_set_mouse_over(HOS_ORNAMENT(object), g_value_get_boolean(value));
      break;
    case PROP_GRABBED:
      ornament_set_grabbed(HOS_ORNAMENT(object), g_value_get_boolean(value));
      break;
    case PROP_VISIBLE:
      ornament_set_visible(HOS_ORNAMENT(object), g_value_get_boolean(value));
      break;
    case PROP_SENSITIVE:
      ornament_set_sensitive(HOS_ORNAMENT(object), g_value_get_boolean(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_ornament_get_property   (GObject         *object,
			     guint            prop_id,
			     GValue          *value,
			     GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_MOUSE_OVER:
      g_value_set_boolean(value, HOS_ORNAMENT(object)->mouse_over);
      break;
    case PROP_GRABBED:
      g_value_set_boolean(value, HOS_ORNAMENT(object)->grabbed);
      break;
    case PROP_VISIBLE:
      g_value_set_boolean(value, HOS_ORNAMENT(object)->visible);
      break;
    case PROP_SENSITIVE:
      g_value_set_boolean(value, HOS_ORNAMENT(object)->sensitive);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
ornament_save_pointer(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(GTK_WIDGET_DRAWABLE(canvas));

  GdkModifierType state;
  gint x_int, y_int;

  gdk_window_get_pointer(GTK_WIDGET(canvas)->window, &x_int, &y_int, &state);

  gdouble x = x_int;
  gdouble y = y_int;

  canvas_view2world(canvas, &x, &y);
  self->save_x = x;
  self->save_y = y;
}

static void
ornament_acquire_handler(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  self->drag_signal_id =
    g_signal_connect (canvas, "motion-notify-event",
		      G_CALLBACK (ornament_canvas_drag),
		      self);

  ornament_save_pointer(self);

}

static void
ornament_release_handler(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  g_signal_handler_disconnect(canvas, self->drag_signal_id);
  self->drag_signal_id = 0;

}


static void
ornament_enter_method(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  self->button_press_signal_id =
    g_signal_connect (canvas, "button-press-event",
		      G_CALLBACK (ornament_canvas_button_press),
		      self);
}

static void
ornament_leave_method(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  g_signal_handler_disconnect(canvas, self->button_press_signal_id);
  self->button_press_signal_id = 0;
}

static void
ornament_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  g_return_if_fail(HOS_IS_CANVAS(self->canvas));

  HosOrnamentClass *ornament_class = HOS_ORNAMENT_GET_CLASS(self);
  HosOrnament *ornament = HOS_ORNAMENT(self);

  if (HOS_ORNAMENT(self)->visible)
    if(ornament_overlap_region(HOS_ORNAMENT(self), event->region))
      ornament_class->paint(HOS_ORNAMENT(self), self->canvas);
}

static GdkRegion*
ornament_calculate_region(HosOrnament *self)
{
  HosOrnamentClass *class = HOS_ORNAMENT_GET_CLASS(self);
  if (self->visible && class->calculate_region)
    return class->calculate_region(self);
  else
    return gdk_region_new();
}

/**
 * @brief Force ornament 'self' to acquire mouse focus.
 */
void
ornament_acquire(HosOrnament* self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  ornament_set_grabbed(self, TRUE);
}

/**
 * @brief Force ornament 'self' to release mouse focus.
 */
void
ornament_release(HosOrnament* self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  ornament_set_grabbed(self, FALSE);
}

/*
 * TRUE if region defined by x1, y1, xn, yn (in ppm)
 * overlaps the visual area of the ornament.
 */
static gboolean
ornament_overlap_region(HosOrnament *self,
			GdkRegion *region)
{
  GdkRegion* tmp = gdk_region_copy(region);
  gdk_region_intersect(tmp, self->region);

  return gdk_region_empty(tmp) ? FALSE : TRUE;
}

static void
ornament_configure(HosCanvasItem *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  HosOrnament* ornament = HOS_ORNAMENT(self);

  if(self->canvas)
    {
      g_return_if_fail(HOS_IS_CANVAS(self->canvas));

      GdkRegion *old_region = ornament->region;
      ornament->region = ornament_calculate_region(ornament);
      gdk_region_union(old_region, ornament->region);
      
      canvas_invalidate_region(self->canvas, old_region);

      gdk_region_destroy(old_region);
    }
}

/*
 * Called whenever this ornament (as a CanvasItem) is associated
 * with a canvas object.
 * Connects to certain of the canvas' signals.
 */
static void
ornament_set_canvas(HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  g_return_if_fail(HOS_IS_CANVAS(canvas));

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
			G_CALLBACK (ornament_canvas_motion_notify),
			self);
      g_signal_connect (canvas, "button-release-event",
			G_CALLBACK (ornament_canvas_button_release),
			self);
      g_signal_connect (canvas, "configure-event",
			G_CALLBACK (ornament_canvas_configure),
			self);
      g_signal_connect (canvas, "realize",
			G_CALLBACK (ornament_canvas_realize),
			self);
    }
}

static gboolean
ornament_canvas_button_release(GtkWidget *widget, GdkEventButton *event, HosOrnament *self)
{
  ornament_release(self);
  return FALSE;
}

static gboolean
ornament_canvas_button_press(GtkWidget *widget, GdkEventButton *event, HosOrnament *self)
{
  if ((event->button == 1) && (self->sensitive))
    {
      ornament_acquire(self);
      return TRUE;
    }
  else return FALSE;
}

static gboolean
ornament_canvas_configure(GtkWidget *widget, GdkEventConfigure *event, HosOrnament *self)
{
  g_return_val_if_fail(HOS_IS_CANVAS(widget), FALSE);
  g_return_val_if_fail(HOS_IS_ORNAMENT(self), FALSE);

  gdk_region_destroy(self->region);
  self->region = ornament_calculate_region(self);

  return FALSE;
}

static void
ornament_canvas_realize(GtkWidget *widget, HosOrnament* self)
{
  g_return_if_fail(HOS_IS_CANVAS(widget));
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  gdk_region_destroy(self->region);
  self->region = ornament_calculate_region(self);

}

static void
ornament_canvas_world_configure(HosCanvasItem *self, HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  HosOrnament *ornament = HOS_ORNAMENT(self);

  gdk_region_destroy(ornament->region);
  ornament->region = ornament_calculate_region(ornament);

  HosCanvasItemClass *parent_class =
    HOS_CANVAS_ITEM_CLASS(hos_ornament_parent_class);

  if (parent_class->canvas_world_configure)
    (parent_class->canvas_world_configure)(self, canvas);
}

/*
 * Connected to the canvas 'motion-notify' signal; called when the pointer
 * moves over the canvas.
 */
static gboolean
ornament_canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event, HosOrnament* self)
{

  /* Is the pointer in the ornament region? */
  GdkModifierType state;
  gint x, y;

  x=event->x;
  y=event->y;
  ornament_set_mouse_over(self,
			  gdk_region_point_in(self->region, x, y));

  return FALSE;
}

/*
 * Called upon motion-notify-event when an ornament has been grabbed.
 */
static gboolean
ornament_canvas_drag(GtkWidget *widget, GdkEventMotion *event, HosOrnament* self)
{

  gdouble old_x = self->save_x;
  gdouble old_y = self->save_y;

  ornament_save_pointer(self);
  
  gdouble dx = self->save_x - old_x;
  gdouble dy = self->save_y - old_y;

  HosOrnamentClass *class = HOS_ORNAMENT_GET_CLASS(self);
  if (class->move_relative)
    class->move_relative(self, dx, dy);
  
  return FALSE;
}

static void
ornament_set_mouse_over(HosOrnament *self, gboolean mouse_over)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  if ((mouse_over != self->mouse_over) && (!self->grabbed))
    {
      self->mouse_over = mouse_over;
      g_object_notify(G_OBJECT(self), "mouse-over");
      g_signal_emit(self,
		    mouse_over ? signals[ENTER] : signals[LEAVE],
		    0);
    }
}

static void
ornament_set_grabbed(HosOrnament *self, gboolean grabbed)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  if (grabbed != self->grabbed)
    {
      self->grabbed = grabbed;
      g_object_notify(G_OBJECT(self), "grabbed");
      g_signal_emit(self,
		    grabbed ? signals[ACQUIRE] : signals[RELEASE],
		    0);
    }
}

static void
ornament_set_visible(HosOrnament *self, gboolean visible)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  if (visible != self->visible)
    {
      self->visible = visible;
      g_object_notify(G_OBJECT(self), "visible");
      canvas_item_configure(HOS_CANVAS_ITEM(self));
      /* FIXME emit a 'hide' or 'show' signal?? */
    }
}

static void
ornament_set_sensitive(HosOrnament *self, gboolean sensitive)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  if (sensitive != self->sensitive)
    {
      self->sensitive = sensitive;
      g_object_notify(G_OBJECT(self), "sensitive");
    }
}

/**
 * @}
 */

