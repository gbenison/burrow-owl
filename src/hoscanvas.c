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
#include "painter_gdk.h"
#include "marshal.h"

enum {
  CLICKED,
  LAST_SIGNAL
};

static guint canvas_signals[LAST_SIGNAL] = { 0 };

static void hos_canvas_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void hos_canvas_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);

static gboolean canvas_button_press(GtkWidget *widget, GdkEventButton *event);
static gboolean canvas_button_release(GtkWidget *widget, GdkEventButton *event);
static gboolean canvas_expose_event(GtkWidget *widget, GdkEventExpose *event);
static gboolean canvas_configure_event(GtkWidget *widget, GdkEventConfigure *event);
static void canvas_realize(GtkWidget *widget);
static gboolean canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event);
static void canvas_painter_ready(HosPainter *painter, gpointer data);
static void canvas_painter_configure(HosPainter *painter, gpointer data);
static void canvas_painter_sync_xform(HosCanvas *canvas, HosPainter *painter);
static void canvas_get_geometry(HosCanvas *canvas, gdouble *width, gdouble *height);
static void canvas_drop_all_ornaments(HosCanvas *canvas);

G_DEFINE_TYPE (HosCanvas, hos_canvas, GTK_TYPE_DRAWING_AREA)

static void
hos_canvas_class_init (HosCanvasClass *klass)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;

  gobject_class = G_OBJECT_CLASS (klass);
  widget_class = GTK_WIDGET_CLASS (klass);
  
  gobject_class->set_property = hos_canvas_set_property;
  gobject_class->get_property = hos_canvas_get_property;

  widget_class->button_press_event=canvas_button_press;
  widget_class->button_release_event=canvas_button_release;
  widget_class->motion_notify_event=canvas_motion_notify;
  widget_class->expose_event=canvas_expose_event;
  widget_class->configure_event=canvas_configure_event;
  widget_class->realize=canvas_realize;

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

  canvas_set_painter(canvas, painter_gdk_new());

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

  canvas->x1 = 0;
  canvas->y1 = 0;
  canvas->xn = 100;
  canvas->yn = 100;

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
  canvas_view2ppm(canvas, &x, &y);


#ifdef UNDEF

  /* FIXME this block to be moved to ornament signal handlers */

  /* there shouldn't be any active ornament, but release to be sure... */
  canvas_drop_all_ornaments(canvas);

  /*
   * See if we have a new active ornament as a result of this button press.
   */

  ptr = canvas->ornaments;
  if (ptr != NULL)
    while (1)
      {
	
	HosOrnament *ornament = (HosOrnament*)(ptr->data);
	
	if (ornament_test_grab(ornament, x, y))
	  {
	    if (grab_id == 0)
	      grab_id = ornament_get_group_id(ornament);
	    
	    if (grab_id == ornament->group_id)
	      {
		canvas->active_ornaments = g_list_append(canvas->active_ornaments, ornament);
		ornament_pick_up(ornament);
	      }
	  }
	
	ptr = ptr->next;
	if (ptr == canvas->ornaments)
	  break;
	
      }

#endif /* UNDEF */

  g_signal_emit(canvas,
		canvas_signals[CLICKED],
		0, x, y);

  if(GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_press_event(widget, event);
  else
    return FALSE;

}

static void
canvas_drop_all_ornaments(HosCanvas *canvas)
{
  /*

    FIXME this functionality to be moved to ornaments' button signal handlers

  if (canvas->active_ornaments != NULL)
    {
      g_list_foreach(canvas->active_ornaments, (GFunc)ornament_release_cb, NULL);
      g_list_free(canvas->active_ornaments);
      canvas->active_ornaments = NULL;
    }
  */
}

/* callback for button release events on canvas widget */
static gboolean
canvas_button_release(GtkWidget *widget, GdkEventButton *event)
{
  HosCanvas *canvas;

  g_assert(HOS_IS_CANVAS(widget));
  canvas = HOS_CANVAS(widget);

  canvas_drop_all_ornaments(canvas);
  
  if(GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_release_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->button_release_event(widget, event);
  else
    return FALSE;

}

static gboolean
canvas_expose_event(GtkWidget *widget, GdkEventExpose *event)
{
  gdouble x1, y1, xn, yn;
  HosCanvas *canvas = HOS_CANVAS(widget);

  /* chain up */
  if (GTK_WIDGET_CLASS(hos_canvas_parent_class)->expose_event)
    GTK_WIDGET_CLASS(hos_canvas_parent_class)->expose_event(widget, event);

  /* redraw the affected canvas portion */
  x1 = event->area.x;
  xn = event->area.x + event->area.width;
  y1 = event->area.y;
  yn = event->area.y + event->area.height;
  canvas_view2pt(canvas, &x1, &y1);
  canvas_view2pt(canvas, &xn, &yn);
  painter_redraw(HOS_PAINTER(canvas->painter), x1, y1, xn, yn);

  GList* ptr;
  for (ptr = canvas->items; ptr != NULL; ptr = ptr->next)
    canvas_item_expose(HOS_CANVAS_ITEM(ptr->data), event);

  return FALSE;
}

/*
 * Set this painter's coordinate transform so that it fills the
 * canvas drawing area.
 */
static void
canvas_painter_sync_xform(HosCanvas *canvas, HosPainter *painter)
{
  static gboolean recursing = FALSE;

  HosSpectrum *spectrum = NULL;

  if (recursing)
    goto done;

  recursing = TRUE;

  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(HOS_IS_PAINTER(painter));

  if (!GTK_WIDGET_REALIZED(canvas))
    goto done;

  spectrum = canvas_get_spectrum(canvas);
  if (spectrum)
    {
      gdouble x_0 = 0.0, x_1 = 1.0, y_0 = 0.0, y_1 = 1.0, x_slope, y_slope;

      canvas_pt2view(canvas, &x_0, &y_0);
      canvas_pt2view(canvas, &x_1, &y_1);

      x_slope = x_1 - x_0;
      y_slope = y_1 - y_0;
      
      if (painter != NULL)
	painter_set_xform(HOS_PAINTER(painter), x_0, y_0, x_slope, y_slope);
    }

 done:

  recursing = FALSE;

}

static gboolean
canvas_configure_event(GtkWidget *widget,
		       GdkEventConfigure *event)
{
  HosCanvas *canvas = HOS_CANVAS(widget);

  canvas_painter_sync_xform(canvas, HOS_PAINTER(canvas->painter));

  gtk_widget_queue_draw(widget);
  
  /* chain up */
  if (GTK_WIDGET_CLASS(hos_canvas_parent_class)->configure_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->configure_event(widget, event);
  else
    return FALSE;
}

static void
canvas_realize(GtkWidget *widget)
{
  HosCanvas *canvas = HOS_CANVAS(widget);

  (GTK_WIDGET_CLASS(hos_canvas_parent_class))->realize(widget);

  canvas->gc = gdk_gc_new(widget->window);

  if (canvas->painter != NULL)
    painter_gdk_set_drawable_gc(canvas->painter,
				GDK_DRAWABLE(widget->window),
				canvas->gc);

}

static gboolean
canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event)
{
  HosCanvas *canvas;
  gdouble x, y;

  g_assert(HOS_IS_CANVAS(widget));
  canvas = HOS_CANVAS(widget);

  x = event->x;
  y = event->y;
  canvas_view2ppm(canvas, &x, &y);

  /*

    FIXME ornaments are now responsible for their own movement through signal handlers

  {
    GList* ptr;
    for (ptr = canvas->active_ornaments; ptr != NULL; ptr = ptr->next)
      ornament_move(HOS_ORNAMENT(ptr->data), x, y);
  }
  */

  if(GTK_WIDGET_CLASS(hos_canvas_parent_class)->motion_notify_event)
    return GTK_WIDGET_CLASS(hos_canvas_parent_class)->motion_notify_event(widget, event);
  else
    return FALSE;
}

HosSpectrum*
canvas_get_spectrum(HosCanvas *self)
{
  HosPainter *painter;

  if (!HOS_IS_PAINTER(self->painter))
    return NULL;
  else
    painter = HOS_PAINTER(self->painter);

  if (!HOS_IS_SPECTRUM(painter->spectrum))
    return NULL;
  else
    return HOS_SPECTRUM(painter->spectrum);
}

void
canvas_view2ppm(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  HosSpectrum *spectrum;
  if (!(HOS_IS_CANVAS(canvas)))
    return;

  spectrum = canvas_get_spectrum(canvas);
  if (spectrum == NULL)
    return;

  canvas_view2pt(canvas, x, y);

  if (x) { *x = spectrum_pt2ppm(spectrum, 0, *x); }
  if (y) { *y = spectrum_pt2ppm(spectrum, 1, *y); }

}

void
canvas_ppm2view(HosCanvas *canvas, gdouble *x, gdouble *y)
{

  HosSpectrum *spectrum;
  if(!(HOS_IS_CANVAS(canvas)))
    return;

  spectrum = canvas_get_spectrum(canvas);
  if (spectrum == NULL)
    return;

  if (x) { *x = spectrum_ppm2pt(spectrum, 0, *x); }
  if (y) { *y = spectrum_ppm2pt(spectrum, 1, *y); }

  canvas_pt2view(canvas, x, y);

}

static void
canvas_get_geometry(HosCanvas *canvas, gdouble *width, gdouble *height)
{
  GtkWidget *widget = GTK_WIDGET(canvas);
  gint x, y;

  if (!GTK_WIDGET_REALIZED(widget))
    return;

  gdk_window_get_size(widget->window, &x, &y);
  *width = x;
  *height = y;
  
#ifdef UNDEFINED
  gdk_window_get_geometry(widget->window,
			  NULL, /* x */
			  NULL, /* y */
			  width,
			  height,
			  NULL);
#endif

}

void
canvas_view2pt(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  /*  GtkWidget *widget = GTK_WIDGET(canvas); */
  gdouble width, height;
  HosSpectrum *spec = canvas_get_spectrum(canvas);

  if (!HOS_IS_SPECTRUM(spec))
    return;

  canvas_get_geometry(canvas, &width, &height);

  if (x) { *x = *x / (gdouble)width * spectrum_np(spec, 0); }
  if (y) { *y = (1.0 - *y / (gdouble)height) * spectrum_np(spec, 1); }

}

void
canvas_pt2view(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  /*  GtkWidget *widget = GTK_WIDGET(canvas); */
  gdouble width, height;
  HosSpectrum *spec = canvas_get_spectrum(canvas);

  if (!HOS_IS_SPECTRUM(spec))
    return;

  canvas_get_geometry(canvas, &width, &height);

  if (x) { *x = width * *x / (gdouble)(spectrum_np(spec, 0)); }
  if (y) { *y = height * ( 1.0 - *y / (gdouble)(spectrum_np(spec, 1) )); }

}


static void
hos_canvas_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  HosCanvas *canvas = HOS_CANVAS(object);

  canvas=canvas; /* to eliminate warning */

  switch (prop_id)
    {
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
  HosCanvas *canvas = HOS_CANVAS(object);

  canvas=canvas; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_IMAGE:
      g_value_set_object (value, (GObject *)priv->image);
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
canvas_set_painter(HosCanvas *self, HosPainterGdk *painter)
{
  if (painter != self->painter)
    {
      if (self->painter != NULL)
	{
	  g_signal_handlers_disconnect_by_func (self->painter,
						canvas_painter_ready,
						self);
	  g_signal_handlers_disconnect_by_func (self->painter,
						canvas_painter_configure,
						self);
	  g_object_unref(self->painter);
	  g_object_unref(self);
	}

      /*
       * FIXME
       * problems arise when the canvas refcount drops to zero and these
       * signals keep triggering...
       * work-around by adding an extra ref to the canvas...
       */
      if (painter != NULL)
	{
	  g_object_ref(painter);
	  g_object_ref(self);
	  self->painter = painter;

	  g_signal_connect(painter, "ready", G_CALLBACK(canvas_painter_ready), self);
	  g_signal_connect(painter, "configuration-changed",
			   G_CALLBACK(canvas_painter_configure), self);

	  painter_gdk_set_drawable_gc(painter,
				      GDK_DRAWABLE(GTK_WIDGET(self)->window),
				      self->gc);
	  canvas_painter_sync_xform(self, HOS_PAINTER(painter));
	}

      gtk_widget_queue_draw(GTK_WIDGET(self));
    }
}

/* connected to painter's "ready" signal */
static void
canvas_painter_ready(HosPainter *painter, gpointer data)
{
  HosCanvas *canvas = HOS_CANVAS(data);
  gtk_widget_queue_draw(GTK_WIDGET(canvas));
}

/* Connected to painter's "configuration-changed" signal */
static void
canvas_painter_configure(HosPainter *painter, gpointer data)
{
  HosCanvas *canvas = HOS_CANVAS(data);
  canvas_painter_sync_xform(canvas, HOS_PAINTER(painter));
  gtk_widget_queue_draw(GTK_WIDGET(canvas));
}

HosPainter*
canvas_get_painter(HosCanvas *self)
{
  return HOS_PAINTER(self->painter);
}

void
canvas_add_item(HosCanvas *self, HosCanvasItem *canvasitem)
{
  g_return_if_fail(HOS_IS_CANVAS(self));
  g_return_if_fail(HOS_IS_CANVAS_ITEM(canvasitem));

  canvas_item_set_canvas(canvasitem, self);

  if (!g_list_find(self->items, canvasitem))
    {
      g_object_ref(canvasitem);
      self->items = g_list_append(self->items, canvasitem);
    }
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

void
canvas_world2view(HosCanvas *canvas, gdouble *x, gdouble *y)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));

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
