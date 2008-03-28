/*
 *  Copyright (C) 2007, 2008 Greg Benison
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
#include "contourplot.h"
#include "painter_gdk.h"
#include "painter_cairo.h"
#include "cairo_shapes.h"
#include "boomerang.h"

enum {
  PROP_0,
  PROP_SPECTRUM,
  PROP_CONTOUR,
  PROP_SMOOTHED
};

#define HOS_CONTOUR_PLOT_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_CONTOUR_PLOT, HosContourPlotPrivate))
#define CONTOUR_PLOT_PRIVATE(o, field) ((HOS_CONTOUR_PLOT_GET_PRIVATE(o))->field)
typedef struct _HosContourPlotPrivate HosContourPlotPrivate;

struct _HosContourPlotPrivate
{
  gboolean xform_is_in_sync;
  gboolean cairo_ready;
  gboolean cairo_tracing;

  gpointer cairo_trace_state;

  cairo_surface_t *backing;
  cairo_t         *cr;

  gulong configure_id;

};

enum {
  LAST_SIGNAL
};

/* The time to spend tracing contours before checking for more events */
static gdouble contour_plot_draw_interval = 0.003; /* sec */
/* The time to spend in the main loop, taking a break from contour tracing */
static guint contour_plot_sleep_interval = 1;  /* msec */

/* static guint contour_plot_signals[LAST_SIGNAL] = { 0 }; */

static void contour_plot_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void contour_plot_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);
static void     contour_plot_expose            (HosCanvasItem *self, GdkEventExpose *event);
static gboolean contour_plot_painter_configure (HosPainter *painter,
						HosContourPlot *contour_plot);
static gboolean contour_plot_painter_ready     (HosPainter *painter,
						HosContourPlot *contour_plot);
static void     contour_plot_configure         (HosCanvasItem *self);
static void     contour_plot_set_painter       (HosContourPlot *self, HosPainter *painter);
static void     contour_plot_sync_xform        (HosContourPlot *self);
static void     contour_plot_invalidate_xform  (HosContourPlot *self);
static void     contour_plot_set_canvas        (HosCanvasItem *self,
						HosCanvas *old_canvas,
						HosCanvas *canvas);
static gboolean contour_plot_smooth_ready      (HosContourPlot *self);

static gboolean contour_plot_canvas_configure  (GtkWidget *widget,
						GdkEventConfigure *event, HosContourPlot *self);
static void     contour_plot_canvas_world_configure(HosCanvas *canvas, HosContourPlot *self);
static void     contour_plot_set_smoothed      (HosContourPlot* self, gboolean smoothed);
static void     contour_plot_sync_painters     (HosContourPlot *self);
static void     contour_plot_trace_cairo       (HosContourPlot *self);
static gboolean contour_plot_idle_draw         (HosContourPlot *self);
static void     contour_plot_invalidate_cairo  (HosContourPlot *self, gboolean resize);


static GTimer* contour_plot_timer = NULL;

G_DEFINE_TYPE (HosContourPlot, hos_contour_plot, HOS_TYPE_CANVAS_ITEM)

static void
hos_contour_plot_class_init(HosContourPlotClass *klass)
{
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*) klass;
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);

  gobject_class->set_property = contour_plot_set_property;
  gobject_class->get_property = contour_plot_get_property;

  canvas_item_class->expose         = contour_plot_expose;
  canvas_item_class->configure      = contour_plot_configure;
  canvas_item_class->set_canvas     = contour_plot_set_canvas;

  g_object_class_install_property (gobject_class,
                                   PROP_SPECTRUM,
                                   g_param_spec_object ("spectrum",
							"spectrum",
							"2D spectrum that is drawn by this contour plot",
							HOS_TYPE_SPECTRUM,
							G_PARAM_READABLE | G_PARAM_WRITABLE));


  g_object_class_install_property (gobject_class,
                                   PROP_CONTOUR,
                                   g_param_spec_object ("contour",
							"contour",
							"Contour parameters (threshold, etc.) for this contour plot",
							HOS_TYPE_CONTOUR,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_SMOOTHED,
                                   g_param_spec_boolean ("smoothed",
							 "Smoothed",
							 "If true: this contour plot will draw itself first using fast GDK operations, then with antialiased lines",
							 TRUE,
							 G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_type_class_add_private(gobject_class, sizeof (HosContourPlotPrivate));
}

static void
contour_plot_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  HosContourPlot *contour_plot = HOS_CONTOUR_PLOT(object);
  HosSpectrum *spectrum = NULL;
  HosContour *contour = NULL;

  switch (prop_id)
    {
    case PROP_SPECTRUM:
      spectrum = HOS_SPECTRUM(g_value_get_object(value));
      contour_plot_set_spectrum(contour_plot, spectrum);
      break;
    case PROP_CONTOUR:
      contour = HOS_CONTOUR(g_value_get_object(value));
      contour_plot_set_contour(contour_plot, contour);
      break;
    case PROP_SMOOTHED:
      contour_plot_set_smoothed(HOS_CONTOUR_PLOT(object),
				g_value_get_boolean(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
contour_plot_get_property (GObject         *object,
			   guint            prop_id,
			   GValue          *value,
			   GParamSpec      *pspec)
{
  HosContourPlot *contour_plot = HOS_CONTOUR_PLOT(object);

  switch (prop_id)
    {
    case PROP_SPECTRUM:
      g_value_set_object(value, G_OBJECT(contour_plot_get_spectrum(HOS_CONTOUR_PLOT(object))));
      break;
    case PROP_CONTOUR:
      g_value_set_object(value, G_OBJECT(contour_plot_get_contour(HOS_CONTOUR_PLOT(object))));
      break;
    case PROP_SMOOTHED:
      g_value_set_boolean(value, HOS_CONTOUR_PLOT(object)->smoothed);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


static void
hos_contour_plot_init(HosContourPlot *self)
{
  CONTOUR_PLOT_PRIVATE(self, configure_id) = 1;
  self->smoothed = TRUE;
  self->painter_cairo = g_object_new(HOS_TYPE_PAINTER_CAIRO, NULL);
  contour_plot_set_painter(self, g_object_new(HOS_TYPE_PAINTER_GDK, NULL));
}


static gboolean
contour_plot_painter_configure(HosPainter *painter,
			       HosContourPlot *contour_plot)
{
  ++CONTOUR_PLOT_PRIVATE(contour_plot, configure_id);
  contour_plot_sync_painters(contour_plot);
  contour_plot_invalidate_cairo(contour_plot, FALSE);
  canvas_item_configure(HOS_CANVAS_ITEM(contour_plot));
}

static gboolean
contour_plot_painter_ready(HosPainter *painter,
			   HosContourPlot *contour_plot)
{
  ++CONTOUR_PLOT_PRIVATE(contour_plot, configure_id);
  contour_plot_sync_painters(contour_plot);
  contour_plot_invalidate_cairo(contour_plot, FALSE);
  canvas_item_configure(HOS_CANVAS_ITEM(contour_plot));
}

static void
contour_plot_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  HosContourPlot *contour_plot = HOS_CONTOUR_PLOT(self);

  HosContourPlotPrivate *priv = HOS_CONTOUR_PLOT_GET_PRIVATE(contour_plot);

  HosCanvas *canvas = self->canvas;
  g_return_if_fail(HOS_IS_CANVAS(canvas));

  HosSpectrum* spectrum;
  g_object_get(G_OBJECT(self), "spectrum", &spectrum, NULL);

  if (!HOS_IS_SPECTRUM(spectrum))
    {
      /* FIXME what to do? */
      /* maybe draw a 'no spectrum' message */
      return;
    }

  if (priv->xform_is_in_sync == FALSE)
    contour_plot_sync_xform(contour_plot);

  spectrum_traverse(spectrum);

  gboolean ready;
  g_object_get(G_OBJECT(spectrum), "ready", &ready, NULL);

  if (ready)
    {
      if (contour_plot_smooth_ready(contour_plot))
	{
	  cairo_t* cr = canvas_get_cairo_context(canvas);
	  cairo_set_source_surface(cr, priv->backing, 0, 0);
	  cairo_rectangle(cr, event->area.x, event->area.y, event->area.width, event->area.height);
	  cairo_fill(cr);
	  cairo_destroy(cr);
	}
      else
	{
	  contour_plot_trace_cairo(contour_plot);

	  /* fallback... draw with GDK */
	  HosPainterGdk *painter_gdk = HOS_PAINTER_GDK(contour_plot->painter);
      
	  /* redraw the affected canvas portion */
	  gdouble x1 = event->area.x;
	  gdouble xn = event->area.x + event->area.width;
	  gdouble y1 = event->area.y;
	  gdouble yn = event->area.y + event->area.height;
	  canvas_view2world(canvas, &x1, &y1);
	  canvas_view2world(canvas, &xn, &yn);
	  
	  painter_gdk_set_drawable_gc(painter_gdk,
				      GDK_DRAWABLE(GTK_WIDGET(canvas)->window),
				      canvas->gc);

	  gpointer state =
	    painter_redraw_init_ppm(HOS_PAINTER(painter_gdk), x1, y1, xn, yn);

	  if (state == NULL)
	    return;

	  gulong configure_id = priv->configure_id;

	  if (contour_plot_timer == NULL)
	    {
	      contour_plot_timer = g_timer_new();
	      g_timer_start(contour_plot_timer);
	    }

	  gdouble start_time = g_timer_elapsed(contour_plot_timer, NULL);

	  while (1)
	    {
	      if (configure_id != priv->configure_id)
		{
		  painter_redraw_cancel(state);
		  state = NULL;
		  break;
		}
	      if (painter_redraw_tick(state) == FALSE)
		break;
	      if ((g_timer_elapsed(contour_plot_timer, NULL) - start_time) > contour_plot_draw_interval)
		{
		  gdk_display_flush(gdk_display_get_default());
		  boomerang_throw(contour_plot_sleep_interval);
		  start_time = g_timer_elapsed(contour_plot_timer, NULL);
		}
	    }
	}
    }
  else
    {
      /* spectrum not ready */
      cairo_t* cr = canvas_get_cairo_context(canvas);

      gint window_width, window_height;
      gdk_window_get_size(GTK_WIDGET(canvas)->window,
			  &window_width, &window_height);
      
      cairo_translate (cr, window_width / 2, window_height / 2);
      cairo_shape_hourglass(cr, 15, 25);

      cairo_destroy(cr);
    }

}

static gboolean
contour_plot_smooth_ready(HosContourPlot *self)
{
  g_return_val_if_fail(HOS_IS_CONTOUR_PLOT(self), FALSE);
  return (CONTOUR_PLOT_PRIVATE(self, cairo_ready) && self->smoothed);
}

static void
contour_plot_trace_cairo(HosContourPlot *self)
{
  if (self->smoothed == FALSE)
    return;

  if (CONTOUR_PLOT_PRIVATE(self, cairo_tracing))
    return;

  CONTOUR_PLOT_PRIVATE(self, cairo_tracing) = TRUE;
  g_idle_add_full(G_PRIORITY_LOW, (GSourceFunc)contour_plot_idle_draw, self, NULL);
}

static gboolean
contour_plot_idle_draw(HosContourPlot *self)
{
  g_return_val_if_fail(HOS_IS_CONTOUR_PLOT(self), FALSE);

  HosContourPlotPrivate *priv = HOS_CONTOUR_PLOT_GET_PRIVATE(self);

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;
  if (!GTK_WIDGET_DRAWABLE(canvas))
    {
      priv->cairo_tracing = FALSE;
      return FALSE;
    }

  /* create backing if needed */
  if (priv->backing == NULL)
    {
	gint width, height;
	gdk_window_get_size(GTK_WIDGET(canvas)->window,
			    &width, &height);
	
	priv->backing = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
	priv->cr      = cairo_create(priv->backing);
	g_assert(HOS_IS_PAINTER_CAIRO(self->painter_cairo));
	painter_cairo_set_context(self->painter_cairo, priv->cr);
    }

  if (priv->cairo_trace_state == NULL)
    priv->cairo_trace_state = painter_redraw_init_ppm(HOS_PAINTER(self->painter_cairo),
						      canvas->x1,
						      canvas->y1,
						      canvas->xn,
						      canvas->yn);

  if ((priv->cairo_trace_state != NULL) && (painter_redraw_tick(priv->cairo_trace_state)))
    return TRUE;

  /* get here... must be done! */
  priv->cairo_trace_state = NULL;
  priv->cairo_ready = TRUE;
  canvas_item_configure(HOS_CANVAS_ITEM(self));
  priv->cairo_tracing = FALSE;
  return FALSE;
}

static void
contour_plot_cancel_cairo_draw(HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  CONTOUR_PLOT_PRIVATE(self, cairo_ready) = FALSE;

  if(CONTOUR_PLOT_PRIVATE(self, cairo_trace_state) != NULL)
    painter_redraw_cancel(CONTOUR_PLOT_PRIVATE(self, cairo_trace_state));
  CONTOUR_PLOT_PRIVATE(self, cairo_trace_state) = NULL;
}

/*
 * Cancel any existing cairo trace of the contour plot.
 * if resize==FALSE, just erase the canvas, but keep it.
 * if resize==TRUE, the canvas size has changed and it must be destroyed.
 */
static void
contour_plot_invalidate_cairo(HosContourPlot *self, gboolean resize)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  HosContourPlotPrivate *priv = HOS_CONTOUR_PLOT_GET_PRIVATE(self);

  contour_plot_cancel_cairo_draw(self);

  if (resize)
    {
      if (priv->backing != NULL)
	cairo_surface_destroy(priv->backing);
      priv->backing = NULL;
      
      if(priv->cr != NULL)
	cairo_destroy(priv->cr);
      priv->cr = NULL;
    }
  else
    {
      if ((priv->backing != NULL) && (priv->cr != NULL))
	{
	  cairo_save(priv->cr);
	  cairo_set_operator(priv->cr, CAIRO_OPERATOR_SOURCE);
	  cairo_set_source_rgba(priv->cr, 0, 0, 0, 0);
	  cairo_paint(priv->cr);
	  cairo_restore(priv->cr);
	}
    }

}

static void
contour_plot_configure(HosCanvasItem *self)
{
  /* FIXME cache the old region, so that it's redrawn too! */
  /* invalidate the whole spectrum region */
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;
  if (canvas && GTK_WIDGET_REALIZED(canvas))
    {
      HosPainter* painter = HOS_PAINTER(HOS_CONTOUR_PLOT(self)->painter);
      g_return_if_fail(HOS_IS_PAINTER(painter));

      HosSpectrum *spectrum = painter_get_spectrum(painter);
      g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

      gdouble x1 = spectrum_giro_ppm(spectrum, 0);
      gdouble y1 = spectrum_giro_ppm(spectrum, 1);
      gdouble xn = spectrum_orig_ppm(spectrum, 0);
      gdouble yn = spectrum_orig_ppm(spectrum, 1);

      canvas_world2view(canvas, &x1, &y1);
      canvas_world2view(canvas, &xn, &yn);

      GdkRectangle rect;
      rect.x = MIN(x1, xn);
      rect.y = MIN(y1, yn);
      rect.width = ABS(xn - x1);
      rect.height = ABS(yn - y1);

      GdkRegion *region = gdk_region_rectangle(&rect);
      canvas_invalidate_region(canvas, region);
      gdk_region_destroy(region);
    }
}

/*
 * Call this whenever the scaling of 'self' changes, e.g.:
 * - 'self' widget is resized
 * - 'self' receives a new spectrum
 * - the canvas of 'self' changes world size
 */
static void
contour_plot_invalidate_xform(HosContourPlot *self)
{
  CONTOUR_PLOT_PRIVATE(self, xform_is_in_sync) = FALSE;
}

static void
contour_plot_sync_xform(HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  HosCanvas *canvas = HOS_CANVAS_ITEM(self)->canvas;

  if (canvas && GTK_WIDGET_REALIZED(canvas))
    {
      g_return_if_fail(HOS_IS_CANVAS(canvas));

      HosPainter* painter = HOS_PAINTER(self->painter);
      g_return_if_fail(HOS_IS_PAINTER(painter));

      g_return_if_fail(HOS_IS_PAINTER_CAIRO(self->painter_cairo));

      HosSpectrum *spectrum = painter_get_spectrum(painter);

      if (!HOS_IS_SPECTRUM(spectrum))
	return;

      gdouble x_0 = spectrum_pt2ppm(spectrum, 0, 0);
      gdouble y_0 = spectrum_pt2ppm(spectrum, 1, 0);
      canvas_world2view(canvas, &x_0, &y_0);
      
      gdouble x_1 = spectrum_pt2ppm(spectrum, 0, 1);
      gdouble y_1 = spectrum_pt2ppm(spectrum, 1, 1);
      canvas_world2view(canvas, &x_1, &y_1);

      gdouble x_slope = x_1 - x_0;
      gdouble y_slope = y_1 - y_0;

      painter_set_xform(painter, x_0, y_0, x_slope, y_slope);
      painter_set_xform(HOS_PAINTER(self->painter_cairo), x_0, y_0, x_slope, y_slope);

      CONTOUR_PLOT_PRIVATE(self, xform_is_in_sync) = TRUE;

    }
  
}

static void
contour_plot_set_painter(HosContourPlot *self, HosPainter *painter)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  if (self->painter != NULL)
    {
      /* detach the old painter */
      gint n_connections =
	g_signal_handlers_disconnect_by_func(self->painter,
					     G_CALLBACK(contour_plot_painter_configure), self);
      g_assert(1 == n_connections);
      g_object_unref(self->painter);
    }

  self->painter = painter;
  if (painter != NULL)
    {
      g_return_if_fail(HOS_IS_PAINTER(painter));
      g_object_ref(painter);
      g_signal_connect(painter, "configuration-changed",
		       G_CALLBACK(contour_plot_painter_configure), self);
      g_signal_connect(painter, "ready",
		       G_CALLBACK(contour_plot_painter_ready), self);
    }
  contour_plot_sync_painters(self);
}

static void
contour_plot_sync_painters(HosContourPlot *self)
{
  HosSpectrum *spectrum = painter_get_spectrum(self->painter);
  HosContour  *contour  = painter_get_contour(self->painter);

  if (HOS_IS_CONTOUR(contour))
    painter_set_contour(HOS_PAINTER(self->painter_cairo), contour);
  if (HOS_IS_SPECTRUM(spectrum))
    painter_set_spectrum(HOS_PAINTER(self->painter_cairo), spectrum);
}


static void
contour_plot_set_canvas(HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
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
      g_signal_connect (canvas, "configure-event",
			G_CALLBACK (contour_plot_canvas_configure),
			self);
      g_signal_connect (canvas, "world-configure",
			G_CALLBACK (contour_plot_canvas_world_configure),
			self);
    }
}

/*
 * Callback for canvas widget's 'configure' signal
 */
static gboolean
contour_plot_canvas_configure(GtkWidget *widget, GdkEventConfigure *event, HosContourPlot *self)
{
  g_return_val_if_fail(HOS_IS_CANVAS(widget), TRUE);
  g_return_val_if_fail(HOS_IS_CONTOUR_PLOT(self), TRUE);

  ++CONTOUR_PLOT_PRIVATE(self, configure_id);

  contour_plot_invalidate_cairo(self, TRUE);
  contour_plot_invalidate_xform(self);

  return FALSE;
}

static void
contour_plot_canvas_world_configure(HosCanvas *canvas, HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  ++CONTOUR_PLOT_PRIVATE(self, configure_id);
  contour_plot_invalidate_xform(self);
}

static void
contour_plot_set_smoothed(HosContourPlot* self, gboolean smoothed)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  if (smoothed != self->smoothed)
    {
      self->smoothed = smoothed;
      contour_plot_invalidate_cairo(self, FALSE);
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

void
contour_plot_set_spectrum(HosContourPlot *self, HosSpectrum *spectrum)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  g_return_if_fail(HOS_IS_PAINTER(self->painter));

  painter_set_spectrum(self->painter, spectrum);
  contour_plot_invalidate_xform(self);
}

void
contour_plot_set_contour(HosContourPlot *self, HosContour *contour)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  g_return_if_fail(HOS_IS_PAINTER(self->painter));

  painter_set_contour(self->painter, contour);
}

HosSpectrum*
contour_plot_get_spectrum(HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  g_return_if_fail(HOS_IS_PAINTER(self->painter));

  return self->painter->spectrum;
}

HosContour*
contour_plot_get_contour(HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  g_return_if_fail(HOS_IS_PAINTER(self->painter));

  return painter_get_contour(self->painter);
}
