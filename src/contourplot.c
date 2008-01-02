/*
 *  Copyright (C) 2007 Greg Benison
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

enum {
  PROP_0,
  PROP_SPECTRUM,
  PROP_CONTOUR
};

enum {
  LAST_SIGNAL
};

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
static void     contour_plot_item_configure    (HosCanvasItem *self);
static void     contour_plot_set_painter       (HosContourPlot *self, HosPainter *painter);
static void     contour_plot_sync_xform        (HosContourPlot *self);
static void     contour_plot_set_canvas        (HosCanvasItem *self,
						HosCanvas *old_canvas,
						HosCanvas *canvas);

static gboolean contour_plot_canvas_configure  (GtkWidget *widget, GdkEventConfigure *event, HosContourPlot *self);
static void     contour_plot_canvas_world_configure(HosCanvas *canvas, HosContourPlot *self);

G_DEFINE_TYPE (HosContourPlot, hos_contour_plot, HOS_TYPE_CANVAS_ITEM)

static void
hos_contour_plot_class_init(HosContourPlotClass *klass)
{
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*) klass;
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);

  gobject_class->set_property = contour_plot_set_property;
  gobject_class->get_property = contour_plot_get_property;

  canvas_item_class->expose         = contour_plot_expose;
  canvas_item_class->item_configure = contour_plot_item_configure;
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
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


static void
hos_contour_plot_init(HosContourPlot *self)
{
  /* FIXME this is for cairo types */
  /*  contour_plot_set_painter(self, g_object_new(HOS_TYPE_PAINTER_CAIRO, NULL)); */
  contour_plot_set_painter(self, g_object_new(HOS_TYPE_PAINTER_GDK, NULL));
}


static gboolean
contour_plot_painter_configure(HosPainter *painter,
			       HosContourPlot *contour_plot)
{
  canvas_item_configure(HOS_CANVAS_ITEM(contour_plot));
}

static void
contour_plot_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  HosContourPlot *contour_plot = HOS_CONTOUR_PLOT(self);

  HosCanvas *canvas = self->canvas;
  g_return_if_fail(HOS_IS_CANVAS(canvas));

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

  painter_redraw_region_ppm(HOS_PAINTER(painter_gdk), x1, y1, xn, yn);

}

static void
contour_plot_item_configure(HosCanvasItem *self)
{
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

      HosSpectrum *spectrum = painter_get_spectrum(painter);
      g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

      gdouble x_0 = spectrum_pt2ppm(spectrum, 0, 0);
      gdouble y_0 = spectrum_pt2ppm(spectrum, 1, 0);
      canvas_world2view(canvas, &x_0, &y_0);
      
      gdouble x_1 = spectrum_pt2ppm(spectrum, 0, 1);
      gdouble y_1 = spectrum_pt2ppm(spectrum, 1, 1);
      canvas_world2view(canvas, &x_1, &y_1);

      gdouble x_slope = x_1 - x_0;
      gdouble y_slope = y_1 - y_0;

      painter_set_xform(painter, x_0, y_0, x_slope, y_slope);

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
    }
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

  contour_plot_sync_xform(self);
  return FALSE;
}

static void
contour_plot_canvas_world_configure(HosCanvas *canvas, HosContourPlot *self)
{
  g_return_if_fail(HOS_IS_CANVAS(canvas));
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  contour_plot_sync_xform(self);
}

void
contour_plot_set_spectrum(HosContourPlot *self, HosSpectrum *spectrum)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));
  g_return_if_fail(HOS_IS_PAINTER(self->painter));

  painter_set_spectrum(self->painter, spectrum);
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
