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
#include "painter_cairo.h"

enum {
  PROP_0,
  PROP_SPECTRUM,
  PROP_CONTOUR
};

enum {
  LAST_SIGNAL
};

static guint contour_plot_signals[LAST_SIGNAL] = { 0 };

static void contour_plot_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void contour_plot_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);
static void contour_plot_expose(HosCanvasItem *self, GdkEventExpose *event);
static gboolean contour_plot_painter_configure(HosPainter *painter,
					       HosContourPlot *contour_plot);
static void contour_plot_item_configure(HosCanvasItem *self);
static void contour_plot_set_painter(HosContourPlot *self, HosPainter *painter);



G_DEFINE_TYPE (HosContourPlot, hos_contour_plot, HOS_TYPE_CANVAS_ITEM)

static void
hos_contour_plot_class_init(HosContourPlotClass *klass)
{
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*) klass;
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);

  gobject_class->set_property = contour_plot_set_property;
  gobject_class->get_property = contour_plot_get_property;

  canvas_item_class->expose = contour_plot_expose;
  canvas_item_class->item_configure = contour_plot_item_configure;

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
  contour_plot_set_painter(self, g_object_new(HOS_TYPE_PAINTER_CAIRO, NULL));
  /* FIXME */
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
  /* FIXME */
}

static void
contour_plot_item_configure(HosCanvasItem *self)
{
  /* FIXME invalidate the whole spectrum region */
}


static void
contour_plot_set_painter(HosContourPlot *self, HosPainter *painter)
{
  g_return_if_fail(HOS_IS_CONTOUR_PLOT(self));

  if (self->painter != NULL)
    {
      /* FIXME detach the old painter */
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
