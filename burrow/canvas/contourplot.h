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

#ifndef HAVE_CONTOUR_PLOT_H
#define HAVE_CONTOUR_PLOT_H

#include "canvasitem.h"
#include "burrow/spectrum.h"
#include "painter_cairo.h"
#include "contour-color.h"

G_BEGIN_DECLS

#define HOS_TYPE_CONTOUR_PLOT             (hos_contour_plot_get_type())
#define HOS_CONTOUR_PLOT(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), HOS_TYPE_CONTOUR_PLOT, HosContourPlot))
#define HOS_CONTOUR_PLOT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_CONTOUR_PLOT, HosContourPlotClass)
#define HOS_IS_CONTOUR_PLOT(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), HOS_TYPE_CONTOUR_PLOT))
#define HOS_IS_CONTOUR_PLOT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass), HOS_TYPE_CONTOUR_PLOT))
#define HOS_CONTOUR_PLOT_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_CONTOUR_PLOT, HosContourPlotClass))

typedef struct _HosContourPlot      HosContourPlot;
typedef struct _HosContourPlotClass HosContourPlotClass;

struct _HosContourPlotClass
{
  HosCanvasItemClass parent_class;

  void(*configuration_changed)(HosContourPlot *contour_plot);
};

struct _HosContourPlot
{
  HosCanvasItem   parent_instance;

  gboolean        smoothed;

  HosPainter      *painter;
  HosPainterCairo *painter_cairo;

};

GType        hos_contour_plot_get_type (void);
void         contour_plot_set_spectrum (HosContourPlot *self, HosSpectrum *spectrum);
void         contour_plot_set_contour  (HosContourPlot *self, HosContour *contour);
HosSpectrum* contour_plot_get_spectrum (HosContourPlot *self);
HosContour*  contour_plot_get_contour  (HosContourPlot *self);

G_END_DECLS

#endif /* not  HAVE_CONTOUR_PLOT_H */


