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

enum {
  LAST_SIGNAL
};

static guint contour_plot_signals[LAST_SIGNAL] = { 0 };

static void contour_plot_expose(HosCanvasItem *self, GdkEventExpose *event);

G_DEFINE_TYPE (HosContourPlot, hos_contour_plot, HOS_TYPE_CONTOUR_PLOT)

static void
hos_contour_plot_class_init(HosContourPlotClass *klass)
{
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*) klass;

  canvas_item_class->expose = contour_plot_expose;
}

static void
hos_contour_plot_init(HosContourPlot *self)
{
  /* FIXME */
}

static void
contour_plot_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  /* FIXME */
}



