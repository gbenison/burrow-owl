/*
 *  Copyright (C) 2005 Greg Benison
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

#ifndef _HAVE_PAINTER_H
#define _HAVE_PAINTER_H

#include <glib-object.h>
#include <burrow/spectrum.h>
#include "finite-state-machine/contour-fsm.h"
#include "contour.h"

G_BEGIN_DECLS

#define HOS_TYPE_PAINTER              (hos_painter_get_type())
#define HOS_PAINTER(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_PAINTER, HosPainter))
#define HOS_PAINTER_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_PAINTER, HosPainterClass))
#define HOS_IS_PAINTER(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_PAINTER))
#define HOS_IS_PAINTER_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_PAINTER))
#define HOS_PAINTER_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_PAINTER, HosPainterClass))

typedef struct _HosPainter       HosPainter;
typedef struct _HosPainterClass  HosPainterClass;

struct _HosPainterClass
{
  GObjectClass parent_class;

  void(*trace_line)(HosPainter* self, struct hos_point* points, const gint n_point, gint lvl, gboolean closed);
  void(*configuration_changed)(HosPainter *painter);
  void(*ready)(HosPainter *painter);

};

struct _HosPainter
{
  GObject parent_instance;

  gdouble x_slope;
  gdouble y_slope;

  gdouble x_offset;
  gdouble y_offset;

  HosContour *contour;
  HosSpectrum *spectrum;

};
typedef void(*trace_func)(HosPainter*, struct hos_point*, const gint, gint, gboolean);


void painter_set_contour(HosPainter* painter, HosContour *contour);
HosContour* painter_get_contour(HosPainter* painter);

void painter_set_spectrum(HosPainter* painter, HosSpectrum *spectrum);
HosSpectrum* painter_get_spectrum(HosPainter* painter);

fsm_state_t* painter_redraw_init(HosPainter* painter, gint x1, gint xn, gint y1, gint yn);
void painter_redraw_region(HosPainter* painter,
			   int x_lower,
			   int y_lower,
			   int x_upper,
			   int y_upper);
void painter_redraw(HosPainter* painter);
void painter_set_xform(HosPainter* painter,
		       gdouble x_offset,
		       gdouble y_offset,
		       gdouble x_slope,
		       gdouble y_slope);

void painter_view_ppm(HosPainter *painter);
void painter_view_world(HosPainter *painter);

GType hos_painter_get_type(void);

G_END_DECLS

#endif /* not _HAVE_PAINTER_H */
