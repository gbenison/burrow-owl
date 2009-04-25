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

#ifndef _HOS_HAVE_CANVAS_H
#define _HOS_HAVE_CANVAS_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

#define HOS_TYPE_CANVAS                  (hos_canvas_get_type())
#define HOS_CANVAS(obj)                  (G_TYPE_CHECK_INSTANCE_CAST((obj), HOS_TYPE_CANVAS, HosCanvas))
#define HOS_CANVAS_CLASS(klass)          (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_CANVAS, HosCanvasClass)
#define HOS_IS_CANVAS(obj)               (G_TYPE_CHECK_INSTANCE_TYPE((obj), HOS_TYPE_CANVAS))
#define HOS_IS_CANVAS_CLASS(klass)       (G_TYPE_CHECK_CLASS_TYPE((klass), HOS_TYPE_CANVAS))
#define HOS_CANVAS_GET_CLASS(obj)        (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_CANVAS, HosCanvasClass))

/**
 * @ingroup HosCanvas
 * @brief   A GTK+ widget for displaying spectra and annotations
 */
typedef struct _HosCanvas      HosCanvas;
typedef struct _HosCanvasClass HosCanvasClass;

#include "canvasitem.h"

struct _HosCanvas
{
  GtkDrawingArea parent_instance;

  GdkGC *gc;

  GList *items;
  
  /* world coordinates */
  gdouble x1;
  gdouble y1;
  gdouble xn;
  gdouble yn;

  /* zoom status */
  gdouble zoom;
  gdouble x_focus;
  gdouble y_focus;

};

struct _HosCanvasClass
{

  GtkDrawingAreaClass parent_class;

  void (*clicked)        (HosCanvas *canvas, gdouble x, gdouble y);
  void (*world_configure)(HosCanvas *canvas);

};

HosCanvasItem* canvas_add_item          (HosCanvas *self, HosCanvasItem *canvasitem);
HosCanvasItem* canvas_get_item          (HosCanvas *self, guint idx);
void           canvas_set_world         (HosCanvas *canvas, gdouble x1, gdouble y1, gdouble xn, gdouble yn);
void           canvas_world2view        (HosCanvas *canvas, gdouble *x, gdouble *y);
void           canvas_view2world        (HosCanvas *canvas, gdouble *x, gdouble *y);
void           canvas_invalidate_region (HosCanvas *canvas, GdkRegion *region);
cairo_t*       canvas_get_cairo_context (HosCanvas *canvas);

GtkAdjustment* adjustment_for_canvas_x(HosCanvas* canvas);
GtkAdjustment* adjustment_for_canvas_y(HosCanvas* canvas);

void           canvas_set_zoom          (HosCanvas *canvas, gdouble zoom);
void           canvas_set_focus         (HosCanvas *canvas, gdouble x, gdouble y);

GType hos_canvas_get_type(void);

G_END_DECLS

#endif /* _HOS_HAVE_CANVAS_H */



