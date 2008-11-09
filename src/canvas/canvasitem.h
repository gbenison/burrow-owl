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

#ifndef HAVE_CANVAS_ITEM_H
#define HAVE_CANVAS_ITEM_H

#include <glib-object.h>
#include <gtk/gtk.h>

typedef struct _HosCanvasItem      HosCanvasItem;
typedef struct _HosCanvasItemClass HosCanvasItemClass;

#include "canvas.h"

G_BEGIN_DECLS

#define HOS_TYPE_CANVAS_ITEM              (hos_canvas_item_get_type())
#define HOS_CANVAS_ITEM(obj)              (G_TYPE_CHECK_INSTANCE_CAST((obj), HOS_TYPE_CANVAS_ITEM, HosCanvasItem))
#define HOS_CANVAS_ITEM_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_CANVAS_ITEM, HosCanvasItemClass))
#define HOS_IS_CANVAS_ITEM(obj)           (G_TYPE_CHECK_INSTANCE_TYPE((obj), HOS_TYPE_CANVAS_ITEM))
#define HOS_IS_CANVAS_ITEM_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE((klass), HOS_TYPE_CANVAS_ITEM))
#define HOS_CANVAS_ITEM_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_CANVAS_ITEM, HosCanvasItemClass))


struct _HosCanvasItemClass
{
  GObjectClass parent_class;
  void (*expose)         (HosCanvasItem* self, GdkEventExpose *event);
  void (*configure)      (HosCanvasItem* self);
  void (*set_canvas)     (HosCanvasItem* self, HosCanvas *old_canvas, HosCanvas *canvas);
};

struct _HosCanvasItem
{
  GObject parent_instance;

  HosCanvas *canvas;
};


GType   hos_canvas_item_get_type (void);
void    canvas_item_expose       (HosCanvasItem *self, GdkEventExpose *event);
void    canvas_item_configure    (HosCanvasItem *self);
void    canvas_item_set_canvas   (HosCanvasItem *self, HosCanvas *canvas);

G_END_DECLS

#endif /* HAVE_CANVAS_ITEM_H */



