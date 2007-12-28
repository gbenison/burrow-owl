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
#include "canvasitem.h"

enum {
  LAST_SIGNAL
};

static guint canvas_item_signals[LAST_SIGNAL] = { 0 };

G_DEFINE_ABSTRACT_TYPE (HosCanvasItem, hos_canvas_item, G_TYPE_OBJECT)

static void
hos_canvas_item_class_init(HosCanvasItemClass *klass)
{
  /* FIXME */
}

static void
hos_canvas_item_init(HosCanvasItem *self)
{
  /* FIXME */
}

void
canvas_item_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  HosCanvasItemClass *class = HOS_CANVAS_ITEM_GET_CLASS(self);
  if (class->expose) class->expose(self, event);
}



