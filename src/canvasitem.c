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
  PROP_0,
  PROP_CANVAS
};

enum {
  ITEM_CONFIGURE,
  LAST_SIGNAL
};

static void canvas_item_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void canvas_item_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);

static guint canvas_item_signals[LAST_SIGNAL] = { 0 };

G_DEFINE_ABSTRACT_TYPE (HosCanvasItem, hos_canvas_item, G_TYPE_OBJECT)

static void
hos_canvas_item_class_init(HosCanvasItemClass *klass)
{

  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);

  gobject_class->set_property = canvas_item_set_property;
  gobject_class->get_property = canvas_item_get_property;

  canvas_item_signals[ITEM_CONFIGURE] =
    g_signal_new ("item-configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosCanvasItemClass, item_configure),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  g_object_class_install_property (gobject_class,
                                   PROP_CANVAS,
                                   g_param_spec_object ("canvas",
							"Canvas",
							"The canvas on which this canvas-item will be drawn",
							HOS_TYPE_CANVAS,
							G_PARAM_READABLE | G_PARAM_WRITABLE));



}

static void
hos_canvas_item_init(HosCanvasItem *self)
{
  /* FIXME */
}

static void canvas_item_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec)
{
  HosCanvasItem *canvas_item = HOS_CANVAS_ITEM(object);
  HosCanvas *canvas;

  switch (prop_id)
    {
    case PROP_CANVAS:
      canvas = HOS_CANVAS(g_value_get_object(value));
      canvas_item_set_canvas(canvas_item, canvas);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void canvas_item_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec)
{
  HosCanvasItem *canvas_item = HOS_CANVAS_ITEM(object);
  HosCanvas *canvas;

  switch (prop_id)
    {
    case PROP_CANVAS:
      g_value_set_object(value, G_OBJECT(canvas_item->canvas));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


void
canvas_item_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  HosCanvasItemClass *class = HOS_CANVAS_ITEM_GET_CLASS(self);
  if (class->expose) class->expose(self, event);
}

void
canvas_item_configure(HosCanvasItem *self)
{
  g_return_if_fail (HOS_IS_CANVAS_ITEM(self));

  g_signal_emit(self, canvas_item_signals[ITEM_CONFIGURE], 0);
}

void
canvas_item_set_canvas(HosCanvasItem *self, HosCanvas *canvas)
{
  if (self->canvas != canvas)
    {
      if(HOS_CANVAS_ITEM_GET_CLASS(self)->set_canvas)
	HOS_CANVAS_ITEM_GET_CLASS(self)->set_canvas(self, self->canvas, canvas);

      g_object_unref(self->canvas);
      self->canvas = canvas;

      if (canvas != NULL)
	{
	  g_object_ref(canvas);
       	  canvas_add_item(canvas, self);
	  g_object_notify (G_OBJECT (self), "canvas");
	}
    }
}

