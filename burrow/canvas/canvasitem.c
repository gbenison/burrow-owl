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
#include "canvasitem.h"

/**
 * @defgroup HosCanvasItem
 * @brief    Visible objects to place on a HosCanvas
 *
 * Parent Class:
 * - GObject
 *
 * Subclasses:
 * - ::HosOrnament
 * - ::HosContourPlot
 * - ::HosGrid
 *
 * A ::HosCanvas displays a collection of HosCanvasItem objects, each
 * of which has some visual appearance.  Examples include contour plots,
 * cursors, and markers.
 *
 * @{
 */

enum canvas_item_properties {
  PROP_0,
  PROP_CANVAS       /**< The HosCanvas on which this HosCanvasItem appears */
};

enum canvas_item_signals {
  CANVAS_WORLD_CONFIGURE,  /**< The canvas coordinate system has changed */
  ITEM_CONFIGURE,   /**< HosCanvasItem has been changed and needs to be redrawn */
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

static void canvas_item_canvas_world_configure (HosCanvas     *canvas,
						HosCanvasItem *self);

static guint signals[LAST_SIGNAL] = { 0 };

G_DEFINE_ABSTRACT_TYPE (HosCanvasItem, hos_canvas_item, G_TYPE_OBJECT)

static void
hos_canvas_item_class_init(HosCanvasItemClass *klass)
{

  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);

  gobject_class->set_property = canvas_item_set_property;
  gobject_class->get_property = canvas_item_get_property;

  signals[ITEM_CONFIGURE] =
    g_signal_new ("item-configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosCanvasItemClass, configure),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  signals[CANVAS_WORLD_CONFIGURE] =
    g_signal_new ("canvas-world-configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosCanvasItemClass, canvas_world_configure),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__OBJECT,
		  G_TYPE_NONE,
		  1,
		  HOS_TYPE_CANVAS);

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

/**
 * @brief   Re-parent a HosCanvasItem
 *
 * @param   self   The HosCanvasItem to re-parent
 * @param   canvas The HosCanvas on which 'self' will appear
 */
void
canvas_item_set_canvas(HosCanvasItem *self, HosCanvas *canvas)
{
  if (self->canvas != canvas)
    {
      if(HOS_CANVAS_ITEM_GET_CLASS(self)->set_canvas)
	HOS_CANVAS_ITEM_GET_CLASS(self)->set_canvas(self, self->canvas, canvas);

      if (self->canvas != NULL)
	{
	  g_signal_handlers_disconnect_matched (self->canvas,
						G_SIGNAL_MATCH_DATA,
						0,      /* id */
						0,      /* detail */
						NULL,   /* closure */
						NULL,   /* func */
						self);  /* data */
	  g_object_unref(self->canvas);
	}

      self->canvas = canvas;

      if (canvas != NULL)
	{
	  g_object_ref(canvas);
       	  canvas_add_item(canvas, self);
	  g_signal_connect(canvas, "world-configure",
			   G_CALLBACK (canvas_item_canvas_world_configure),
			   self);
	  g_object_notify (G_OBJECT (self), "canvas");
	}
    }
}

static void
canvas_item_canvas_world_configure(HosCanvas *canvas, HosCanvasItem *self)
{
  g_signal_emit(self, signals[CANVAS_WORLD_CONFIGURE], 0, canvas);
}

/** @} */

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

  g_signal_emit(self, signals[ITEM_CONFIGURE], 0);
}

