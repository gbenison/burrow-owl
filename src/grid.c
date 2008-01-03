/*
 *  Copyright (C) 2008 Greg Benison
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

#include <math.h>
#include "grid.h"

enum {
  PROP_0,
  PROP_ANCHOR_HORIZONTAL,
  PROP_ANCHOR_VERTICAL,
  PROP_SPACING_HORIZONTAL,
  PROP_SPACING_VERTICAL,
  PROP_AUTO_SPACING
};

static void grid_set_property (GObject         *object,
			       guint            prop_id,
			       const GValue    *value,
			       GParamSpec      *pspec);
static void grid_get_property (GObject         *object,
			       guint            prop_id,
			       GValue          *value,
			       GParamSpec      *pspec);

static void  grid_expose          (HosCanvasItem *self, GdkEventExpose *event);
static void  grid_item_configure  (HosCanvasItem *self);
static void  grid_set_canvas      (HosCanvasItem *self,
				   HosCanvas *old_canvas,
				   HosCanvas *canvas);
static gboolean grid_canvas_configure       (GtkWidget *widget,
					     GdkEventConfigure *event, HosGrid *self);
static void     grid_canvas_world_configure (HosCanvas *canvas, HosGrid *self);

static gdouble  round_sig_figs              (gdouble x, gint sig_figs);
static void     grid_set_spacing_horizontal (HosGrid* self, gdouble spacing);
static void     grid_set_spacing_vertical   (HosGrid* self, gdouble spacing);
static void     grid_set_anchor_horizontal  (HosGrid* self, gdouble anchor);
static void     grid_set_anchor_vertical    (HosGrid* self, gdouble anchor);
static void     grid_set_auto_spacing       (HosGrid* self, gint spacing);


G_DEFINE_TYPE (HosGrid, hos_grid, HOS_TYPE_CANVAS_ITEM)

static void
hos_grid_class_init(HosGridClass *klass)
{
  HosCanvasItemClass *canvas_item_class = HOS_CANVAS_ITEM_CLASS(klass);
  GObjectClass       *gobject_class     = G_OBJECT_CLASS(klass);

  gobject_class->set_property   = grid_set_property;
  gobject_class->get_property   = grid_get_property;

  canvas_item_class->expose         = grid_expose;
  canvas_item_class->item_configure = grid_item_configure;
  canvas_item_class->set_canvas     = grid_set_canvas;

  g_object_class_install_property (gobject_class,
                                   PROP_ANCHOR_HORIZONTAL,
                                   g_param_spec_double ("anchor-horizontal",
							"Anchor:Horizontal",
							"One of the horizontal grid lines will fall on anchor-horizontal.",
							0,
							1e12,
							0,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_ANCHOR_VERTICAL,
                                   g_param_spec_double ("anchor-vertical",
							"Anchor:Vertical",
							"One of the vertical grid lines will fall on anchor-vertical.",
							0,
							1e12,
							0,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_SPACING_HORIZONTAL,
                                   g_param_spec_double ("spacing-horizontal",
							"Spacing:Horizontal",
							"spacing between the horizontal grid lines, in world coordinates.",
							0,
							1e12,
							1,
							G_PARAM_READABLE | G_PARAM_WRITABLE));


  g_object_class_install_property (gobject_class,
                                   PROP_SPACING_VERTICAL,
                                   g_param_spec_double ("spacing-vertical",
							"Spacing:Vertical",
							"spacing between the vertical grid lines, in world coordinates.",
							0,
							1e12,
							1,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
                                   PROP_AUTO_SPACING,
                                   g_param_spec_int ("auto-spacing",
						     "AutoSpacing",
						     "Requested space between grid lines, in pixels.  Zero for no auto-spacing.",
						     0,
						     2048,
						     50,
						     G_PARAM_READABLE | G_PARAM_WRITABLE));



}

static void
hos_grid_init(HosGrid* self)
{
  self->auto_spacing = 50;
}

static void
grid_set_property (GObject         *object,
		   guint            prop_id,
		   const GValue    *value,
		   GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_ANCHOR_HORIZONTAL:
      grid_set_anchor_horizontal(HOS_GRID(object), g_value_get_double(value));
      break;
    case PROP_ANCHOR_VERTICAL:
      grid_set_anchor_vertical(HOS_GRID(object), g_value_get_double(value));
      break;
    case PROP_SPACING_HORIZONTAL:
      grid_set_spacing_horizontal(HOS_GRID(object), g_value_get_double(value));
      break;
    case PROP_SPACING_VERTICAL:
      grid_set_spacing_vertical(HOS_GRID(object), g_value_get_double(value));
      break;
    case PROP_AUTO_SPACING:
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
grid_get_property (GObject         *object,
		   guint            prop_id,
		   GValue          *value,
		   GParamSpec      *pspec)
{
  switch (prop_id)
    {    
    case PROP_ANCHOR_HORIZONTAL:
      g_value_set_double(value, HOS_GRID(object)->anchor_horizontal);
      break;
    case PROP_ANCHOR_VERTICAL:
      g_value_set_double(value, HOS_GRID(object)->anchor_vertical);
      break;
    case PROP_SPACING_HORIZONTAL:
      g_value_set_double(value, HOS_GRID(object)->spacing_horizontal);
      break;
    case PROP_SPACING_VERTICAL:
      g_value_set_double(value, HOS_GRID(object)->spacing_vertical);
      break;
    case PROP_AUTO_SPACING:
      g_value_set_int(value, HOS_GRID(object)->auto_spacing);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/*
 * Set the horizontal and vertical spacing based on the
 * 'auto spacing' parameter.
 */
static void
grid_auto_configure(HosGrid* self)
{
  g_return_if_fail(HOS_IS_GRID(self));

  /* spacing of 0 means no auto-spacing was requested. */
  if (self->auto_spacing == 0)
    return;

  HosCanvas* canvas = HOS_CANVAS_ITEM(self)->canvas;
  if (!HOS_IS_CANVAS(canvas))
    return;

  gint window_width, window_height;
  gdk_window_get_size(GTK_WIDGET(canvas)->window,
		      &window_width, &window_height);

  /* spacing = world / n_tics;  n_tics = view / auto_spacing */
  self->spacing_horizontal =
    round_sig_figs((canvas->xn - canvas->x1) /
		   (window_width / self->auto_spacing), 2);

  self->spacing_vertical =
    round_sig_figs((canvas->yn - canvas->y1) /
		   (window_height / self->auto_spacing), 2);

}

static gdouble
round_sig_figs(gdouble x, gint sig_figs)
{
  gdouble factor = 1.0;
  while (x >= 10)
    {
      x /= 10.0;
      factor *= 10.0;
    }
  while (x < 1)
    {
      x *= 10.0;
      factor *= 0.1;
    }
  for (; sig_figs > 1; --sig_figs)
    {
      x *= 10.0;
      factor *= 0.1;
    }

  return factor * floor(x);
}

static void
grid_expose(HosCanvasItem *self, GdkEventExpose *event)
{
}

static void
grid_item_configure(HosCanvasItem *self)
{
}

static gboolean
grid_canvas_configure(GtkWidget *widget,
		      GdkEventConfigure *event, HosGrid *self)
{
  grid_auto_configure(self);
}

static void
grid_canvas_world_configure(HosCanvas *canvas, HosGrid *self)
{
  grid_auto_configure(self);
}

static void
grid_set_canvas(HosCanvasItem *self,
		HosCanvas *old_canvas,
		HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_GRID(self));
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
			G_CALLBACK (grid_canvas_configure),
			self);
      g_signal_connect (canvas, "world-configure",
			G_CALLBACK (grid_canvas_world_configure),
			self);
    }
  grid_auto_configure(HOS_GRID(self));
}



static void
grid_set_spacing_horizontal(HosGrid* self, gdouble spacing)
{
  g_return_if_fail(HOS_IS_GRID(self));

  self->auto_spacing = 0;
  if (self->spacing_horizontal != spacing)
    {
      self->spacing_horizontal = spacing;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

static void
grid_set_spacing_vertical(HosGrid* self, gdouble spacing)
{
  g_return_if_fail(HOS_IS_GRID(self));

  self->auto_spacing = 0;
  if (self->spacing_vertical != spacing)
    {
      self->spacing_vertical = spacing;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

static void
grid_set_anchor_horizontal(HosGrid* self, gdouble anchor)
{
  g_return_if_fail(HOS_IS_GRID(self));

  if (self->anchor_horizontal != anchor)
    {
      self->anchor_horizontal = anchor;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

static void
grid_set_anchor_vertical(HosGrid* self, gdouble anchor)
{
  g_return_if_fail(HOS_IS_GRID(self));

  if (self->anchor_vertical != anchor)
    {
      self->anchor_vertical = anchor;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

static void
grid_set_auto_spacing(HosGrid* self, gint spacing)
{
  g_return_if_fail(HOS_IS_GRID(self));
  if (self->auto_spacing != spacing)
    {
      self->auto_spacing = spacing;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}
