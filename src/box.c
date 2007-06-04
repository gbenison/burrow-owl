/*
 *  Copyright (C) 2006 Greg Benison
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

/* A box-shaped, stretchable ornament */

#include <math.h>
#include "marshal.h"
#include "box.h"

static int box_click_radius = 10;

enum {
  GRABBED,
  MOVED,
  DROPPED,
  LAST_SIGNAL
};


static HosOrnamentClass *parent_class = NULL;
static guint box_signals[LAST_SIGNAL] = { 0 };


static void hos_box_init(HosBox  *box);
static void hos_box_class_init (HosBoxClass *klass);
static void hos_box_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_box_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);


static void box_paint_method(HosOrnament *self);
static void box_set_pos_method(HosOrnament *self, gdouble x, gdouble y);
static gboolean box_overlap_region_method(HosOrnament *self,
					     gdouble x1, gdouble y1,
					     gdouble xn, gdouble yn);
static gboolean box_point_overlap_method(HosOrnament *self, gdouble x, gdouble y);
static void box_acquire_method(HosOrnament *self);
static void box_motion_event_method(HosOrnament *self, gdouble x, gdouble y);
static void box_release_method(HosOrnament *self);

static void box_sync_region(HosOrnament *self);


GType
hos_box_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosBoxClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_box_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBox),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_box_init,
      };

      type = g_type_register_static (HOS_TYPE_ORNAMENT,
				     "HosBox",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_box_class_init (HosBoxClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_box_set_property;
  gobject_class->get_property = hos_box_get_property;

  ornament_class->paint = box_paint_method;
  ornament_class->set_pos = box_set_pos_method;
  ornament_class->overlap_region = box_overlap_region_method;
  ornament_class->point_overlap = box_point_overlap_method;

  ornament_class->sync_region = box_sync_region;

  ornament_class->acquire = box_acquire_method;
  ornament_class->motion_event = box_motion_event_method;
  ornament_class->release = box_release_method;

  box_signals[GRABBED] =
    g_signal_new("grabbed",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosBoxClass, grabbed),
		 NULL, NULL,
		 g_cclosure_marshal_VOID__UINT,
		 G_TYPE_NONE, 1,
		 G_TYPE_UINT);
  
  box_signals[DROPPED] =
    g_signal_new("dropped",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosBoxClass, grabbed),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE,
		 G_TYPE_NONE, 4,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);

  box_signals[MOVED] =
    g_signal_new("moved",
		 G_OBJECT_CLASS_TYPE(gobject_class),
		 G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		 G_STRUCT_OFFSET(HosBoxClass, grabbed),
		 NULL, NULL,
		 g_cclosure_user_marshal_VOID__DOUBLE_DOUBLE_DOUBLE_DOUBLE,
		 G_TYPE_NONE, 4,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE,
		 G_TYPE_DOUBLE);


}

static void
box_paint_method(HosOrnament *self)
{

  HosBox *box = HOS_BOX(self);
  HosCanvas *canvas;
  GtkWidget *widget;
  GdkGC *gc;
  gdouble
    x1 = box->x1,
    y1 = box->y1,
    xn = box->xn,
    yn = box->yn;

  canvas = HOS_CANVAS(self->canvas);
  
  if (canvas == NULL)
    return;

  widget = GTK_WIDGET(canvas);

  if (!(GTK_WIDGET_MAPPED(widget)))
    return;

  gc = canvas->gc;
  {
    GdkColor color = {0, 0xFFFF, 0xFFFF, 0xFFFF};
    gdk_gc_set_rgb_fg_color(gc, &color);
  }
  gdk_gc_set_line_attributes(gc,
			     2, /* width */
			     GDK_LINE_SOLID,
			     GDK_CAP_BUTT,
			     GDK_JOIN_MITER);

  canvas_ppm2view(canvas, &x1, &y1);
  canvas_ppm2view(canvas, &xn, &yn);

  gdk_draw_line(widget->window, gc, x1, y1, x1, yn);
  gdk_draw_line(widget->window, gc, xn, y1, xn, yn);
  gdk_draw_line(widget->window, gc, x1, y1, xn, y1);
  gdk_draw_line(widget->window, gc, x1, yn, xn, yn);

}

/*
 * Compare these new coordinates to the saved motion coords;
 * update box coordinates accordingly
 */
static void
box_set_pos_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosBox *box = HOS_BOX(self);

  if (box->active_x1)
    box->x1 += x - box->save_x;
  if (box->active_y1)
    box->y1 += y - box->save_y;
  if (box->active_xn)
    box->xn += x - box->save_x;
  if (box->active_yn)
    box->yn += y - box->save_y;

  box->save_x = x;
  box->save_y = y;

  ornament_sync_region(HOS_ORNAMENT(self));

}

static gboolean
box_overlap_region_method(HosOrnament *self,
			     gdouble x1,
			     gdouble y1,
			     gdouble xn,
			     gdouble yn)
{
  /* FIXME */
  return TRUE;
}

static gboolean
is_between(gdouble test, gdouble lim_A, gdouble lim_B)
{
  if (lim_A > lim_B)
    {
      gdouble temp = lim_A;
      lim_A = lim_B;
      lim_B = temp;
    }

  return ((test > lim_A) && (test < lim_B));

}

static gboolean
box_point_overlap_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosBox *box = HOS_BOX(self);
  gboolean result = FALSE;
  gdouble
    x1 = box->x1,
    y1 = box->y1,
    xn = box->xn,
    yn = box->yn;

  HosCanvas *canvas = HOS_CANVAS(self->canvas);

  if (box->movable == FALSE)
    return FALSE;

  box->save_x = x;
  box->save_y = y;

  canvas_ppm2view(canvas, &x, &y);
  canvas_ppm2view(canvas, &x1, &y1);
  canvas_ppm2view(canvas, &xn, &yn);

  if (((fabs (x - x1)) < box_click_radius)
      && is_between(y, y1, yn))
    {
      box->active_x1 = TRUE;
      result = TRUE;
    }

  if (((fabs (x - xn)) < box_click_radius)
      && is_between(y, y1, yn)
      && (!box->active_x1))
    {
      box->active_xn = TRUE;
      result = TRUE;
    }

  if (((fabs (y - yn)) < box_click_radius)
      && is_between(x, x1, xn))
    {
      box->active_yn = TRUE;
      result = TRUE;
    }

  if (((fabs (y - y1)) < box_click_radius)
      && is_between(x, x1, xn)
      && (!box->active_yn))
    {
      box->active_y1 = TRUE;
      result = TRUE;
    }

  return result;

}

static void
box_sync_region(HosOrnament *self)
{
  HosCanvas *canvas = self->canvas;
  HosBox *box = HOS_BOX(self);
  gdouble x1 = box->x1;
  gdouble xn = box->xn;
  gdouble y1 = box->y1;
  gdouble yn = box->yn;
  gdouble tmp;

  GdkRectangle rect;

  ornament_invalidate_region(self);

  canvas_ppm2view(canvas, &x1, &y1);
  canvas_ppm2view(canvas, &xn, &yn);

  if (x1 > xn) {tmp = x1; x1 = xn; xn = tmp;}
  if (y1 > yn) {tmp = y1; y1 = yn; yn = tmp;}

  rect.x = x1 - box_click_radius;
  rect.y = y1 - box_click_radius;
  rect.width = xn - x1 + 2 * box_click_radius;
  rect.height = 2 * box_click_radius;

  ornament_set_region(self, gdk_region_rectangle(&rect));

  rect.y = yn - box_click_radius;
  gdk_region_union_with_rect(self->region, &rect);

  rect.width = 2 * box_click_radius;
  rect.y = y1 - box_click_radius;
  rect.height = yn - y1 + 2 * box_click_radius;
  gdk_region_union_with_rect(self->region, &rect);

  rect.x = xn - box_click_radius;
  gdk_region_union_with_rect(self->region, &rect);

  ornament_invalidate_region(self);

}

static void
box_acquire_method(HosOrnament *self)
{

  if (HOS_ORNAMENT_CLASS(parent_class)->acquire)
    (HOS_ORNAMENT_CLASS(parent_class)->acquire)(self);

}

static void
box_motion_event_method(HosOrnament *self, gdouble x, gdouble y)
{
  HosBox *box = HOS_BOX(self);

  if (HOS_ORNAMENT_CLASS(parent_class)->motion_event)
    (HOS_ORNAMENT_CLASS(parent_class)->motion_event)(self, x, y);
  g_signal_emit_by_name(self,
			"moved",
			box->x1,
			box->y1,
			box->xn,
			box->yn);
			

}

static void
box_release_method(HosOrnament *self)
{
  HosBox *box = HOS_BOX(self);

  g_signal_emit_by_name(box,
			"dropped",
			box_get_x1(box),
			box_get_y1(box),
			box_get_xn(box),
			box_get_yn(box));

  box->active_x1 = FALSE;
  box->active_y1 = FALSE;
  box->active_xn = FALSE;
  box->active_yn = FALSE;

  if (HOS_ORNAMENT_CLASS(parent_class)->release)
    (HOS_ORNAMENT_CLASS(parent_class)->release)(self);

}


static void
hos_box_init(HosBox *box)
{
  box->movable = TRUE;
}

static void
hos_box_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  HosBox *box = HOS_BOX(object);

  box=box; /* to eliminate warning */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_box_get_property (GObject         *object,
			  guint            prop_id,
			  GValue          *value,
			  GParamSpec      *pspec)
{
  HosBox *box = HOS_BOX(object);

  box=box; /* to eliminate warning */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

void
box_set_movable(HosBox *box, gboolean movable)
{
  g_return_if_fail(HOS_IS_BOX(box));
  box->movable = movable;
}

/*
 * Create a new box with default position appropriate
 * for this canvas; add to the canvas.
 */
HosBox*
canvas_add_box(HosCanvas *canvas)
{
  HosSpectrum *spectrum = canvas_get_spectrum(canvas);

  HosBox* result = g_object_new(HOS_TYPE_BOX, NULL);

  if (spectrum != NULL)
    {
      result->x1 = spectrum_pt2ppm(spectrum, 0, 0.3 * spectrum_np(spectrum, 0));
      result->xn = spectrum_pt2ppm(spectrum, 0, 0.7 * spectrum_np(spectrum, 0));
      result->y1 = spectrum_pt2ppm(spectrum, 1, 0.3 * spectrum_np(spectrum, 1));
      result->yn = spectrum_pt2ppm(spectrum, 1, 0.7 * spectrum_np(spectrum, 1));
    }

  canvas_add_ornament(canvas, HOS_ORNAMENT(result));

  return result;
}

gdouble box_get_x1(HosBox *box) { return box->x1 > box->xn ? box->xn : box->x1; }
gdouble box_get_y1(HosBox *box) { return box->y1 > box->yn ? box->yn : box->y1; }
gdouble box_get_xn(HosBox *box) { return box->x1 > box->xn ? box->x1 : box->xn; }
gdouble box_get_yn(HosBox *box) { return box->y1 > box->yn ? box->y1 : box->yn; }






