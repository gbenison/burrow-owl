/*
 *  Copyright (C) 2006, 2007 Greg Benison
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

#include "ornament.h"
#include "marshal.h"
#include "hoscanvas.h"

/* signals & properties */
enum {
  ACQUIRE,
  RELEASE,
  CONFIGURE,
  LAST_SIGNAL
};

enum {
  PROP_0
};

static guint ornament_signals[LAST_SIGNAL] = { 0 };


static void hos_ornament_set_property   (GObject         *object,
					 guint            prop_id,
					 const GValue    *value,
					 GParamSpec      *pspec);
static void hos_ornament_get_property   (GObject         *object,
					 guint            prop_id,
					 GValue          *value,
					 GParamSpec      *pspec);

static void ornament_acquire_handler(HosOrnament *self);
static void ornament_release_handler(HosOrnament *self);
static void ornament_expose(HosCanvasItem *self, GdkEventExpose *event);
static void ornament_set_canvas(HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas);
static gboolean ornament_overlap_region(HosOrnament *self, GdkRegion *region);
static void ornament_configure_handler(HosOrnament *self);
static GdkRegion* ornament_calculate_region(HosOrnament *self);

static void ornament_canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event, HosOrnament* self);

G_DEFINE_ABSTRACT_TYPE (HosOrnament, hos_ornament, HOS_TYPE_CANVAS_ITEM)

static void
hos_ornament_init(HosOrnament *ornament)
{
  /* FIXME */
}

static void
hos_ornament_class_init (HosOrnamentClass *klass)
{
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*)klass;
  
  gobject_class->set_property = hos_ornament_set_property;
  gobject_class->get_property = hos_ornament_get_property;

  klass->acquire   = ornament_acquire_handler;
  klass->release   = ornament_release_handler;
  klass->configure = ornament_configure_handler;

  canvas_item_class->expose     = ornament_expose;
  canvas_item_class->set_canvas = ornament_set_canvas;

  ornament_signals[ACQUIRE] =
    g_signal_new ("acquire",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, acquire),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  ornament_signals[RELEASE] =
    g_signal_new ("release",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, release),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  ornament_signals[CONFIGURE] =
    g_signal_new ("configure",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST | G_SIGNAL_ACTION,
		  G_STRUCT_OFFSET(HosOrnamentClass, configure),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

  
}

static void
hos_ornament_set_property   (GObject         *object,
			     guint            prop_id,
			     const GValue    *value,
			     GParamSpec      *pspec)
{
  /* FIXME */
}

static void
hos_ornament_get_property   (GObject         *object,
			     guint            prop_id,
			     GValue          *value,
			     GParamSpec      *pspec)
{
  /* FIXME */
}

static void
ornament_acquire_handler(HosOrnament *self)
{
}

static void
ornament_release_handler(HosOrnament *self)
{
}

static void
ornament_expose(HosCanvasItem *self, GdkEventExpose *event)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  g_return_if_fail(HOS_IS_CANVAS(self->canvas));

  HosOrnamentClass *ornament_class = HOS_ORNAMENT_GET_CLASS(self);
  HosOrnament *ornament = HOS_ORNAMENT(self);

  if(ornament_overlap_region(HOS_ORNAMENT(self), event->region))
    ornament_class->paint(HOS_ORNAMENT(self), self->canvas);
}

void
ornament_invalidate_region(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  HosCanvasItem *canvas_item = HOS_CANVAS_ITEM(self);

  if (HOS_IS_CANVAS(canvas_item->canvas))
    {
      GtkWidget *widget = GTK_WIDGET(canvas_item->canvas);
      if (widget->window != NULL)
	gdk_window_invalidate_region(widget->window, self->region, TRUE);
    }
}

void
ornament_set_region(HosOrnament *self, GdkRegion* region)
{
  if (self->region)
    gdk_region_destroy(self->region);
  self->region = region;
}

void
ornament_move (HosOrnament *self, gdouble x, gdouble y)
{

  HosOrnamentClass *class;

  g_return_if_fail(HOS_IS_ORNAMENT(self));
  class = HOS_ORNAMENT_GET_CLASS(self);

  /*
    FIXME deprecate for move_relative ??
    class->set_pos(self, x, y);
  */

}

void
ornament_release (HosOrnament* ornament)
{
  g_return_if_fail(HOS_IS_ORNAMENT(ornament));
  g_signal_emit(ornament, ornament_signals[RELEASE], 0);
}

static GdkRegion*
ornament_calculate_region(HosOrnament *self)
{
  HosOrnamentClass *class = HOS_ORNAMENT_GET_CLASS(self);
  if (class->calculate_region)
    return class->calculate_region(self);
}

/*
 * Test if a click at x(ppm), y(ppm) is in the ornament's active area.
 * If yes: ornament emits 'acuire'; return TRUE
 */
gboolean
ornament_test_grab (HosOrnament *ornament, gdouble x_ppm, gdouble y_ppm)
{
  HosOrnamentClass *class;
  gboolean result = FALSE;

  if (!(HOS_IS_ORNAMENT(ornament)))
    return FALSE;

  class = HOS_ORNAMENT_GET_CLASS(ornament);

  /*

    FIXME use the standard region to test -- test-grab should not be exported-- this
    should be handled as part of the button press signal handlers
  result = class->point_overlap(HOS_ORNAMENT(ornament), x_ppm, y_ppm);

  */

  return result;

}

void
ornament_pick_up(HosOrnament* ornament)
{
  g_signal_emit(ornament, ornament_signals[ACQUIRE], 0);
}

/*
 * TRUE if region defined by x1, y1, xn, yn (in ppm)
 * overlaps the visual area of the ornament.
 */
static gboolean
ornament_overlap_region(HosOrnament *self,
			GdkRegion *region)
{
  GdkRegion* tmp = gdk_region_copy(region);
  gdk_region_intersect(tmp, self->region);

  return gdk_region_empty(tmp) ? FALSE : TRUE;
}

GtkAdjustment*
adjustment_for_spectrum(HosSpectrum *spec, guint dim)
{
  gdouble lower, upper, step;
  gint np;

  np = spectrum_np(spec, dim);
  if (np < 2)
    return NULL;
  lower = spectrum_giro_ppm(spec, dim);
  upper = spectrum_orig_ppm(spec, dim);
  step = spectrum_sw(spec, dim) / np / spectrum_sf(spec, dim);
  return GTK_ADJUSTMENT(gtk_adjustment_new((lower + upper) / 2.0, lower, upper, step, 0, 0));
}

void
ornament_configure(HosOrnament* ornament)
{
  g_return_if_fail(HOS_IS_ORNAMENT(ornament));
  g_signal_emit(ornament, ornament_signals[CONFIGURE], 0);
}

static void
ornament_configure_handler(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  HosCanvasItem *canvas_item = HOS_CANVAS_ITEM(self);

  GdkRegion *old_region = self->region;
  self->region = ornament_calculate_region(self);
  gdk_region_union(old_region, self->region);

  if (canvas_item->canvas)
    canvas_invalidate_region(canvas_item->canvas, old_region);

  gdk_region_destroy(old_region);
}


/* FIXME */
static void
ornament_set_canvas(HosCanvasItem *self, HosCanvas *old_canvas, HosCanvas *canvas)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));
  g_return_if_fail(HOS_IS_CANVAS(canvas));

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
      /* FIXME link new signals */
      g_signal_connect (canvas, "motion-notify-event",
			G_CALLBACK (ornament_canvas_motion_notify),
			self);

      /* link button press signal */
      /* link button release signal */
    }
}

/*
 * Connected to the canvas 'motion-notify' signal; called when the pointer
 * moves over the canvas.
 */
static void
ornament_canvas_motion_notify(GtkWidget *widget, GdkEventMotion *event, HosOrnament* self)
{
  /* FIXME */
  /* move me?? */
  /* pick me up?? */
}

