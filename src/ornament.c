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

#include "ornament.h"
#include "marshal.h"
#include "hoscanvas.h"

/* signals & properties */
enum {
  ACQUIRE,
  RELEASE,
  LAST_SIGNAL
};

enum {
  PROP_0
};

static GObjectClass *parent_class = NULL;
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
static void hos_ornament_class_init (HosOrnamentClass *klass);
static void hos_ornament_init(HosOrnament *ornament);

static gulong ornament_id_counter = 1;


GType
hos_ornament_get_type (void)
{
  static GType ornament_type = 0;

  if (!ornament_type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosOrnamentClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_ornament_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosOrnament),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_ornament_init,
      };

      ornament_type = g_type_register_static (G_TYPE_OBJECT,
					      "HosOrnament",
					      &_info,
					      G_TYPE_FLAG_ABSTRACT);
    }

  return ornament_type;
}

static void
hos_ornament_init(HosOrnament *ornament)
{
  ornament->group_id = ornament_id_counter;
  ornament_id_counter++;
}

static void
hos_ornament_class_init (HosOrnamentClass *klass)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_ornament_set_property;
  gobject_class->get_property = hos_ornament_get_property;

  klass->acquire = ornament_acquire_handler;
  klass->release = ornament_release_handler;

  /*
  g_object_class_install_property (gobject_class,
                                   PROP_N_LVL,
                                   g_param_spec_uint ("n-lvl",
						      "N-lvl",
						      "number of contour levels",
						      0, 0xFFFF, 0,
						      G_PARAM_READWRITE));
  */

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

void
ornament_redraw(HosOrnament *self)
{
  HosOrnamentClass *class;

  g_return_if_fail(HOS_IS_ORNAMENT(self));
  class = HOS_ORNAMENT_GET_CLASS(self);

  class->paint(self);
}

void
ornament_invalidate_region(HosOrnament *self)
{
  g_return_if_fail(HOS_IS_ORNAMENT(self));

  if (HOS_IS_CANVAS(self->canvas))
    {
      GtkWidget *widget = GTK_WIDGET(self->canvas);
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

gulong
ornament_get_group_id(HosOrnament *self)
{
  return self->group_id;
}

void
ornament_set_group_id(HosOrnament *self, gulong id)
{
  self->group_id = id;
}

void
ornament_move (HosOrnament *self, gdouble x, gdouble y)
{

  HosOrnamentClass *class;

  g_return_if_fail(HOS_IS_ORNAMENT(self));
  class = HOS_ORNAMENT_GET_CLASS(self);

  class->set_pos(self, x, y);

}

void
ornament_release (HosOrnament* ornament)
{
  g_return_if_fail(HOS_IS_ORNAMENT(ornament));
  g_signal_emit(ornament, ornament_signals[RELEASE], 0);
}

void
ornament_sync_region(HosOrnament *self)
{
  HosOrnamentClass *class = HOS_ORNAMENT_GET_CLASS(self);
  if (class->sync_region)
    class->sync_region(self);
}

/*
 * Test if a click at x(ppm), y(ppm) is in the ornament's active area.
 * If yes: ornament emits 'acuire'; return TRUE
 */
gboolean
ornament_test_grab (HosOrnament *ornament, gdouble x_ppm, gdouble y_ppm)
{
  HosOrnamentClass *class;
  gboolean result;

  if (!(HOS_IS_ORNAMENT(ornament)))
    return FALSE;

  class = HOS_ORNAMENT_GET_CLASS(ornament);

  result = class->point_overlap(HOS_ORNAMENT(ornament), x_ppm, y_ppm);

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
gboolean
ornament_overlap_region (HosOrnament *ornament,
			 gdouble x1,
			 gdouble y1,
			 gdouble xn,
			 gdouble yn)
{
  HosOrnamentClass *class;

  if (!(HOS_IS_ORNAMENT(ornament)))
    return FALSE;

  class = HOS_ORNAMENT_GET_CLASS(ornament);

  if (class->overlap_region != NULL)
    return class->overlap_region(HOS_ORNAMENT(ornament), x1, y1, xn, yn);
  else
    return FALSE;
      
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
