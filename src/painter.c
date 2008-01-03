/*
 *  Copyright (C) 2005-2007 Greg Benison
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

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gdk/gdk.h>

#include "finite-state-machine/contour-fsm.h"

#ifdef _VERBOSE
#include <stdio.h>
#endif

#include <assert.h>
#include "painter.h"

enum {
  CONFIGURATION_CHANGED,
  READY,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_SPECTRUM,
  PROP_CONTOUR
};

static void hos_painter_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_painter_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);
static void painter_contour_configuration_changed  (HosContour *contour, gpointer data);
static void painter_trace_line                     (HosPainter*, struct hos_point*, const gint, gint, gboolean);
static void painter_spectrum_ready                 (HosSpectrum *spectrum, gpointer data);

static guint painter_signals[LAST_SIGNAL] = { 0 };

G_DEFINE_ABSTRACT_TYPE(HosPainter, hos_painter, G_TYPE_OBJECT)

static void
hos_painter_class_init (HosPainterClass *klass)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS (klass);
  
  gobject_class->set_property = hos_painter_set_property;
  gobject_class->get_property = hos_painter_get_property;

  klass->ready = NULL;

  g_object_class_install_property (gobject_class,
                                   PROP_SPECTRUM,
                                   g_param_spec_object ("spectrum",
							"spectrum",
							"2D spectrum that is drawn by this contour plot",
							HOS_TYPE_SPECTRUM,
							G_PARAM_READABLE | G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                                   PROP_CONTOUR,
                                   g_param_spec_object ("contour",
							"contour",
							"Contour parameters (threshold, etc.) for this contour plot",
							HOS_TYPE_CONTOUR,
							G_PARAM_READABLE | G_PARAM_WRITABLE));
  painter_signals[CONFIGURATION_CHANGED] =
    g_signal_new ("configuration-changed",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET(HosPainterClass, configuration_changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);
  
  painter_signals[READY] =
    g_signal_new ("ready",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET(HosPainterClass, ready),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

}

static void
hos_painter_init(HosPainter *painter)
{
  painter_set_contour(painter, g_object_new(HOS_TYPE_CONTOUR, NULL));
  painter_set_xform(painter, 0, 0, 1, 1);
}

static void
hos_painter_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_SPECTRUM:
      painter_set_spectrum(HOS_PAINTER(object), HOS_SPECTRUM(g_value_get_object(value)));
      break;
    case PROP_CONTOUR:
      painter_set_contour(HOS_PAINTER(object), HOS_CONTOUR(g_value_get_object(value)));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_painter_get_property (GObject         *object,
			  guint            prop_id,
			  GValue          *value,
			  GParamSpec      *pspec)
{
  switch (prop_id)
    {
    case PROP_SPECTRUM:
      g_value_set_object(value, G_OBJECT(painter_get_spectrum(HOS_PAINTER(object))));
      break;
    case PROP_CONTOUR:
      g_value_set_object(value, G_OBJECT(painter_get_contour(HOS_PAINTER(object))));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/*
 * Callback is connected to painter->spectrum:ready
 * and emits the painter's 'ready' signal to notify
 * that the spectrum is cached and the painter is
 * ready to draw.
 */
static void
painter_spectrum_ready(HosSpectrum *spectrum,
		       gpointer data)
{
  HosPainter *painter = HOS_PAINTER(data);
  g_signal_emit(painter, painter_signals[READY], 0);
}

void
painter_set_spectrum(HosPainter *painter, HosSpectrum *spectrum)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  if (painter->spectrum != spectrum)
    {
      if (painter->spectrum)
	{
	  g_signal_handlers_disconnect_by_func (painter->spectrum,
						painter_spectrum_ready,
						painter);
	  g_object_unref(painter->spectrum);
	}
      painter->spectrum = spectrum;
      g_object_ref(spectrum);
      g_signal_connect (painter->spectrum, "ready",
			G_CALLBACK(painter_spectrum_ready),
			painter);
      g_signal_emit(painter, painter_signals[CONFIGURATION_CHANGED], 0);
    }
}

HosContour*
painter_get_contour(HosPainter *painter)
{
  if (HOS_IS_PAINTER(painter))
    return painter->contour;
  else
    return NULL;
}

HosSpectrum*
painter_get_spectrum(HosPainter *painter)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));
  return painter->spectrum;
}

void
painter_set_contour(HosPainter *painter, HosContour *contour)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));
  g_return_if_fail(HOS_IS_CONTOUR(contour));
  if (painter->contour != contour)
    {
      if (painter->contour)
	{
	  g_signal_handlers_disconnect_by_func(painter->contour,
					       painter_contour_configuration_changed,
					       painter);
	  g_object_unref(painter->contour);
	}
      painter->contour = contour;
      g_object_ref(contour);

      /* connect to contour's configuration-changed signal */
      g_signal_connect(contour, "configuration-changed",
		       G_CALLBACK(painter_contour_configuration_changed),
		       painter);

      g_signal_emit(painter, painter_signals[CONFIGURATION_CHANGED], 0);
    }
}

/*
 * When the underlying contour parameters emit a 'configuration-changed' signal,
 * any painter that uses that contour object also emits a 'configuration-changed'
 * signal.
 */
static void
painter_contour_configuration_changed(HosContour *contour,
				      gpointer data)
{
  HosPainter *painter = HOS_PAINTER(data);
  g_signal_emit(painter, painter_signals[CONFIGURATION_CHANGED], 0);
}

/*
 * Contour buffer...
 * Stores the coordinates for contour lines to be used by painter_trace_line.
 * I know, I know, this is not thread safe, it is an evil global variable.
 * Maybe globals are OK sometimes... why are you painting things in threads
 * anyway?
 */
static struct hos_point* contour_buffer_base = NULL;
/* static struct hos_point* contour_buffer = NULL; */
static int contour_buffer_max = 0;
static int contour_buffer_offset = 0;
#define CONTOUR_BUFFER (contour_buffer_base[contour_buffer_offset])

static void
contour_buffer_resize()
{
  while (contour_buffer_offset >= contour_buffer_max)
    {
      contour_buffer_max = contour_buffer_max == 0 ? ( 1 << 8 ) : contour_buffer_max * 2;
      contour_buffer_base = realloc(contour_buffer_base, contour_buffer_max * sizeof(struct hos_point));
    }
}

#define CONTOUR_BUFFER_RESET {contour_buffer_offset = 0;}
#define CONTOUR_BUFFER_PUSH(_pt_x, _pt_y) { \
   while (contour_buffer_offset >= contour_buffer_max) \
      contour_buffer_resize(); \
   assert(contour_buffer_offset < contour_buffer_max); \
   CONTOUR_BUFFER.x = (_pt_x); CONTOUR_BUFFER.y = (_pt_y); ++contour_buffer_offset; }

/*
 * Scale the points in the contour buffer and then call the class-specific line tracing routine.
 * Before this call, contour coordinates in the buffer are stored in spectral point units.
 * This call destructively updates the contour buffer to painter view coordinates.
 */
static void
painter_trace_line(HosPainter* painter, struct hos_point* points, const gint n_point, gint lvl, gboolean closed)
{
  struct hos_point* cur;


  gdouble x_slope = painter->x_slope;
  gdouble y_slope = painter->y_slope;
  gdouble x_offset = painter->x_offset;
  gdouble y_offset = painter->y_offset;

  /* sanity check... */
#ifdef UNDEFINED
  for (cur=points; (cur - points) < (n_point - 1); ++cur)
    {
      struct hos_point* next = cur + 1;
      assert((fabs(cur->x - next->x)) < 2.0);
      assert((fabs(cur->y - next->y)) < 2.0);
    }
#endif

  /* scale the points according to the painter's coordinate xform */
  for (cur=points; (cur - points) < n_point; ++cur)
    {
      cur->x *= x_slope;
      cur->y *= y_slope;
      cur->x += x_offset;
      cur->y += y_offset;
    }

  /* call the class-specific painter tracing routine */
  (HOS_PAINTER_GET_CLASS(painter))->trace_line(painter, points, n_point, (lvl), (closed));
}

gpointer
painter_redraw_init(HosPainter* painter,
		    gint x1, gint xn, gint y1, gint yn)
{
  g_return_val_if_fail(HOS_IS_PAINTER(painter), NULL);
  
  HosSpectrum *spectrum = painter->spectrum;
  g_return_val_if_fail(HOS_IS_SPECTRUM(spectrum), NULL);

  HosContour *contour = HOS_CONTOUR(painter->contour);
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), NULL);

  gdouble* buffer = spectrum_traverse(spectrum);

  if (buffer == NULL)
    return NULL;

  fsm_state_t* result =
    fsm_state_init (buffer,
		    spectrum_np(spectrum, 0),
		    spectrum_np(spectrum, 1),
		    x1, xn, y1, yn,
		    contour_get_levels(contour),
		    contour_get_n_contours(contour),
		    (trace_func_t)painter_trace_line,
		    painter);

  return result;
}

/*
 * Returns:  true: more drawing remains.
 *           false: drawing is done; 'state' is no longer valid.
 */
gboolean
painter_redraw_tick(gpointer state)
{
  g_return_val_if_fail(state != NULL, FALSE);
  gboolean result = contour_fsm((fsm_state_t*)state) ? FALSE : TRUE;

  if (!result)
    fsm_state_free(state);

  return result;
}

void
painter_redraw_cancel(gpointer state)
{
  g_assert(state != NULL);
  fsm_state_free(state);
}

/*
 * Trace contours emanating from within the rectangle defined by x_lower, ...
 * Creates and frees its own marks buffer.
 */
void
painter_redraw_region(HosPainter* painter,
		      int x_lower,
		      int y_lower,
		      int x_upper,
		      int y_upper)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));

  HosSpectrum *spectrum = painter->spectrum;
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  fsm_state_t* state = painter_redraw_init(painter,
					   MIN(x_lower, x_upper),
					   MAX(x_lower, x_upper),
					   MIN(y_lower, y_upper),
					   MAX(y_lower, y_upper));
  while(!contour_fsm(state)) { /* no-op */ }

  fsm_state_free(state);
  
}

gpointer
painter_redraw_init_ppm (HosPainter* painter,
			 gdouble x_lower,
			 gdouble y_lower,
			 gdouble x_upper,
			 gdouble y_upper)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));

  HosSpectrum *spectrum = painter->spectrum;
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));
  
  gint x1 = spectrum_ppm2pt(spectrum, 0, x_lower);
  gint y1 = spectrum_ppm2pt(spectrum, 1, y_lower);
  gint xn = spectrum_ppm2pt(spectrum, 0, x_upper);
  gint yn = spectrum_ppm2pt(spectrum, 1, y_upper);

  /* padding ensures that the whole ppm region will be redrawn */
  return painter_redraw_init(painter,
			     MIN(x1, xn) - 2,
			     MAX(x1, xn) + 2,
			     MIN(y1, yn) - 2,
			     MAX(y1, yn) + 2);

}

void
painter_redraw_region_ppm(HosPainter* painter,
			  gdouble x_lower,
			  gdouble y_lower,
			  gdouble x_upper,
			  gdouble y_upper)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));

  HosSpectrum *spectrum = painter->spectrum;
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  gint x1 = spectrum_ppm2pt(spectrum, 0, x_lower);
  gint y1 = spectrum_ppm2pt(spectrum, 1, y_lower);
  gint xn = spectrum_ppm2pt(spectrum, 0, x_upper);
  gint yn = spectrum_ppm2pt(spectrum, 1, y_upper);

  /* padding ensures that the whole ppm region will be redrawn */
  painter_redraw_region(painter,
			MIN(x1, xn) - 2,
			MIN(y1, yn) - 2,
			MAX(x1, xn) + 2,
			MAX(y1, yn) + 2);
}

/*
 * Redraw the entire region of 'painter'.
 */
void
painter_redraw(HosPainter* painter)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));

  HosSpectrum *spectrum = painter->spectrum;
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  painter_redraw_region(painter, 0, 0, spectrum_np(spectrum, 0) - 1, spectrum_np(spectrum, 1) - 1);
}

void 
painter_set_xform(HosPainter* painter,
		  gdouble x_offset,
		  gdouble y_offset,
		  gdouble x_slope,
		  gdouble y_slope)
{
  painter->x_offset = x_offset;
  painter->y_offset = y_offset;
  painter->x_slope = x_slope;
  painter->y_slope = y_slope;

  g_signal_emit(painter, painter_signals[CONFIGURATION_CHANGED], 0);
}

/*
 * Set this painter's coordinate transform so that view coordinates
 * are in ppm.
 * useful for making hard copy plots
 */
void
painter_view_ppm(HosPainter *painter)
{
  HosSpectrum *spec = painter->spectrum;

  gdouble view_x_min = spectrum_orig_ppm(spec, 0);
  gdouble view_x_max = spectrum_giro_ppm(spec, 0);

  gdouble view_y_min = spectrum_orig_ppm(spec, 1);
  gdouble view_y_max = spectrum_giro_ppm(spec, 1);

  painter->x_offset = view_x_min;
  painter->x_slope = (view_x_max - view_x_min) / spectrum_np(spec, 0);

  painter->y_offset = view_y_min;
  painter->y_slope = (view_y_max - view_y_min) / spectrum_np(spec, 1);

}

/*
 * Set painter xform such that limits correspond to world boundaries (0, 1).
 * also useful for hard copy plots
 */
void
painter_view_world(HosPainter *painter)
{
  HosSpectrum *spec = painter->spectrum;

  gdouble view_x_min = 0;
  gdouble view_x_max = 1;

  gdouble view_y_min = 0;
  gdouble view_y_max = 1;

  painter->x_offset = view_x_min;
  painter->x_slope = (view_x_max - view_x_min) / spectrum_np(spec, 0);

  painter->y_offset = view_y_min;
  painter->y_slope = (view_y_max - view_y_min) / spectrum_np(spec, 1);

}

