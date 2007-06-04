/*
 *  Copyright (C) 2005, 2006 Greg Benison
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

/* turn on finite-state-machine contouring by default. */
#ifndef CONTOUR_NO_FSM
#define CONTOUR_USE_FSM
#endif

#ifdef _VERBOSE
#include <stdio.h>
#endif

#include <assert.h>
#include "painter.h"
#include "boomerang.h"

enum {
  CONFIGURATION_CHANGED,
  READY,
  LAST_SIGNAL
};

enum {
  PROP_0
};

#define PAINTER_IDLE_MSEC 1
#define PAINTER_DRAW_SEC 0.01

static void hos_painter_init(HosPainter  *painter);
static void hos_painter_class_init (HosPainterClass *klass);
static void hos_painter_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_painter_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);
static void painter_configuration_changed (HosPainter *painter);
static void painter_contour_configuration_changed(HosContour *contour, gpointer data);
static void painter_trace_line(HosPainter*, struct hos_point*, const gint, gint, gboolean);
static void painter_spectrum_ready(HosSpectrum *spectrum, gpointer data);
static char* painter_validate_marks(HosPainter *painter);



static GObjectClass *parent_class = NULL;
static guint painter_signals[LAST_SIGNAL] = { 0 };

GType
hos_painter_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosPainterClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_painter_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosPainter),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_painter_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "HosPainter",
				     &_info,
				     G_TYPE_FLAG_ABSTRACT);
    }

  return type;
}

static void
hos_painter_class_init (HosPainterClass *klass)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_painter_set_property;
  gobject_class->get_property = hos_painter_get_property;

  klass->configuration_changed = painter_configuration_changed;
  klass->ready = NULL;

  /*
  g_object_class_install_property (gobject_class,
                                   PROP_N_LVL,
                                   g_param_spec_uint ("n-lvl",
						      "N-lvl",
						      "number of contour levels",
						      0, 0xFFFF, 0,
						      G_PARAM_READWRITE));
  */

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
  painter->marks = NULL;
}

static void
hos_painter_set_property (GObject         *object,
			  guint            prop_id,
			  const GValue    *value,
			  GParamSpec      *pspec)
{
  HosPainter *painter;
  
  painter = HOS_PAINTER(object);

  switch (prop_id)
    {
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
  HosPainter *painter = HOS_PAINTER(object);

  painter=painter; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_IMAGE:
      g_value_set_object (value, (GObject *)priv->image);
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/*
 * The "configuration-changed" signal is emitted whenever
 * the contour parameters associated with the painter or
 * the spectrum associated with the painter changes.
 */
static void
painter_configuration_changed (HosPainter *painter)
{
  painter_cancel_redraws(painter);
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
  if (painter->marks != NULL)
    {
      free(painter->marks);
      painter->marks = NULL;
    }
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

      /* FIXME connect to contour's configuration-changed signal */
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

/*
 * the mark array is a bitvector used in the contour tracing
 * process.  There is one bit, for every contour level, for
 * every edge in the spectrum.
 * A one-to-one correspondence can be made between points in the
 * spectrum and edges.
 * If a contour line is discovered crossing an edge, the bit
 * corresponding to that edge is set.
 *
 * Each point in the spectrum is allocated 'mark_stride' number
 * of bytes; there must be enough bits to accomodate all the
 * contour levels.
 */
static char*
painter_validate_marks(HosPainter *painter)
{
#define TEST(x) { if (!(x)) { return NULL; }}
  TEST(HOS_IS_PAINTER(painter));
  TEST(HOS_IS_SPECTRUM(painter->spectrum));
  TEST(HOS_IS_CONTOUR(painter->contour));
#undef TEST

  if (painter->marks == NULL)
    {
      int n_lvl = contour_get_n_contours(painter->contour);
      int nx = spectrum_np(painter->spectrum, 0);
      int ny = spectrum_np(painter->spectrum, 1);
      
      painter->mark_stride = (n_lvl / 8) + 1;
      g_assert(painter->mark_stride * 8 > n_lvl);
      painter->marks = (char*)calloc(nx * ny * painter->mark_stride, 1);
    }
  return painter->marks;
}

/*
 * Non-blocking painter redraw...
 * if the underlying spectrum is not cached,
 * this returns immediately.
 */
void
painter_redraw(HosPainter* painter,
		     int x_lower,
		     int y_lower,
		     int x_upper,
		     int y_upper)
{

  gdouble* buf = NULL;  /* an array that holds the spectral data. */
  int mark_stride;
  char* marks;
  guint32 cancellation_id;
  int nx, ny;
  int current_level;
  HosContour *contour = HOS_CONTOUR(painter->contour);
  gdouble* levels = contour_get_levels(contour);
  int max_lvl = contour_get_n_contours(contour) - 1;
  int current_row;

  /* upper limit on total number of contour segments */
  int sanity_net = 0;    
#define SANITY_NET_MAX 1e9
  /* upper limit on number of segments in one trace */
  int sanity_trace_n = 0;   
#define SANITY_TRACE_N_MAX 100000
  GTimer *timer = g_timer_new();

  /************/

  if (!(HOS_IS_SPECTRUM(painter->spectrum)))
    return;

  buf = spectrum_traverse(painter->spectrum);
  if (buf == NULL)
    return;

  cancellation_id = painter->cancellation_id;
  nx = spectrum_np(painter->spectrum, 0);
  ny = spectrum_np(painter->spectrum, 1);


  /* normalize limit orders */
  if (y_upper < y_lower)
    {
      int tmp = y_upper;
      y_upper = y_lower;
      y_lower = tmp;
    }

  if (x_upper < x_lower)
    {
      int tmp = x_upper;
      x_upper = x_lower;
      x_lower = tmp;
    }

  /*
   * the bounding box for contour tracing;
   * don't trace outside this box (in spectral points units)
   */
  x_lower = x_lower < 0 ? 0 : x_lower;
  y_lower = y_lower < 0 ? 0 : y_lower;
  x_upper = x_upper < 0 ? nx : x_upper;
  y_upper = y_upper < 0 ? ny : y_upper;

  /* I like a little extra padding */
  x_lower -= 5;
  y_lower -= 5;

  x_upper += 5;
  y_upper += 5;

  /* special case-- bail out of empty spectra */
  if ((nx <= 0) || (ny <= 0))
    return;

  /* enforce clamping of values */
#define G_CLAMP(_x, min, max) {if (_x < min) _x = min; if (_x > max) _x = max;}
  G_CLAMP(x_lower, 0, (nx - 1));
  G_CLAMP(x_upper, 0, (nx - 1));
  G_CLAMP(y_lower, 0, (ny - 1));
  G_CLAMP(y_upper, 0, (ny - 1));
#undef G_CLAMP

  /*
   * Contour drawing using new finite state machine code
   */
#ifdef CONTOUR_USE_FSM
  contour_fsm(buf, x_lower, y_lower, nx,
	      (x_upper - x_lower + 1), (y_upper - y_lower + 1),
	      levels, contour_get_n_contours(contour),
	      painter_trace_line,
	      painter,
	      &(painter->cancellation_id));

  /*********** REMOVE FROM PRODUCTION CODE *********************/
  /* FIXME dummy contour-fsm invocation for testing purposes */

  /*  contour_fsm(buf, nx, 30, 30, levels, 10, test_method, NULL, NULL); */

  /*************************************************************/

#else

  /* Start old contour tracing methods */


  marks = painter_validate_marks(painter);
  mark_stride = painter->mark_stride;
  
  /* clear marks in region */
  {
    int i;
    for (i = y_lower; i < y_upper; ++i)
      {
	int offset = (i * nx + x_lower) * mark_stride;
	int length = (x_upper - x_lower) * mark_stride;
	memset(marks + offset, 0, length);
      }
  }

#define MARK_EDGE(x, y, n) marks[(((y) * nx + (x)) * mark_stride) + ((n) / 8)] |= (1 << ((n) % 8))
#define UNMARK_EDGE(x, y, n) marks[(((y) * nx + (x)) * mark_stride) + ((n) / 8)] &= (~(1 << ((n) % 8)))
#define EDGE_IS_MARKED(x, y, n) (marks[(((y) * nx + (x)) * mark_stride) + ((n) / 8)] & (1 << ((n) % 8)))


  /* ---- confessional verbose debugging; turn on by defining VERBOSE ----- */
#ifdef _VERBOSE
#define CONFESS(...) if (getenv("DEBUG_CONTOUR")) \
        fprintf(stderr, "-- contour --> " __VA_ARGS__)
#else
#define CONFESS(...) /* do nothing */
#endif /* _VERBOSE */

#define DATA(_data_x, _data_y) \
    (buf[(_data_y) * nx + (_data_x)])

#define EDGE_CROSSES_LVL(_x1, _y1, _x2, _y2, _lvl) \
  (!(((DATA(_x1, _y1) > levels[_lvl]) &&   \
      (DATA(_x2, _y2) > levels[_lvl])) ||  \
     ((DATA(_x1, _y1) < levels[_lvl]) &&   \
      (DATA(_x2, _y2) < levels[_lvl]))))

#define EDGE_CROSSES(_crx1, _cry1, _crx2, _cry2) \
  EDGE_CROSSES_LVL(_crx1, _cry1, _crx2, _cry2, current_level)

#define INTERP_X(_x_left, _interp_y) \
((_x_left) + \
((levels[current_level] - DATA(_x_left, _interp_y)) / \
 (DATA((_x_left + 1), _interp_y) - DATA(_x_left, _interp_y))))

#define INTERP_Y(_interp_x, _y_bottom) \
((_y_bottom) + \
((levels[current_level] - DATA((_interp_x), _y_bottom)) / \
 (DATA(_interp_x, (_y_bottom + 1)) - DATA(_interp_x, _y_bottom))))

#define INIT_LINE(__x, __y) {CONTOUR_BUFFER_RESET; CONTOUR_BUFFER_PUSH((__x), (__y));}
#define EXTEND_LINE(__x, __y) {CONTOUR_BUFFER_PUSH((__x), (__y));}
#define FINISH_LINE(lvl, how) { painter_trace_line(painter, contour_buffer_base, contour_buffer_offset, (lvl), (how)); break; }

#define FINISH_LINE_OPEN(lvl) FINISH_LINE((lvl), FALSE);
#define FINISH_LINE_CLOSED(lvl) FINISH_LINE((lvl), TRUE);

#define NORTH 1
#define SOUTH 2
#define EAST 3
#define WEST 4
#define CONTOUR_LOST {assert("contour lost" == 0);}

/*
 * We are in a little room with its southwestern corner
 * at the coordinates (_x, _y).  We are trying to get
 * out and cannot go back whence we came.  If we discover
 * we are out of bounds, we give up.  We try each of the
 * three valid cardinal directions, and move to the
 * next box.
 */

#define TRACE(_whence_arg, inp_x, inp_y) { \
int _x = (inp_x);  int _y = (inp_y); \
int _whence = _whence_arg; \
sanity_trace_n = 0; \
CONFESS("trace *enter* with current_level=%d (%f)\n", current_level, levels[current_level]); \
while(1) {\
assert (sanity_net < SANITY_NET_MAX); ++sanity_net; \
assert (sanity_trace_n < SANITY_TRACE_N_MAX); ++sanity_trace_n; \
CONFESS("trace loop start x=%d, y=%d, whence=%d\n", _x, _y, _whence); \
if (_x < x_lower) FINISH_LINE_OPEN(current_level); \
if (_y < y_lower) FINISH_LINE_OPEN(current_level); \
if (_x >= x_upper - 1) FINISH_LINE_OPEN(current_level); \
if (_y >= y_upper - 1) FINISH_LINE_OPEN(current_level); \
if ((_whence != NORTH) && EDGE_CROSSES(_x, _y + 1, _x + 1, _y + 1)) {\
  if (EDGE_IS_MARKED(_x, _y + 1, current_level)) FINISH_LINE_CLOSED(current_level); \
  EXTEND_LINE(INTERP_X(_x, _y + 1), (gdouble)(_y + 1)); \
  MARK_EDGE(_x, _y + 1, current_level); \
  ++_y; _whence = SOUTH; continue; } \
if ((_whence != SOUTH) && EDGE_CROSSES(_x, _y, _x + 1, _y)) {\
  if (EDGE_IS_MARKED(_x, _y, current_level)) FINISH_LINE_CLOSED(current_level); \
  EXTEND_LINE(INTERP_X(_x, _y), (gdouble)_y); \
  MARK_EDGE(_x, _y, current_level); \
  --_y; _whence = NORTH; continue; } \
if ((_whence != EAST) && EDGE_CROSSES(_x + 1, _y, _x + 1, _y + 1)) {\
  EXTEND_LINE((gdouble)(_x + 1), INTERP_Y(_x + 1, _y)); \
  ++_x; _whence = WEST; continue; } \
if ((_whence != WEST) && EDGE_CROSSES(_x, _y, _x, _y + 1)) {\
  EXTEND_LINE((gdouble)_x, INTERP_Y(_x, _y)); \
  --_x; _whence = EAST; continue; } \
CONTOUR_LOST }}

  /* begin the tracing */
  for (current_row = y_lower; current_row < y_upper; ++current_row)
    {
      int i, down_flag;
      gdouble *spec_ptr;

      /*
       * Recurse the glib main loop here.
       * Note that events called from the main loop
       * (notably signal handlers) may change painter->cancellation_id,
       * causing this drawing to terminate.
       * It is possible for painter_redraw() to be called from the next
       * line too, causing an entire spectrum painting even before
       * this one is complete.
       */

      /* FIXME check timer */
      if (g_timer_elapsed(timer, NULL) > PAINTER_DRAW_SEC)
	{
	  boomerang_throw(PAINTER_IDLE_MSEC);
	  g_timer_start(timer);
	  gdk_flush();
	}

      if (cancellation_id != painter->cancellation_id)
	return;

      spec_ptr = &buf[current_row * nx + x_lower];
#define THIS_POINT (*spec_ptr)
#define NEXT_POINT (*(spec_ptr + 1))

      /* initialize this row-- seek the appropriate starting level */
      current_level = -1;
      while(THIS_POINT > levels[current_level + 1])
	{
	  current_level++;
	  if (current_level == max_lvl)
	    break;
	  assert(current_level < max_lvl);
	}

      for(i = x_lower; i < (x_upper - 1); ++i)
	{
	  CONFESS("Searching square %d, %d (%f) with current level %d (%f -- %f)\n", \
		  i, current_row, DATA(i, current_row), \
		  current_level, \
		  current_level >= 0 ? levels[current_level] : -1e30, \
		  levels[current_level + 1]);

	  /*
	   * Assert 'starting condition'--
	   * The 'current_level' is the level nearest the leftmost
	   * point on the bottom side.
	   * This block is reponsible for setting up the same starting
	   * condition upon its next entry.
	   */
	  if ((current_level >= 0) && (current_level <= max_lvl))
	    {
	      assert(THIS_POINT > levels[current_level]);
	      if ((current_level + 1) <= max_lvl)
		assert(THIS_POINT <= levels[current_level + 1]);
	    }

	  /* search upwards */
	  down_flag = 1;
	  while (((current_level + 1) <= max_lvl)
		 && (NEXT_POINT > levels[current_level + 1]))
	    {
	      down_flag = 0;

	      current_level++;

	      /* level sanity check */
	      assert(current_level >= 0);
	      if(current_level < (max_lvl - 1))
		assert(levels[current_level + 1] > levels[current_level]);

	      CONFESS("Increasing contour level (%d -> %d) due to next point (%f)\n",
		      current_level, current_level + 1, NEXT_POINT);

	      if (!EDGE_IS_MARKED(i, current_row, current_level))
		{
		  INIT_LINE(INTERP_X(i, current_row), (gdouble)current_row);
		  TRACE(NORTH, i, current_row - 1);
		}
	      if (!EDGE_IS_MARKED(i, current_row, current_level))
		{
		  INIT_LINE(INTERP_X(i, current_row), (gdouble)current_row);
		  TRACE(SOUTH, i, current_row);
		}
	    }

	  /* search downwards */
	  if (down_flag)
	    while ((current_level >= 0) && (NEXT_POINT <= levels[current_level]))
	      {

		/* level sanity check */
		if (current_level > 0)
		  assert(levels[current_level - 1] < levels[current_level]);

		if (!EDGE_IS_MARKED(i, current_row, current_level))
		  {
		    INIT_LINE(INTERP_X(i, current_row), (gdouble)current_row);
		    TRACE(NORTH, i, current_row - 1);
		  }
		if (!EDGE_IS_MARKED(i, current_row, current_level))
		  {
		    INIT_LINE(INTERP_X(i, current_row), (gdouble)current_row);
		    TRACE(SOUTH, i, current_row);
		  }
		current_level--;
		
	      }
	  
	  spec_ptr++;

	}
    }
#undef MARK_EDGE
#undef UNMARK_EDGE
#undef EDGE_IS_MARKED
#undef TRACE
#undef CONTOUR_LOST
#undef EDGE_CROSSES

  g_timer_destroy(timer);

#endif /* CONTOUR_USE_FSM */

}

/*
 * Cancellation mechanism for asynchronous redraws:
 *
 * Every painter maintains a 'cancellation id' counter.
 * This counter is recorded at the start of a drawing task.
 * The drawing task is abandoned if the painter's cancellation
 * id changes (which it can do during the redraw, because the
 * main loop is called periodically during redraws.)
 *
 */
void
painter_cancel_redraws(HosPainter* painter)
{
  g_return_if_fail(HOS_IS_PAINTER(painter));
  painter->cancellation_id++;
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

