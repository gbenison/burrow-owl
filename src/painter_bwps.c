/*
 *  Copyright (C) 2005 Greg Benison
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

/*
 * TODO / DEPRECATED:
 * remove bwps_finish, etc. bwps_anergize
 */

#include <stdlib.h>
#include <string.h>
#include "painter.h"
#include "painter_bwps.h"

static HosPainterClass *parent_class = NULL;


static void hos_painter_bwps_init(HosPainterBwps  *painter);
static void hos_painter_bwps_class_init (HosPainterBwpsClass *klass);
static void hos_painter_bwps_set_property (GObject         *object,
				      guint            prop_id,
				      const GValue    *value,
				      GParamSpec      *pspec);
static void hos_painter_bwps_get_property (GObject         *object,
				      guint            prop_id,
				      GValue          *value,
				      GParamSpec      *pspec);

static void bwps_init(HosPainterBwps*, const gdouble, const gdouble, const guint);
static void bwps_extend(HosPainterBwps*, const gdouble, const gdouble);
static void bwps_finish(HosPainterBwps*);
static void bwps_trace_line(HosPainterBwps *self, struct hos_point* points, const gint n_point, gint lvl, gboolean closed);

/*
 * Option to disable all printing of results.
 * contours will still be traced; this is just here
 * to allow observation of how long it takes to trace the
 * contours without actually printing commands to draw them.
 */
int bwps_anergize = 0;

/*
 * These are the actual postscript tokens emitted
 * by your bwps painter.
 */
#define BWPS_STROKE "cv_stk"
#define BWPS_MOVETO "cv_mv"
#define BWPS_LINETO "cv_ln"


GType
hos_painter_bwps_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosPainterBwpsClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_painter_bwps_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosPainterBwps),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_painter_bwps_init,
      };

      type = g_type_register_static (HOS_TYPE_PAINTER,
				     "HosPainterBwps",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_painter_bwps_class_init (HosPainterBwpsClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  HosPainterClass *painter_class = HOS_PAINTER_CLASS(klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_painter_bwps_set_property;
  gobject_class->get_property = hos_painter_bwps_get_property;

  painter_class->trace_line = (trace_func)bwps_trace_line;

  /* gobject_class->destroy = hos_spectrum_destroy; */

/* here is where you would set klass->member etc. */
  
/*

  PROPERTIES GO HERE
  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}

static void
hos_painter_bwps_init(HosPainterBwps  *painter)
{
  /* FIXME */
}

static void
hos_painter_bwps_set_property (GObject         *object,
			       guint            prop_id,
			       const GValue    *value,
			       GParamSpec      *pspec)
{
  HosPainterBwps *bwps = HOS_PAINTER_BWPS(object);

  bwps=bwps; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_LABEL:
      gtk_button_set_label (button, g_value_get_string (value));
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_painter_bwps_get_property (GObject         *object,
			       guint            prop_id,
			       GValue          *value,
			       GParamSpec      *pspec)
{
  HosPainterBwps *bwps = HOS_PAINTER_BWPS(object);

  bwps=bwps; /* to eliminate warning */

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

static void
bwps_init(HosPainterBwps *self, const gdouble x, const gdouble y, const guint lvl)
{
  gdouble* levels;
  HosContour* contour = painter_get_contour(HOS_PAINTER(self));
  if (bwps_anergize) return;
  levels = (contour_get_levels(contour));
  
  fprintf(self->channel, "%s\n", (levels[lvl] < 0 ? "cntr-negative" : "cntr-positive"));
  fprintf(self->channel, "%f %f %s\n", x, y, BWPS_MOVETO);
}

static void
bwps_trace_line(HosPainterBwps *self, struct hos_point* points, const gint n_point, gint lvl, gboolean closed)
{
  int i;

  if (n_point <= 0)
    return;

  bwps_init(self, points[0].x, points[0].y, lvl);
  for (i = 1; i < n_point; ++i)
    bwps_extend(self, points[i].x, points[i].y);

  if (closed)
    fprintf(self->channel, "closepath\n");
  fprintf(self->channel, "%s\n", BWPS_STROKE);
  fflush(self->channel);

  bwps_finish(self);

}

static void
bwps_extend(HosPainterBwps *self, const gdouble x, const gdouble y)
{
  if (bwps_anergize) return;
  fprintf(self->channel, "%f %f %s\n", x, y, BWPS_LINETO);
}

static void
bwps_finish(HosPainterBwps *self)
{
  if (bwps_anergize) return;
}

HosPainterBwps*
painter_bwps_new_file(gchar* fname)
{
  FILE* channel = NULL;
  HosPainterBwps *result = NULL;

  if (strcmp(fname, "-") == 0)
    channel = stdout;
  else
    channel = fopen(fname, "w");

  result = g_object_new(HOS_TYPE_PAINTER_BWPS, NULL);
  result->channel = channel;

  return result;

}

