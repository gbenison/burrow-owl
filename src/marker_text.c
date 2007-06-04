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

#include <stdlib.h>
#include <math.h>
#include <glib-object.h>

#ifdef _VERBOSE
#include <stdio.h>
#endif

#include <assert.h>
#include "marshal.h"
#include "marker_text.h"
#include "hosspectrum.h"

enum {
  PROP_0,
  PROP_LABEL
};

enum {
  LAST_SIGNAL
};

static GObjectClass *parent_class = NULL;
/* static guint signals[LAST_SIGNAL] = { 0 }; */


static void hos_marker_text_init(HosMarkerText  *marker);
static void hos_marker_text_class_init (HosMarkerTextClass *klass);
static void hos_marker_text_set_property (GObject         *object,
					  guint            prop_id,
					  const GValue    *value,
					  GParamSpec      *pspec);
static void hos_marker_text_get_property (GObject         *object,
					  guint            prop_id,
					  GValue          *value,
					  GParamSpec      *pspec);


static void marker_text_paint_method(HosOrnament *self);
static void marker_text_get_patch_bbox(HosMarkerText* self, GdkRectangle *rect);
static gboolean marker_text_point_overlap_method(HosOrnament *self,
						 gdouble x_ppm, gdouble y_ppm);
static void marker_text_sync_region(HosOrnament *self);


GType
hos_marker_text_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosMarkerTextClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_marker_text_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosMarkerText),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_marker_text_init,
      };

      type = g_type_register_static (HOS_TYPE_MARKER,
				     "HosMarkerText",
				     &_info,
				     0);
    }

  return type;
}

static void
hos_marker_text_class_init (HosMarkerTextClass *klass)
{
  GObjectClass *gobject_class;
  HosOrnamentClass *ornament_class;
  HosMarkerClass *marker_class;

  gobject_class = G_OBJECT_CLASS (klass);
  ornament_class = HOS_ORNAMENT_CLASS (klass);
  marker_class = HOS_MARKER_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_marker_text_set_property;
  gobject_class->get_property = hos_marker_text_get_property;

  ornament_class->paint = marker_text_paint_method;
  ornament_class->sync_region = marker_text_sync_region;

  g_object_class_install_property (gobject_class,
                                   PROP_LABEL,
                                   g_param_spec_string ("label",
                                                        "Label",
                                                        "Text written on the canvas",
                                                        NULL,
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


}

static void
marker_text_sync_region(HosOrnament *self)
{
  gdouble x, y;
  GdkRectangle patch_rectangle;
  GdkRectangle text_rectangle;
  HosCanvas *canvas = self->canvas;
  HosMarkerText* marker_text = HOS_MARKER_TEXT(self);

  if (marker_get_pos(HOS_MARKER(self), &x, &y))
    {
      ornament_invalidate_region(self);

      marker_text_get_patch_bbox(marker_text, &patch_rectangle);
      
      canvas_ppm2view(canvas, &x, &y);
      text_rectangle.x = x;
      text_rectangle.y = y;
      text_rectangle.width = 0;
      text_rectangle.height = 0;

      if (marker_text->layout)
	{
	  pango_layout_get_pixel_size(marker_text->layout, &text_rectangle.width, &text_rectangle.height);
	  text_rectangle.y -= text_rectangle.height;
	  text_rectangle.width += 2;
	}
      gdk_rectangle_union(&patch_rectangle, &text_rectangle, &patch_rectangle);

      ornament_set_region(self, gdk_region_rectangle(&patch_rectangle));
      ornament_invalidate_region(self);
    }

}

static void
marker_text_paint_method(HosOrnament *self)
{

  HosMarker *marker = HOS_MARKER(self);
  HosMarkerText *marker_text = HOS_MARKER_TEXT(self);
  HosCanvas *canvas;
  GtkWidget *widget;
  gdouble x;
  gdouble y;
  GdkGC *gc;

  canvas = HOS_CANVAS(self->canvas);
  
  if (canvas == NULL)
    return;

  widget = GTK_WIDGET(canvas);

  if (!(GTK_WIDGET_MAPPED(widget)))
    return;

  marker_get_pos(marker, &x, &y);
  canvas_ppm2view(canvas, &x, &y);

  gc = canvas->gc;

  /* draw the patch, if present */
  if ((marker_text->patch_width > 0) && (marker_text->patch_height > 0))
    {
      GdkRectangle rect;
      marker_text_get_patch_bbox(marker_text, &rect);
      GdkColor color = {0, 0x2000, 0xB000, 0x1000};
      gdk_gc_set_rgb_fg_color(gc, &color);

      gdk_draw_rectangle (widget->window, gc, TRUE, rect.x, rect.y, rect.width, rect.height);
    }

  gdk_gc_set_rgb_fg_color(gc, &marker_text->text_color);
  gdk_gc_set_line_attributes(gc,
			     2, /* width */
			     GDK_LINE_SOLID,
			     GDK_CAP_BUTT,
			     GDK_JOIN_MITER);

  /* draw a center dot */
  gdk_draw_line(widget->window, gc, x, y-1, x, y+1);

  /* draw text; offset up to the right a little */
  if (marker_text->layout)
    {
      int x_size, y_size;
      pango_layout_get_pixel_size(marker_text->layout, &x_size, &y_size);

      gdk_draw_layout (widget->window,
		       gc,
		       x + 2,
		       y - y_size,
		       marker_text->layout);
    }

}

static void
marker_text_get_patch_bbox(HosMarkerText* self, GdkRectangle *rect)
{

  HosMarker* marker = HOS_MARKER(self);
  HosOrnament* ornament = HOS_ORNAMENT(self);
  HosCanvas* canvas = ornament->canvas;
  gdouble x1, y1, xn, yn;

  gint x_view, y_view;

  marker_get_pos(marker, &x1, &y1);
  /* note: the sign is due to ppm/ view conventions */
  xn = x1 - self->patch_width;
  yn = y1 - self->patch_height;

  canvas_ppm2view(canvas, &x1, &y1);
  canvas_ppm2view(canvas, &xn, &yn);

  rect->x = x1 < xn ? x1 : xn;
  rect->y = y1 < yn ? y1 : yn;

  rect->width = abs(xn - x1);
  rect->height = abs(yn - y1);

  /* accomodate the dot */
  rect->x -= 2;
  rect->y -= 2;
  rect->width += 4;
  rect->height += 4;

}

void
marker_text_set_label(HosMarkerText* marker_text, const gchar *text)
{
  HosCanvas *canvas = (HOS_ORNAMENT(marker_text))->canvas;

  if (canvas)
    {
      marker_text->layout =
	gtk_widget_create_pango_layout (GTK_WIDGET(canvas), NULL);
      pango_layout_set_markup(marker_text->layout, text, -1);
    }
}



static gboolean
marker_text_point_overlap_method(HosOrnament *self,
			    gdouble x_ppm, gdouble y_ppm)
{
  /* FIXME */
  /* compensate for text offset, etc? */
  marker_text_point_overlap_method(NULL, 0, 0);  /* elim warning */
  return TRUE;
}

static void
hos_marker_text_init(HosMarkerText *marker_text)
{
  HosMarker* marker = HOS_MARKER(marker_text);

  marker->size = 20;
  marker->movable = TRUE;
  marker_text->text_color.pixel = 0;
  marker_text->text_color.red = 0xFFFF;
  marker_text->text_color.green = 0xFFFF;
  marker_text->text_color.blue = 0xFFFF;

}

void
marker_text_set_color(HosMarkerText* self, guint16 red, guint16 green, guint16 blue)
{
  self->text_color.red = red;
  self->text_color.green = green;
  self->text_color.blue = blue;

  ornament_invalidate_region(HOS_ORNAMENT(self));
}

static void
hos_marker_text_set_property (GObject         *object,
			      guint            prop_id,
			      const GValue    *value,
			      GParamSpec      *pspec)
{
  HosMarkerText *marker_text = HOS_MARKER_TEXT(object);

  switch (prop_id)
    {
    case PROP_LABEL:
      marker_text_set_label(marker_text, g_value_get_string(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_marker_text_get_property (GObject         *object,
			  guint            prop_id,
			  GValue          *value,
			  GParamSpec      *pspec)
{
  HosMarker *marker = HOS_MARKER(object);

  marker=marker; /* to eliminate warning */

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

/*
 * Create a new marker with default adjustments appropriate
 * for this canvas; add to the canvas.
 */
HosMarkerText*
canvas_add_marker_text(HosCanvas *canvas, const gchar *text)
{
  HosSpectrum *spectrum = canvas_get_spectrum(canvas);

  HosMarkerText* result = g_object_new(HOS_TYPE_MARKER_TEXT, NULL);
  HosMarker* marker = HOS_MARKER(result);

  if (spectrum != NULL)
    marker_set_adjustments(marker,
			   adjustment_for_spectrum(spectrum, 0),
			   adjustment_for_spectrum(spectrum, 1));

  canvas_add_ornament(canvas, HOS_ORNAMENT(result));
  marker_text_set_label(result, text);

  return result;
}

/*
 * Set the size of the 'patch region'; the blanked-out, specified-in-ppm box that
 * shows how big a hard copy version of this label might be.
 */
void
marker_text_set_patch(HosMarkerText* self, gdouble width, gdouble height)
{
  g_return_if_fail(HOS_IS_MARKER_TEXT(self));
  self->patch_width = width;
  self->patch_height = height;
}









