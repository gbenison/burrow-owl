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

/*
 * A 'contour' encapsulates information necessary for tracing
 * contour lines, that is independent of the drawing surface.
 * It is used together with a 'painter' object, which encapsulates
 * information that is dependent on the drawing surface.
 */

#ifndef _HAVE_CONTOUR_H
#define _HAVE_CONTOUR_H

#include <glib-object.h>
#include <gtk/gtk.h>

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_CONTOUR              (contour_get_type())
#define HOS_CONTOUR(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_CONTOUR, HosContour))
#define HOS_CONTOUR_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_CONTOUR, HosContourClass))
#define HOS_IS_CONTOUR(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_CONTOUR))
#define HOS_IS_CONTOUR_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_CONTOUR))
#define HOS_CONTOUR_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_CONTOUR, HosContourClass))

typedef struct _HosContour       HosContour;
typedef struct _HosContourClass  HosContourClass;

struct _HosContourClass
{
  GObjectClass parent_class;

  void(*configuration_changed)(HosContour *self);
};

/* FIXME -- add width, dash pattern, what else ?? */
struct contour_linestyle_struct {

  /* color */
  guint16 red;
  guint16 blue;
  guint16 green;

};

struct _HosContour
{
  GObject parent_instance;

  GtkAdjustment* thres_adjustment;
  GtkAdjustment* factor_adjustment;
  GtkAdjustment* nlvl_adjustment;
  gboolean draw_negative;

  guint16 red_min_pos;
  guint16 blue_min_pos;
  guint16 green_min_pos;

  guint16 red_max_pos;
  guint16 blue_max_pos;
  guint16 green_max_pos;

  guint16 red_min_neg;
  guint16 blue_min_neg;
  guint16 green_min_neg;

  guint16 red_max_neg;
  guint16 blue_max_neg;
  guint16 green_max_neg;

  gdouble *levels;
  struct contour_linestyle_struct *lines;

};

#define CONTOUR_GET_RED(cntr, lvl) (cntr->lines[lvl].red)
#define CONTOUR_GET_BLUE(cntr, lvl) (cntr->lines[lvl].blue)
#define CONTOUR_GET_GREEN(cntr, lvl) (cntr->lines[lvl].green)

guint contour_get_n_contours(HosContour *contour);
gdouble* contour_get_levels(HosContour *contour);
void contour_set_draw_negative(HosContour *self, gboolean draw_negative);
gdouble contour_get_thres(HosContour* contour);
gdouble contour_get_factor(HosContour* contour);
guint contour_get_nlvl(HosContour* contour);

void contour_set_thres_adjustment(HosContour *self, GtkAdjustment *adjustment);
void contour_set_factor_adjustment(HosContour *self, GtkAdjustment *adjustment);
void contour_set_nlvl_adjustment(HosContour *self, GtkAdjustment *adjustment);

void contour_set_color_positive(HosContour* self,
				guint16 red_min,   guint16 red_max,
				guint16 green_min, guint16 green_max,
				guint16 blue_min,  guint16 blue_max);

void contour_set_color_negative(HosContour* self,
				guint16 red_min,   guint16 red_max,
				guint16 green_min, guint16 green_max,
				guint16 blue_min,  guint16 blue_max);

GtkAdjustment* contour_get_thres_adjustment(HosContour *self);
GtkAdjustment* contour_get_factor_adjustment(HosContour *self);
GtkAdjustment* contour_get_nlvl_adjustment(HosContour *self);

GType contour_get_type(void);

#endif  /* _HAVE_CONTOUR_H */
