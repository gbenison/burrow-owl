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

#ifndef _HAVE_CONTOUR_COLOR_H
#define _HAVE_CONTOUR_COLOR_H

#include <gdk/gdk.h>
#include <burrow/spectrum/contour.h>

G_BEGIN_DECLS

#define HOS_TYPE_CONTOUR_COLOR            (hos_contour_color_get_type())
#define HOS_CONTOUR_COLOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
								       HOS_TYPE_CONTOUR_COLOR, HosContourColor))
#define HOS_CONTOUR_COLOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), \
								    HOS_TYPE_CONTOUR_COLOR, HosContourColorClass))
#define HOS_IS_CONTOUR_COLOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_CONTOUR_COLOR))
#define HOS_IS_CONTOUR_COLOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_CONTOUR_COLOR))
#define HOS_CONTOUR_COLOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), \
								      HOS_TYPE_CONTOUR_COLOR, HosContourColorClass))

/**
 * @ingroup HosContourColor
 * @brief   Contour properties including colors
 */
typedef struct _HosContourColor       HosContourColor;
typedef struct _HosContourColorClass  HosContourColorClass;

struct _HosContourColorClass
{
  HosContourClass parent_class;
};

struct _HosContourColor
{
  HosContour  parent_instance;

  GdkColor *color_positive_low;
  GdkColor *color_positive_high;

  GdkColor *color_negative_low;
  GdkColor *color_negative_high;
};

GType hos_contour_color_get_type(void);

GdkColor* contour_get_color(HosContour *self, gint lvl);

G_END_DECLS

#endif /* not _HAVE_CONTOUR_COLOR_H */

