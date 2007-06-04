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
 * the "black and white postscript" painter type.
 * it ignores the 'level' argument of init_line and paints
 * everything in one shade.
 */

#ifndef _HAVE_PAINTER_GDK_H
#define _HAVE_PAINTER_GDK_H

#include <stdio.h>
#include <gdk/gdk.h>
#include "painter.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_PAINTER_GDK              (hos_painter_gdk_get_type())
#define HOS_PAINTER_GDK(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_PAINTER_GDK, HosPainterGdk))
#define HOS_PAINTER_GDK_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_PAINTER_GDK, HosPainterGdkClass))
#define HOS_IS_PAINTER_GDK(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_PAINTER_GDK))
#define HOS_IS_PAINTER_GDK_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_PAINTER_GDK))
#define HOS_PAINTER_GDK_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_PAINTER_GDK, HosPainterGdkClass))
					
typedef struct _HosPainterGdk       HosPainterGdk;
typedef struct _HosPainterGdkClass  HosPainterGdkClass;

struct _HosPainterGdk
{
  HosPainter parent_instance;

  GdkDrawable *drawable;
  GdkGC *gc;

};

struct _HosPainterGdkClass
{
  HosPainterClass parent_class;
};

HosPainterGdk* painter_gdk_new(void);
void painter_gdk_set_drawable_gc(HosPainterGdk *self, GdkDrawable *drawable, GdkGC *gc);

GType hos_painter_gdk_get_type(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not _HAVE_PAINTER_GDK_H */
