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

#ifndef _HAVE_PAINTER_BWPS_H
#define _HAVE_PAINTER_BWPS_H

#include <stdio.h>
#include "painter.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_PAINTER_BWPS              (hos_painter_bwps_get_type())
#define HOS_PAINTER_BWPS(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_PAINTER_BWPS, HosPainterBwps))
#define HOS_PAINTER_BWPS_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_PAINTER_BWPS, HosPainterBwpsClass))
#define HOS_IS_PAINTER_BWPS(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_PAINTER_BWPS))
#define HOS_IS_PAINTER_BWPS_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_PAINTER_BWPS))
#define HOS_PAINTER_BWPS_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_PAINTER_BWPS, HosPainterBwpsClass))
					
typedef struct _HosPainterBwps       HosPainterBwps;
typedef struct _HosPainterBwpsClass  HosPainterBwpsClass;

struct _HosPainterBwps
{
  HosPainter parent_instance;

  FILE *channel;
};

struct _HosPainterBwpsClass
{
  HosPainterClass parent_class;
};

HosPainterBwps* painter_bwps_new_file(gchar* fname);

GType hos_painter_bwps_get_type(void);

extern int bwps_anergize;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not _HAVE_PAINTER_BWPS_H */
