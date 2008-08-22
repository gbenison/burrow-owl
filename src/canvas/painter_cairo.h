/*
 *  Copyright (C) 2007 Greg Benison
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

#ifndef _HAVE_PAINTER_CAIRO_H
#define _HAVE_PAINTER_CAIRO_H

#include <stdio.h>
#include <gdk/gdk.h>
#include "painter.h"

G_BEGIN_DECLS

#define HOS_TYPE_PAINTER_CAIRO              (hos_painter_cairo_get_type())
#define HOS_PAINTER_CAIRO(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_PAINTER_CAIRO, HosPainterCairo))
#define HOS_PAINTER_CAIRO_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_PAINTER_CAIRO, HosPainterCairoClass))
#define HOS_IS_PAINTER_CAIRO(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_PAINTER_CAIRO))
#define HOS_IS_PAINTER_CAIRO_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_PAINTER_CAIRO))
#define HOS_PAINTER_CAIRO_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_PAINTER_CAIRO, HosPainterCairoClass))
					
typedef struct _HosPainterCairo       HosPainterCairo;
typedef struct _HosPainterCairoClass  HosPainterCairoClass;

struct _HosPainterCairo
{
  HosPainter parent_instance;
  cairo_t* context;
};

struct _HosPainterCairoClass
{
  HosPainterClass parent_class;
};

HosPainterCairo* painter_cairo_new(void);
void painter_cairo_set_context(HosPainterCairo *self, cairo_t *context);
GType hos_painter_cairo_get_type(void);

G_END_DECLS

#endif /* not _HAVE_PAINTER_CAIRO_H */
