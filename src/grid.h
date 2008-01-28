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

#ifndef HAVE_GRID_H
#define HAVE_GRID_H

#include "canvasitem.h"

G_BEGIN_DECLS

#define HOS_TYPE_GRID             (hos_grid_get_type())
#define HOS_GRID(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj), HOS_TYPE_GRID, HosGrid))
#define HOS_GRID_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_GRID, HosGridClass)
#define HOS_IS_GRID(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj), HOS_TYPE_GRID))
#define HOS_IS_GRID_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass), HOS_TYPE_GRID))
#define HOS_GRID_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_GRID, HosGridClass))

typedef struct _HosGrid      HosGrid;
typedef struct _HosGridClass HosGridClass;

struct _HosGridClass
{
  HosCanvasItemClass parent_class;
};

struct _HosGrid
{
  HosCanvasItem parent_instance;

  gint          auto_spacing;

  gdouble       spacing_horizontal;
  gdouble       anchor_horizontal;
  gdouble       spacing_vertical;
  gdouble       anchor_vertical;

};

GType        hos_grid_get_type (void);


G_END_DECLS


#endif /* not HAVE_GRID_H */
