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

#ifndef _HAVE_CURSOR_H
#define _HAVE_CURSOR_H

#include <glib-object.h>
#include <gtk/gtkadjustment.h>
#include "hoscanvas.h"
#include "ornament.h"

G_BEGIN_DECLS

#define HOS_TYPE_CURSOR              (hos_cursor_get_type())
#define HOS_CURSOR(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_CURSOR, HosCursor))
#define HOS_CURSOR_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_CURSOR, HosCursorClass))
#define HOS_IS_CURSOR(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_CURSOR))
#define HOS_IS_CURSOR_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_CURSOR))
#define HOS_CURSOR_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_CURSOR, HosCursorClass))
					
typedef struct _HosCursor       HosCursor;
typedef struct _HosCursorClass  HosCursorClass;

enum {
  HORIZONTAL,
  VERTICAL
};

struct _HosCursor
{
  HosOrnament parent_instance;

  GtkAdjustment *adjustment;

  guint orientation;  /* HORIZONTAL or VERTICAL */

  gboolean movable;
  gboolean enabled;

};

struct _HosCursorClass
{
  HosOrnamentClass parent_class;

  void (*moved)(HosCursor *Cursor, gdouble position);
  void (*dropped)(HosCursor *Cursor, gdouble position);

};

void cursor_set_orientation(HosCursor *cursor, guint orientation);
GtkAdjustment* cursor_get_adjustment(HosCursor *cursor);
void cursor_set_movable(HosCursor *cursor, gboolean movable);
void cursor_set_enabled(HosCursor *cursor, gboolean enabled);
void cursor_set_adjustment(HosCursor *cursor, GtkAdjustment *adjustment);
void cursor_set_pos(HosCursor *cursor, gdouble position);
HosCursor* canvas_add_cursor_horizontal(HosCanvas *canvas);
HosCursor* canvas_add_cursor_vertical(HosCanvas *canvas);
gdouble cursor_get_position(HosCursor *cursor);

GType hos_cursor_get_type(void);

G_END_DECLS

#endif /* not _HAVE_CURSOR_H */
