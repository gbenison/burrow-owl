/*
 *  Copyright (C) 2005, 2007 Greg Benison
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

#ifndef _HAVE_MARKER_H
#define _HAVE_MARKER_H

#include <glib-object.h>
#include <gtk/gtkadjustment.h>
#include "ornament.h"
#include "hoscanvas.h"

G_BEGIN_DECLS

  /* a list of styles available for markers */
  enum { MARKER_CROSS,
	 MARKER_DOT };

#define HOS_TYPE_MARKER              (hos_marker_get_type())
#define HOS_MARKER(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_MARKER, HosMarker))
#define HOS_MARKER_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_MARKER, HosMarkerClass))
#define HOS_IS_MARKER(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_MARKER))
#define HOS_IS_MARKER_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_MARKER))
#define HOS_MARKER_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_MARKER, HosMarkerClass))
					
typedef struct _HosMarker       HosMarker;
typedef struct _HosMarkerClass  HosMarkerClass;

struct _HosMarker
{
  HosOrnament parent_instance;

  GtkAdjustment *adjustment_x;
  GtkAdjustment *adjustment_y;

  guint size;
  guint style;

  gboolean movable;
  gboolean active;

};

struct _HosMarkerClass
{
  HosOrnamentClass parent_class;

  void (*grabbed) (HosMarker *marker);
  void (*moved)   (HosMarker *marker, gdouble x, gdouble y);
  void (*dropped) (HosMarker *marker, gdouble x, gdouble y);

};

void marker_set_size(HosMarker *marker, guint size);
void marker_set_movable(HosMarker *marker, gboolean movable);
void marker_set_adjustments(HosMarker *marker, GtkAdjustment *x_adjustment, GtkAdjustment *y_adjustment);
GtkAdjustment* marker_get_x_adjustment(HosMarker *marker);
GtkAdjustment* marker_get_y_adjustment(HosMarker *marker);
HosMarker* canvas_add_marker(HosCanvas *canvas);
gboolean marker_get_pos(HosMarker *self, gdouble *x, gdouble *y);

GType hos_marker_get_type(void);

G_END_DECLS

#endif /* not _HAVE_MARKER_H */
