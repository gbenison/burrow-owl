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
 * An ornament is a visible object that appears on a canvas.
 * Examples: peak labels, cursors, and markers.
 */

#ifndef _HAVE_ORNAMENT_H
#define _HAVE_ORNAMENT_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <burrow/spectrum.h>

typedef struct _HosOrnament       HosOrnament;
typedef struct _HosOrnamentClass  HosOrnamentClass;


#include "hoscanvas.h"


/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_ORNAMENT              (hos_ornament_get_type())
#define HOS_ORNAMENT(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_ORNAMENT, HosOrnament))
#define HOS_ORNAMENT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_ORNAMENT, HosOrnamentClass))
#define HOS_IS_ORNAMENT(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_ORNAMENT))
#define HOS_IS_ORNAMENT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_ORNAMENT))
#define HOS_ORNAMENT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_ORNAMENT, HosOrnamentClass))



struct _HosOrnamentClass
{
  GObjectClass parent_class;

  void (*paint)(HosOrnament *ornament);

  void (*set_pos)(HosOrnament *ornament, gdouble x, gdouble y);
  void (*sync_region)(HosOrnament *self);
  gboolean (*overlap_region)(HosOrnament *ornament,
			     gdouble x1,
			     gdouble y1,
			     gdouble xn,
			     gdouble yn);

  gboolean (*point_overlap)(HosOrnament *ornament,
			    gdouble x, gdouble y);

  void (*acquire)(HosOrnament *ornament);
  void (*motion_event)(HosOrnament *ornament, gdouble x, gdouble y);
  void (*release)(HosOrnament *ornament);

};

struct _HosOrnament
{
  GObject parent_instance;

  GdkRegion *region;
  HosCanvas *canvas;
  gulong group_id;

};



gboolean ornament_overlap_region (HosOrnament *ornament,
				  gdouble x1,
				  gdouble y1,
				  gdouble xn,
				  gdouble yn);

void ornament_redraw(HosOrnament *self);
void ornament_release(HosOrnament *self);
void ornament_move (HosOrnament *ornament, gdouble x, gdouble y);
gboolean ornament_test_grab(HosOrnament *self, gdouble x_ppm, gdouble y_ppm);
gulong ornament_get_group_id(HosOrnament *self);
void ornament_set_group_id(HosOrnament *self, gulong id);

void ornament_set_region(HosOrnament *self, GdkRegion* region);
void ornament_invalidate_region(HosOrnament *self);
void ornament_sync_region(HosOrnament *self);
void ornament_pick_up(HosOrnament* ornament);

GtkAdjustment* adjustment_for_spectrum(HosSpectrum *spec, guint dim);


GType hos_ornament_get_type (void);


#endif  /* HAVE_ORNAMENT_H */

