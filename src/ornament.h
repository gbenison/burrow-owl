/*
 *  Copyright (C) 2006, 2007 Greg Benison
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
#include <burrow/spectrum.h>
#include "canvasitem.h"

typedef struct _HosOrnament       HosOrnament;
typedef struct _HosOrnamentClass  HosOrnamentClass;

G_BEGIN_DECLS

#define HOS_TYPE_ORNAMENT              (hos_ornament_get_type())
#define HOS_ORNAMENT(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_ORNAMENT, HosOrnament))
#define HOS_ORNAMENT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_ORNAMENT, HosOrnamentClass))
#define HOS_IS_ORNAMENT(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_ORNAMENT))
#define HOS_IS_ORNAMENT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_ORNAMENT))
#define HOS_ORNAMENT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_ORNAMENT, HosOrnamentClass))


struct _HosOrnamentClass
{
  HosCanvasItemClass parent_class;

  void       (*paint)             (HosOrnament *ornament, HosCanvas *canvas);
  GdkRegion* (*calculate_region)  (HosOrnament *ornament);
  void       (*acquire)           (HosOrnament *ornament);
  void       (*release)           (HosOrnament *ornament);
  void       (*configure)         (HosOrnament *ornament);
  void       (*motion_event)      (HosOrnament *ornament, gdouble x, gdouble y);
  void       (*move_relative)     (HosOrnament *ornament, gdouble dx, gdouble dy);

};

struct _HosOrnament
{
  HosCanvasItem parent_instance;

  gboolean mouse_over;
  GdkRegion *region;

  gdouble save_x;
  gdouble save_y;
};

void ornament_release(HosOrnament *self);
void ornament_move (HosOrnament *ornament, gdouble x, gdouble y);
gboolean ornament_test_grab(HosOrnament *self, gdouble x_ppm, gdouble y_ppm);

void ornament_set_region(HosOrnament *self, GdkRegion* region);
void ornament_invalidate_region(HosOrnament *self);
void ornament_pick_up(HosOrnament* ornament);

void ornament_configure(HosOrnament* ornament);

GtkAdjustment* adjustment_for_spectrum(HosSpectrum *spec, guint dim);

GType hos_ornament_get_type (void);


G_END_DECLS


#endif  /* HAVE_ORNAMENT_H */

