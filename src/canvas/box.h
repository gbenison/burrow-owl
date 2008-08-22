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

#ifndef _HAVE_BOX_H
#define _HAVE_BOX_H

#include <glib-object.h>
#include <gtk/gtkadjustment.h>
#include "ornament.h"
#include "hoscanvas.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_BOX              (hos_box_get_type())
#define HOS_BOX(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_BOX, HosBox))
#define HOS_BOX_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_BOX, HosBoxClass))
#define HOS_IS_BOX(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_BOX))
#define HOS_IS_BOX_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_BOX))
#define HOS_BOX_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_BOX, HosBoxClass))
					
typedef struct _HosBox       HosBox;
typedef struct _HosBoxClass  HosBoxClass;

struct _HosBox
{
  HosOrnament parent_instance;

  gdouble x1, y1, xn, yn;
  gdouble save_x, save_y;

  gboolean active_x1, active_y1, active_xn, active_yn;
  gboolean movable;

};

struct _HosBoxClass
{
  HosOrnamentClass parent_class;

  void (*grabbed)(HosBox *box);
  void (*moved)(HosBox *box, gdouble x1, gdouble y1, gdouble xn, gdouble yn);
  void (*dropped)(HosBox *box, gdouble x1, gdouble y1, gdouble xn, gdouble yn);

};

gdouble box_get_x1(HosBox *box);
gdouble box_get_xn(HosBox *box);
gdouble box_get_y1(HosBox *box);
gdouble box_get_yn(HosBox *box);
HosBox* canvas_add_box(HosCanvas *canvas);

GType hos_box_get_type(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not _HAVE_BOX_H */
