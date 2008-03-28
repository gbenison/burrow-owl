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

#ifndef _HAVE_LINE_H
#define _HAVE_LINE_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include <burrow/spectrum.h>
#include "canvasitem.h"

typedef struct _HosLine       HosLine;
typedef struct _HosLineClass  HosLineClass;

G_BEGIN_DECLS

#define HOS_TYPE_LINE              (hos_line_get_type())
#define HOS_LINE(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_LINE, HosLine))
#define HOS_LINE_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_LINE, HosLineClass))
#define HOS_IS_LINE(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_LINE))
#define HOS_IS_LINE_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_LINE))
#define HOS_LINE_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_LINE, HosLineClass))


struct _HosLineClass
{
  HosCanvasItemClass parent_class;

  void       (*paint)             (HosLine *line, HosCanvas *canvas);
  gboolean   (*point_in)          (HosLine *self, gint x, gint y);
  void       (*acquire)           (HosLine *line);
  void       (*release)           (HosLine *line);
  void       (*enter)             (HosLine *line);
  void       (*leave)             (HosLine *line);
  void       (*move_relative)     (HosLine *line, gdouble dx, gdouble dy);

};

typedef struct {gdouble x; gdouble y;} hos_line_point_t;

struct _HosLine
{
  HosCanvasItem parent_instance;

  gboolean mouse_over;

  GArray* points;

};

void   line_set_points   (HosLine* line, double* x, double* y, guint np);
guint  line_append_point (HosLine* line, double x, double y);

GType hos_line_get_type (void);


G_END_DECLS


#endif  /* HAVE_LINE_H */




