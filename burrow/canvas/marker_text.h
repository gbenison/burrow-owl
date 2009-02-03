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

#ifndef _HAVE_MARKER_TEXT_H
#define _HAVE_MARKER_TEXT_H

#include <glib-object.h>
#include <gtk/gtkadjustment.h>
#include "marker.h"

G_BEGIN_DECLS

#define HOS_TYPE_MARKER_TEXT              (hos_marker_text_get_type())
#define HOS_MARKER_TEXT(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_MARKER_TEXT, HosMarkerText))
#define HOS_MARKER_TEXT_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_MARKER_TEXT, HosMarkerTextClass))
#define HOS_IS_MARKER_TEXT(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_MARKER_TEXT))
#define HOS_IS_MARKER_TEXT_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_MARKER_TEXT))
#define HOS_MARKER_TEXT_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_MARKER_TEXT, HosMarkerClass))
					
/**
 * @ingroup HosMarkerText
 * @brief   A marker with a text label
 */
typedef struct _HosMarkerText       HosMarkerText;
typedef struct _HosMarkerTextClass  HosMarkerTextClass;

struct _HosMarkerText
{
  HosMarker parent_instance;

  PangoLayout *layout;

  gdouble   patch_width, patch_height;
  gchar    *text;
  GdkColor *text_color;
};

struct _HosMarkerTextClass
{
  HosMarkerClass parent_class;
};

void           marker_text_set_label  (HosMarkerText* marker_text, const gchar *text);
HosMarkerText* canvas_add_marker_text (HosCanvas *canvas, const gchar *text);
void           marker_text_set_patch  (HosMarkerText* self, gdouble width, gdouble height);

GType hos_marker_text_get_type(void);

G_END_DECLS

#endif /* not _HAVE_MARKER_TEXT_H */
