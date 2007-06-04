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

#ifndef _HOS_HAVE_DIMENSION_H
#define _HOS_HAVE_DIMENSION_H

#include <glib-object.h>
#include "hosbacking.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_DIMENSION              (hos_dimension_get_type())
#define HOS_DIMENSION(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_DIMENSION, HosDimension))
#define HOS_DIMENSION_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_DIMENSION, HosDimensionClass))
#define HOS_IS_DIMENSION(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_DIMENSION))
#define HOS_IS_DIMENSION_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_DIMENSION))
#define HOS_DIMENSION_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_DIMENSION, HosDimensionClass))
					
typedef struct _HosDimension       HosDimension;
typedef struct _HosDimensionClass  HosDimensionClass;

struct _HosDimension
{
  GObject parent_instance;

  guint np;
  gdouble orig;
  gdouble sf;
  gdouble sw;

  gboolean integrated;

  HosBacking *backing;

  /* stateful members- used during traversal */
  gsize iterate_index;
  gboolean multiplier;
  gsize buffer_stride;
  gdouble cost;

};

typedef void(*BackingManipulateFunc)(HosDimension*, HosBacking*);
typedef void(*LimitManipulateFunc)(HosDimension*, const guint);
typedef void(*DimenCopyFunc)(HosDimension*, HosDimension*);
typedef gdouble(*DimenCostFunc)(HosDimension*);
typedef void(*InterpolateFunc)(HosDimension*, const guint);
typedef void(*IncrementFunc)(HosDimension*, HosBacking*);
typedef void(*ResetFunc)(HosDimension*, HosBacking*);
typedef void(*void_func_dimen_guint)(HosDimension*, const guint);

struct _HosDimensionClass
{
  GObjectClass parent_class;

  void (*copy)(HosDimension*, HosDimension*);

  void (*clip_upper)(HosDimension*, const guint);
  void (*clip_lower)(HosDimension*, const guint);
  void (*interpolate)(HosDimension*, const guint);

  void (*increment)(HosDimension*, HosBacking*);
  void (*reset)(HosDimension*, HosBacking*);
  void (*prime)(HosDimension*, HosBacking*);

  gdouble (*cost)(HosDimension*);

};

extern void dimension_cost(HosDimension*, gdouble*);
extern guint dimension_np(HosDimension*);
extern gdouble dimension_sw(HosDimension*);
extern gdouble dimension_sf(HosDimension*);
extern gdouble dimension_orig(HosDimension*);
extern gdouble dimension_hz2pt(HosDimension*, const gdouble);
extern gdouble dimension_pt2hz(HosDimension*, const gdouble);
extern gdouble dimension_ppm2pt(HosDimension*, const gdouble);
extern gdouble dimension_pt2ppm(HosDimension*, const gdouble);

extern void dimension_clip_lower(HosDimension*, const guint);
extern void dimension_extract(HosDimension*, const gdouble, const gdouble);
extern void dimension_extract_ppm(HosDimension*, const gdouble, const gdouble);
extern void dimension_interpolate(HosDimension*, gpointer);
extern void dimension_reset(HosDimension*, gpointer);
extern void dimension_prime(HosDimension*, gpointer);
extern void dimension_increment(HosDimension*, gboolean*);
extern void dimension_integrate(HosDimension*);

extern GList* dimen_list_copy(GList* dimens);

GType hos_dimension_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_DIMENSION_H  */





