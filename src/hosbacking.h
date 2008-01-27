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

#ifndef _HOS_HAVE_BACKING_H
#define _HOS_HAVE_BACKING_H

#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define HOS_TYPE_BACKING              (hos_backing_get_type())
#define HOS_BACKING(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_BACKING, HosBacking))
#define HOS_BACKING_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_BACKING, HosBackingClass))
#define HOS_IS_BACKING(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_BACKING))
#define HOS_IS_BACKING_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_BACKING))
#define HOS_BACKING_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_BACKING, HosBackingClass))
					
typedef struct _HosBacking       HosBacking;
typedef struct _HosBackingClass  HosBackingClass;


struct _HosBacking
{

  GObject parent_instance;
  gboolean negated;

};

typedef gdouble (*PeekFunc)(HosBacking*);

struct _HosBackingClass
{

  GObjectClass parent_class;

  gdouble (*peek)          (HosBacking*);
  void    (*copy)          (HosBacking*, HosBacking*);
  void    (*reset)         (HosBacking*);

};

extern HosBacking* backing_copy       (HosBacking*);
extern void        backing_negate     (HosBacking*);
extern void        backing_reset      (HosBacking*, gpointer);
extern void        backing_accumulate (HosBacking*, gdouble*);

GType hos_backing_get_type (void);

G_END_DECLS

#endif /* not  _HOS_HAVE_BACKING_H  */





