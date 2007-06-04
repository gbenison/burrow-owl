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

#ifndef _HOS_HAVE_BACKING_CACHE_H
#define _HOS_HAVE_BACKING_CACHE_H

#include "hosbackingblock.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_BACKING_CACHE              (hos_backing_cache_get_type())
#define HOS_BACKING_CACHE(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_BACKING_CACHE, HosBackingCache))
#define HOS_BACKING_CACHE_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_BACKING_CACHE, HosBackingCacheClass))
#define HOS_IS_BACKING_CACHE(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_BACKING_CACHE))
#define HOS_IS_BACKING_CACHE_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_BACKING_CACHE))
#define HOS_BACKING_CACHE_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_BACKING_CACHE, HosBackingCacheClass))
					
typedef struct _HosBackingCache       HosBackingCache;
typedef struct _HosBackingCacheClass  HosBackingCacheClass;


struct _HosBackingCache
{

  HosBackingBlock parent_instance;

  gdouble *data;
  long pos;

};

struct _HosBackingCacheClass
{

  HosBackingBlockClass parent_class;
};

GType hos_backing_cache_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_BACKING_CACHE_H  */





