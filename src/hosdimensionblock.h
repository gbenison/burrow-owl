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

#ifndef _HOS_HAVE_DIMENSION_BLOCK_H
#define _HOS_HAVE_DIMENSION_BLOCK_H

#include "hosdimension.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_DIMENSION_BLOCK              (hos_dimension_block_get_type())
#define HOS_DIMENSION_BLOCK(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_DIMENSION_BLOCK, HosDimensionBlock))
#define HOS_DIMENSION_BLOCK_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_DIMENSION_BLOCK, HosDimensionBlockClass))
#define HOS_IS_DIMENSION_BLOCK(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_DIMENSION_BLOCK))
#define HOS_IS_DIMENSION_BLOCK_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_DIMENSION_BLOCK))
#define HOS_DIMENSION_BLOCK_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_DIMENSION_BLOCK, HosDimensionBlockClass))
					
typedef struct _HosDimensionBlock       HosDimensionBlock;
typedef struct _HosDimensionBlockClass  HosDimensionBlockClass;

struct _HosDimensionBlock
{
  HosDimension parent_instance;

  guint np_physical;
  guint stride;
  guint initial_offset;
  guint pointer_physical;
  
  guint *schedule;
  guint *schedule_ptr;

  gdouble sw_physical;

  gboolean negated_initially;
  gboolean negate_on_fold;
  gboolean fold_allowed;
  gboolean negated;

};

struct _HosDimensionBlockClass
{
  HosDimensionClass parent_class;
};

extern void dimension_block_unfold(HosDimensionBlock *self,
				   const guint downfield,
				   const guint upfield,
				   const gboolean negate_on_fold);

GType hos_dimension_block_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_DIMENSION_BLOCK_H  */





