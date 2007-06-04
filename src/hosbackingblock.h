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

#ifndef _HOS_HAVE_BACKING_BLOCK_H
#define _HOS_HAVE_BACKING_BLOCK_H

#include "hosbacking.h"

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define HOS_TYPE_BACKING_BLOCK              (hos_backing_block_get_type())
#define HOS_BACKING_BLOCK(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_BACKING_BLOCK, HosBackingBlock))
#define HOS_BACKING_BLOCK_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_BACKING_BLOCK, HosBackingBlockClass))
#define HOS_IS_BACKING_BLOCK(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_BACKING_BLOCK))
#define HOS_IS_BACKING_BLOCK_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_BACKING_BLOCK))
#define HOS_BACKING_BLOCK_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_BACKING_BLOCK, HosBackingBlockClass))
					
typedef struct _HosBackingBlock       HosBackingBlock;
typedef struct _HosBackingBlockClass  HosBackingBlockClass;


struct _HosBackingBlock
{

  HosBacking parent_instance;

};

struct _HosBackingBlockClass
{

  HosBackingClass parent_class;

  void(*seek_set)(HosBackingBlock* self, long offset);
  void(*seek_cur)(HosBackingBlock* self, long offset);

};

void backing_block_seek_cur(HosBackingBlock*, long);
void backing_block_seek_set(HosBackingBlock*, long);


GType hos_backing_block_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_BACKING_BLOCK_H  */





