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

/*
 * this is a compatibility macro; it must come before any header
 * includes.  It defines which features of the C library will be
 * available.  In this file it is needed for proper round() behavior
 * (at least).
 */
#define _GNU_SOURCE

#include <assert.h>
#include <math.h>
#include "hosbackingblock.h"


static HosBackingClass *parent_class = NULL;

static void hos_backing_block_class_init (HosBackingBlockClass *klass);
static void hos_backing_block_init(HosBackingBlock*);
static void backing_block_copy(HosBacking *src, HosBacking *dest);

GType
hos_backing_block_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosBackingBlockClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_backing_block_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBackingBlock),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_backing_block_init,
      };

      type = g_type_register_static (HOS_TYPE_BACKING,
				     "HosBackingBlock",
				     &info,
				     0);
    }

  return type;
}

static void
hos_backing_block_class_init (HosBackingBlockClass *klass)
{
  HosBackingClass *backing_class = HOS_BACKING_CLASS(klass);
  
  parent_class = g_type_class_peek_parent (klass);

  backing_class->copy = backing_block_copy;

}

static void
hos_backing_block_init(HosBackingBlock  *self)
{
  /* FIXME */
}


static void
backing_block_copy(HosBacking *src, HosBacking *dest)
{
  if (HOS_BACKING_CLASS(parent_class)->copy != NULL)
    HOS_BACKING_CLASS(parent_class)->copy(src, dest);

}

void
backing_block_seek_cur(HosBackingBlock* self, long offset)
{
  HosBackingBlockClass *class = HOS_BACKING_BLOCK_GET_CLASS(self);
  class->seek_cur(self, offset);
}

void
backing_block_seek_set(HosBackingBlock* self, long offset)
{
  HosBackingBlockClass *class = HOS_BACKING_BLOCK_GET_CLASS(self);
  class->seek_set(self, offset);
}

