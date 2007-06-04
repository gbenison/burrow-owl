/*
 *  Copyright (C) 2006 Greg Benison
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

#include "hosbackingcache.h"

static HosBackingBlockClass *parent_class = NULL;

static void backing_cache_seek_cur(HosBackingBlock* self, long offset);
static void backing_cache_seek_set(HosBackingBlock* self, long offset);
static gdouble backing_cache_peek(HosBacking* backing);
static void backing_cache_copy(HosBacking *src, HosBacking *dest);
static void hos_backing_cache_init(HosBackingCache *self);
static void hos_backing_cache_class_init (HosBackingCacheClass *klass);
static void backing_cache_reset(HosBacking* self);

/**********************/

static void
hos_backing_cache_init(HosBackingCache *self)
{
  self->pos = 0;
}

static void
hos_backing_cache_class_init (HosBackingCacheClass *klass)
{

  HosBackingClass *backing_class = HOS_BACKING_CLASS(klass);
  HosBackingBlockClass *backing_block_class = HOS_BACKING_BLOCK_CLASS(klass);

  parent_class = g_type_class_peek_parent (klass);

  backing_class->peek = backing_cache_peek;
  backing_class->copy = backing_cache_copy;
  backing_class->reset = backing_cache_reset;

  backing_block_class->seek_set = backing_cache_seek_set;
  backing_block_class->seek_cur = backing_cache_seek_cur;

}

GType
hos_backing_cache_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosBackingCacheClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_backing_cache_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBackingCache),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_backing_cache_init,
      };

      type = g_type_register_static (HOS_TYPE_BACKING_BLOCK,
				     "HosBackingCache",
				     &info,
				     0);
    }

  return type;
}

static void
backing_cache_seek_cur(HosBackingBlock* self, long offset)
{
  HosBackingCache* backing_cache = HOS_BACKING_CACHE(self);
  backing_cache->pos += offset;
}

static void
backing_cache_seek_set(HosBackingBlock* self, long offset)
{
  HosBackingCache* backing_cache = HOS_BACKING_CACHE(self);
  backing_cache->pos = offset;
}

static void
backing_cache_copy(HosBacking *src, HosBacking *dest)
{
  if (HOS_BACKING_CLASS(parent_class)->copy != NULL)
    HOS_BACKING_CLASS(parent_class)->copy(src, dest);

#define COPY_THE(x) (HOS_BACKING_CACHE(dest)->x) = (HOS_BACKING_CACHE(src)->x)
  COPY_THE(pos);
  COPY_THE(data);
#undef COPY_THE

}

static gdouble
backing_cache_peek(HosBacking* backing)
{
  HosBackingCache* backing_cache = HOS_BACKING_CACHE(backing);
  return (backing_cache->data)[backing_cache->pos];
}

static void
backing_cache_reset(HosBacking* self)
{
  HOS_BACKING_CACHE(self)->pos = 0;
}
