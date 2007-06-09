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

#include "hosbackingfile.h"

#define DEFAULT_BUF_SIZE 512

typedef float specdata_t;

static HosBackingBlockClass *parent_class = NULL;

static void hos_backing_file_init(HosBackingFile *self);
static void hos_backing_file_class_init (HosBackingFileClass *klass);
static gdouble backing_file_peek(HosBacking* self);
static void backing_file_seek_cur(HosBackingBlock* self, long offset);
static void backing_file_seek_set(HosBackingBlock* self, long offset);
static void verify_filebuf(HosBackingFile* self);
static void backing_file_copy(HosBacking *src, HosBacking *dest);
static void backing_file_reset(HosBacking* self);


static void
hos_backing_file_init(HosBackingFile *self)
{
  self->needs_swap = 0;
}

static void
hos_backing_file_class_init (HosBackingFileClass *klass)
{
  /* GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
  HosBackingClass *backing_class = HOS_BACKING_CLASS(klass);
  HosBackingBlockClass *backing_block_class = HOS_BACKING_BLOCK_CLASS(klass);
  
  parent_class = g_type_class_peek_parent (klass);

  /*
  gobject_class->set_property = hos_backing_file_set_property;
  gobject_class->get_property = hos_backing_file_get_property;
  */

  backing_class->peek = backing_file_peek;
  backing_class->copy = backing_file_copy;
  backing_class->reset = backing_file_reset;

  backing_block_class->seek_set = backing_file_seek_set;
  backing_block_class->seek_cur = backing_file_seek_cur;

}

GType
hos_backing_file_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosBackingFileClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_backing_file_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBackingFile),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_backing_file_init,
      };

      type = g_type_register_static (HOS_TYPE_BACKING_BLOCK,
				     "HosBackingFile",
				     &info,
				     0);
    }

  return type;
}


static gdouble
backing_file_peek(HosBacking* self)
{
  specdata_t result;
  HosBackingFile* backing_file = HOS_BACKING_FILE(self);

  verify_filebuf(backing_file);
  filebuf_peek(backing_file->channel, &result, sizeof(specdata_t));

  return result;
}

static void
backing_file_seek_cur(HosBackingBlock* self, long offset)
{
  HosBackingFile* backing_file = HOS_BACKING_FILE(self);
  verify_filebuf(backing_file);
  filebuf_seek_cur(backing_file->channel, offset * sizeof(specdata_t));
}

static void
backing_file_seek_set(HosBackingBlock* self, long offset)
{
  HosBackingFile* backing_file = HOS_BACKING_FILE(self);
  verify_filebuf(backing_file);
  filebuf_seek_set(backing_file->channel, offset * sizeof(specdata_t));
}

static void
verify_filebuf(HosBackingFile* self)
{
  if (self->channel == NULL)
    {
      self->channel = filebuf_new_file(self->fname, DEFAULT_BUF_SIZE);
      filebuf_seek_set(self->channel, self->hdr_size);
    }
}

static void
backing_file_copy(HosBacking *src, HosBacking *dest)
{
  if (HOS_BACKING_CLASS(parent_class)->copy != NULL)
    HOS_BACKING_CLASS(parent_class)->copy(src, dest);

  HOS_BACKING_FILE(dest)->fname = g_strdup(HOS_BACKING_FILE(src)->fname);
  HOS_BACKING_FILE(dest)->channel = NULL;
#define COPY_THE(x) (HOS_BACKING_FILE(dest)->x) = (HOS_BACKING_FILE(src)->x)
  COPY_THE(hdr_size);
#undef COPY_THE

}

static void
backing_file_reset(HosBacking* self)
{
  HosBackingFile *backing_file = HOS_BACKING_FILE(self);

  if (backing_file->channel == NULL)
    backing_file->channel = filebuf_new_file(backing_file->fname, DEFAULT_BUF_SIZE);

  filebuf_seek_set(backing_file->channel, backing_file->hdr_size);
}
