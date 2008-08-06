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
 * We love stdio, but can't figure out how to make it work well with
 * this program.
 * So we create a little wrapper around stdio, to do custom buffering.
 *
 * I guess this won't handle certain things well, like having the
 * file written to while this program is using it.  But if you're
 * doing that then you have greater problems.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "filebuf.h"

#define DEBUG 0
static int debug = 0;

struct _filebuf_struct
{
  FILE *channel;
  char *buf;

  /* the position pointer; offset in bytes from start of file */
  unsigned int position_pointer;

  /* the first byte of the buffer corresponds to this position in the file. */
  unsigned int buffer_start_index;

  /* maximum buffer size; will attempt to read this many bytes on each read */
  int buffer_max;

  /* number of bytes that have actually been read into the buffer. */
  unsigned int n_in_buffer;


};

static void filebuf_set_size(FileBuf *fb, const int size);

FileBuf*
filebuf_new_file(const char* fname, int size)
{
  FileBuf *result = (FileBuf*)calloc(sizeof(FileBuf), 1);

  assert(result);
  result->channel = fopen(fname, "r");

  filebuf_set_size(result, size);

  return result;

}

static void
filebuf_set_size(FileBuf *fb, const int size)
{
  if (fb->buffer_max < size)
    fb->buf = (char*)realloc(fb->buf, size);
  fb->buffer_max = size;
}

/*
 * Check to see if the buffer is synchronized
 * with the current position;
 * if the current position is out of the buffer's range,
 * update the buffer.
 *
 * make sure buffer is prepared to deliver 'length' bytes.
 *
 * returns --
 *  0: no buffer bump necessary
 *  1: buffer was bumped
 */
static int
check_sync(FileBuf *self, int length)
{
  if (self->position_pointer < self->buffer_start_index)
    { goto bump; }
  if ((self->position_pointer + length) > (self->buffer_start_index + self->n_in_buffer))
    { goto bump; }

  /* get here-- everything's ok! */
  return 0;

 bump:
#ifdef DEBUG
  if (debug == 1)
    {
      fprintf(stderr, "--> Sync: start=%lx\n    pos=%lx\n   length=%d\n",
	      self->buffer_start_index,
	      self->position_pointer,
	      length);
    }
#endif
  self->buffer_start_index = self->position_pointer;
  fseek(self->channel, self->position_pointer, SEEK_SET);
  self->n_in_buffer =
    fread(self->buf, 1, self->buffer_max, self->channel);

  /* FIXME handle errors */
  assert((int)self->n_in_buffer >= length);

  return 1;

}

/*
 * Copies 'n' bytes from the filebuf at current position
 * to dest, without moving the filebuf's position pointer
 */
void
filebuf_peek(FileBuf *fb, void *dest, int n)
{
  int offset;

  check_sync(fb, n);

  offset = fb->position_pointer - fb->buffer_start_index;
  memcpy(dest, fb->buf + offset, n);
}

void
filebuf_seek_set(FileBuf *fb, unsigned int offset)
{
  fb->position_pointer = offset;
}

void
filebuf_seek_cur(FileBuf *fb, int offset)
{
  fb->position_pointer += offset;
}
