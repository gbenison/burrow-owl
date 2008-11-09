/*
 *  Copyright (C) 2005, 2008 Greg Benison
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

#include "spectrum.h"
#include "spectrum_priv.h"
#include "nih.h"
#include "spectrum_transpose.h"
#include "spectrum_project.h"
#include "endian.h"
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

/*  --- header contents ----
 *
 * offsets are indices into a
 * 512-element 4-byte float array
 */

#define NIH_HDR_SIZE 512  /* number of floats in an NIH header */

#define AWOL_NP_Z 		15
#define AWOL_ORIG_Z 	12
#define AWOL_SW_Z 		11
#define AWOL_SF_Z 		10
#define AWOL_TRUNC_Z 	50
#define AWOL_SW_X 	100
#define AWOL_ORIG_X 	101
#define AWOL_NP_X 	99
#define AWOL_SF_X 	119
#define AWOL_ORIG_Y 	249
#define AWOL_SW_Y 	229
#define AWOL_SF_Y 	218
#define AWOL_NP_Y 	219
#define AWOL_TRUNC_Y 	 428
#define AWOL_TRUNC_X 	 95

/*
 * real / complex flags
 * - these are set to 1.0 if the data in that dimension has had the
 * imaginary points deleted.  = 0.0 if imaginary data is present.
 */
#define AWOL_REALFLAG_X 56
#define AWOL_REALFLAG_Y 55
#define AWOL_REALFLAG_Z 51

/*
 * Transposition flags
 * 2 == X
 * 1 == Y
 * 3 == Z
 */
#define AWOL_TRANSPOSE_X 24
#define AWOL_TRANSPOSE_Y 25
#define AWOL_TRANSPOSE_Z 26

#define IS_COMPLEX_X(_h)((_h)[AWOL_REALFLAG_X] != 1.0)
#define IS_COMPLEX_Y(_h)((_h)[AWOL_REALFLAG_Y] != 1.0)
#define IS_COMPLEX_Z(_h)((_h)[AWOL_REALFLAG_Z] != 1.0)

static void find_permutation (const int A[3], const int B[3], int result[3]);
static void permute          (float array[3], int permutation[3]);

static void nih_idx2segment  (gpointer env, guint *idx, gint *segid, gint *pt);
static void nih_read_segment (gpointer env, guint segid, gdouble *buf);

static HosSpectrum* spectrum_clip_futile_dimensions(HosSpectrum *self);

#define NIH_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_NIH, HosSpectrumNihPrivate))
#define NIH_PRIVATE(o, field) ((NIH_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumNihPrivate HosSpectrumNihPrivate;

struct _HosSpectrumNihPrivate
{
  gboolean need_swap;
  gsize    stride[3];

  float   *buffer;
  gsize    segment_size;

  int      fd;
  off_t    file_size;
};

G_DEFINE_TYPE (HosSpectrumNih, hos_spectrum_nih, HOS_TYPE_SPECTRUM_SEGMENTED)

static void
hos_spectrum_nih_class_init(HosSpectrumNihClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumSegmentedClass *segmented_class = HOS_SPECTRUM_SEGMENTED_CLASS(klass);

  segmented_class->idx2segment  = nih_idx2segment;
  segmented_class->read_segment = nih_read_segment;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumNihPrivate));
}

static void
hos_spectrum_nih_init(HosSpectrumNih *self)
{
  /*
   * FIXME
   * rather than hard-coded, the segment size could be determined
   * from stat() at object creation time to match the filesystem's IO block size.
   */
  gsize segment_size = 1024 * 4;
  NIH_PRIVATE(self, segment_size) = segment_size;
  spectrum_segmented_set_segment_size(HOS_SPECTRUM_SEGMENTED(self), segment_size);
  spectrum_segmented_set_cache_size(HOS_SPECTRUM_SEGMENTED(self), 64);
  NIH_PRIVATE(self, buffer) = g_new(float, segment_size);
  NIH_PRIVATE(self, fd) = -1;
  HOS_SPECTRUM_SEGMENTED(self)->traversal_env = NIH_GET_PRIVATE(self);
}

/*
 * fill 'result' with a permutation that transforms 'A' into 'B'.
 */
static void
find_permutation(const int A[3], const int B[3], int result[3])
{

  result[0] = -1;
  result[1] = -1;
  result[2] = -1;

  int i, j;
  for (i = 0; i < 3; ++i)
    for (j = 0; j < 3; ++j)
      if(B[i] == A[j])
	result[i] = j;

  g_assert((result[0] > -1) && (result[0] < 3));
  g_assert((result[1] > -1) && (result[1] < 3));
  g_assert((result[2] > -1) && (result[2] < 3));

}

/*
 * re-order 'array' according to 'permutation'
 */
static void
permute(float array[3], int permutation[3])
{
  float buffer[3];

  buffer[0] = array[0];
  buffer[1] = array[1];
  buffer[2] = array[2];

  int i;
  
  for (i = 0; i < 3; ++i)
    {
      g_assert(permutation[i] > -1);
      g_assert(permutation[i] < 3);
      array[i] = buffer[permutation[i]];
    }
}

static void
nih_idx2segment(gpointer env, guint *idx, gint *segid, gint *pt)
{
  HosSpectrumNihPrivate *priv = (HosSpectrumNihPrivate*)env;

  gsize point_idx =
    idx[0] * priv->stride[0] +
    idx[1] * priv->stride[1] +
    idx[2] * priv->stride[2];

  point_idx += NIH_HDR_SIZE;

  *segid = point_idx / priv->segment_size;
  *pt    = point_idx % priv->segment_size;
}

static void
nih_read_segment (gpointer env, guint segid, gdouble *buf)
{
  HosSpectrumNihPrivate *priv = (HosSpectrumNihPrivate*)env;

  /* FIXME  better error handling */  
  g_assert(priv->fd > 0);

  off_t offset = priv->segment_size * segid * sizeof(float);
  g_assert (lseek(priv->fd, offset, SEEK_SET) == offset);

  /* read */
  size_t n_remaining = priv->segment_size * sizeof(float);
  if ((n_remaining + offset) > priv->file_size)
    n_remaining = priv->file_size - offset;
  char* dest = (char*)(priv->buffer);
  while (n_remaining > 0)
    {
      size_t n_read = read(priv->fd, dest, n_remaining);
      dest += n_read;
      n_remaining -= n_read;
    }

  /* convert */
  if (priv->need_swap)
    endian_swap4(&priv->buffer, priv->segment_size);

  int i;
  for (i = 0; i < priv->segment_size; ++i)
    buf[i] = priv->buffer[i];

}


/*
 * Constructor for the NIH spectrum type.
 */
HosSpectrum*
spectrum_nih_from_file(gchar* fname)
{
  float        hdr[NIH_HDR_SIZE];
  HosSpectrum *result = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_NIH, NULL));

  HosSpectrumNihPrivate *priv = NIH_GET_PRIVATE(result);

  /* read header data -- FIXME error checking */
  GError *file_error = NULL;
  GIOChannel *channel = g_io_channel_new_file(fname, "r", &file_error);

  if (channel == NULL)
    return NULL;

  priv->fd = open(fname, O_RDONLY);
  struct stat statbuf;
  g_assert (fstat(priv->fd, &statbuf) == 0);
  priv->file_size = statbuf.st_size;

  g_io_channel_set_encoding(channel, NULL, &file_error);

  gsize header_size_bytes = sizeof(float) * NIH_HDR_SIZE;
  gsize bytes_read;
  g_io_channel_read_chars(channel, (gchar*)hdr, header_size_bytes, &bytes_read, &file_error);

  g_io_channel_close(channel);
  channel = NULL;

  if (header_size_bytes != bytes_read)
    return NULL;

  HOS_SPECTRUM_NIH(result)->fname = g_strdup(fname);

  /* checks for endian-ness based on sanity of header values */
  if ((hdr[AWOL_SF_X] > 1000) || (hdr[AWOL_SF_X] < 1))
    {
      endian_swap4(hdr, NIH_HDR_SIZE);
      priv->need_swap = TRUE;
    }
  else
    priv->need_swap = FALSE;

  /* sanity checks after byte swapping */
  g_assert(!((hdr[AWOL_SF_X] > 1000) || (hdr[AWOL_SF_X] < 1)));
  g_assert(!((hdr[AWOL_NP_X] > 1e6) || (hdr[AWOL_NP_X] < 1)));

  /*
   * check dimension transposition order 
   */
  int transpose_flags[3] = {
    (int)hdr[AWOL_TRANSPOSE_X],
    (int)hdr[AWOL_TRANSPOSE_Y],
    (int)hdr[AWOL_TRANSPOSE_Z]
  };

  static const int transpose_native[3] = {2, 1, 3};
  int permutation[3];
  find_permutation(transpose_native, transpose_flags, permutation);

  float sf[3] = {
    hdr[AWOL_SF_X],
    hdr[AWOL_SF_Y],
    hdr[AWOL_SF_Z]
  };

  float sw[3] = {
    hdr[AWOL_SW_X],
    hdr[AWOL_SW_Y],
    hdr[AWOL_SW_Z]
  };

  float orig[3] = {
    hdr[AWOL_ORIG_X],
    hdr[AWOL_ORIG_Y],
    hdr[AWOL_ORIG_Z]
  };

  float np[3] = {
    hdr[AWOL_NP_X],
    (IS_COMPLEX_X(hdr) && IS_COMPLEX_Y(hdr)) ? hdr[AWOL_NP_Y] / 2 : hdr[AWOL_NP_Y],
    IS_COMPLEX_Z(hdr) ? hdr[AWOL_NP_Z] / 2 : hdr[AWOL_NP_Z]
  };

  permute(sf,   permutation);
  permute(sw,   permutation);
  permute(orig, permutation);

  /* set up dimensions */
  GList *dimensions = NULL;
  gint i;
  for (i = 0; i < 3; ++i)
    {
      dimension_t* dimen = g_new0(dimension_t, 1);
      dimen->np   = np[i];
      dimen->sw   = sw[i];
      dimen->sf   = sf[i];
      dimen->orig = orig[i] + sw[i] * ((np[i] - 1.0) / np[i]);
      dimensions = g_list_append(dimensions, dimen);
    }

  spectrum_set_dimensions(HOS_SPECTRUM(result), dimensions);

  /* X */
  priv->stride[0] = 1;
  gsize cumulative_stride = np[0];
  if (IS_COMPLEX_X(hdr))
    cumulative_stride *= 2;

  /* Y */
  if (IS_COMPLEX_Y(hdr))
    cumulative_stride *= 2;
  priv->stride[1] = cumulative_stride;
  cumulative_stride *= np[1];

  /* Z */
  if (IS_COMPLEX_Z(hdr))
    cumulative_stride *= 2;
  priv->stride[2] = cumulative_stride;
  cumulative_stride *= np[2];

  /* Trim away second and third dimensions if they have only one point. */
  result = spectrum_clip_futile_dimensions(result);
  result = spectrum_clip_futile_dimensions(result);

  return result;
}

/*
 * If final dimension of 'self' is futile i.e. has only one point,
 * return a spectrum having all but the last dimesion; otherwise
 * just return 'self'.
 */
static HosSpectrum*
spectrum_clip_futile_dimensions(HosSpectrum *self)
{
  int ndim = spectrum_ndim(self);
  if (spectrum_np(self, ndim - 1) > 1)
    return self;
  self = spectrum_transpose(self, ndim - 1);
  self = spectrum_project(self, 0);

  return self;
}




