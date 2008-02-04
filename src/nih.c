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

#include <stdio.h>
#include <assert.h>
#include <burrow/nih.h>
#include "hosdimension.h"
#include "hosdimensionblock.h"
#include "hosbackingblock.h"
#include "hosbackingfile.h"
#include "endian.h"

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

/****** end header contents ******/

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

  assert((result[0] > -1) && (result[0] < 3));
  assert((result[1] > -1) && (result[1] < 3));
  assert((result[2] > -1) && (result[2] < 3));

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
      assert(permutation[i] > -1);
      assert(permutation[i] < 3);
      array[i] = buffer[permutation[i]];
    }
}

/*
 * Construct a list of one member.
 */
static GList*
g_list_singleton(gpointer data)
{
  GList* result = NULL;
 
  result = g_list_append(result, data);

  return result;
}


/*
 * Constructor for the NIH spectrum type.
 */
HosSpectrum*
spectrum_nih_from_file(gchar* fname)
{
  float hdr[NIH_HDR_SIZE];
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM, NULL);
  HosDimensionBlock *dimen_block;
  HosDimension *dimen;
  HosBackingFile *backing_file;
  HosBacking *backing;
  gulong cumulative_stride;

  /* read header data -- FIXME error checking */
  {
    FILE *channel = fopen(fname, "r");

    if (channel == NULL)
      return NULL;
    fread(hdr, sizeof(float), NIH_HDR_SIZE, channel);

    fclose(channel);
  }

  /* create a backing object for the spectrum */
  backing_file = g_object_new(HOS_TYPE_BACKING_FILE, NULL);
  backing_file->fname = g_strdup(fname);
  backing_file->hdr_size = NIH_HDR_SIZE * sizeof(float);
  backing = HOS_BACKING(backing_file);

  /* checks for endian-ness based on sanity of header values */
  if ((hdr[AWOL_SF_X] > 1000) || (hdr[AWOL_SF_X] < 1))
    {
      endian_swap4(hdr, NIH_HDR_SIZE);
      backing_file->needs_swap = 1;
    }

  /* sanity checks after byte swapping */
  assert(!((hdr[AWOL_SF_X] > 1000) || (hdr[AWOL_SF_X] < 1)));
  assert(!((hdr[AWOL_NP_X] > 1e6) || (hdr[AWOL_NP_X] < 1)));

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

  permute(sf, permutation);
  permute(sw, permutation);
  permute(orig, permutation);

  /* set up dimensions */

  /* X */
  dimen_block = g_object_new(HOS_TYPE_DIMENSION_BLOCK, NULL);
  dimen = HOS_DIMENSION(dimen_block);

  dimen->backing = backing;
  dimen_block->schedule = NULL;
  dimen_block->np_physical = hdr[AWOL_NP_X];
  dimen_block->sw_physical = sw[0];
  dimen->sw = dimen_block->sw_physical;
  dimen->sf = sf[0];
  dimen->orig = orig[0] + dimen->sw;
  dimen->np = dimen_block->np_physical;
  /* FIXME-- make sure orig corresponds to point 0 */
  dimen->orig -= dimen->sw / dimen->np;
  dimen_block->initial_offset = 0;
  dimen_block->stride = 1;
  dimen_block->fold_allowed = TRUE;
  cumulative_stride = dimen_block->np_physical;
  if (IS_COMPLEX_X(hdr))
    cumulative_stride *= 2;
  dimen_block->negated_initially = FALSE;
  dimen_block->negate_on_fold = FALSE;

  result->dimensions = g_list_append(result->dimensions, g_list_singleton(dimen));

  /* Y */
  dimen_block = g_object_new(HOS_TYPE_DIMENSION_BLOCK, NULL);
  dimen = HOS_DIMENSION(dimen_block);

  dimen->backing = backing;
  dimen_block->schedule = NULL;
  dimen_block->np_physical =
    (IS_COMPLEX_X(hdr) && IS_COMPLEX_Y(hdr)) ? hdr[AWOL_NP_Y] / 2 : hdr[AWOL_NP_Y];
  dimen_block->sw_physical = sw[1];
  dimen->sw = dimen_block->sw_physical;
  dimen->sf = sf[1];
  dimen->orig = orig[1] + dimen->sw;
  dimen->np = dimen_block->np_physical;
  /* FIXME-- make sure orig corresponds to point 0 */
  dimen->orig -= dimen->sw / dimen->np;
  dimen_block->initial_offset = 0;
  if (IS_COMPLEX_Y(hdr))
    cumulative_stride *= 2;
  dimen_block->stride = cumulative_stride;
  dimen_block->fold_allowed = TRUE;
  cumulative_stride *= dimen_block->np_physical;
  dimen_block->negated_initially = FALSE;
  dimen_block->negate_on_fold = FALSE;

  result->dimensions = g_list_append(result->dimensions, g_list_singleton(dimen));

  /* Z */
  dimen_block = g_object_new(HOS_TYPE_DIMENSION_BLOCK, NULL);
  dimen = HOS_DIMENSION(dimen_block);

  dimen->backing = backing;
  dimen_block->schedule = NULL;
  dimen_block->np_physical =
    IS_COMPLEX_Z(hdr) ? hdr[AWOL_NP_Z] / 2 : hdr[AWOL_NP_Z];

  dimen_block->sw_physical = sw[2];
  dimen->sw = dimen_block->sw_physical;
  dimen->sf = sf[2];
  dimen->orig = orig[2] + dimen->sw;
  dimen->np = dimen_block->np_physical;
  /* FIXME-- make sure orig corresponds to point 0 */
  dimen->orig -= dimen->sw / dimen->np;
  dimen_block->initial_offset = 0;
  if (IS_COMPLEX_Z(hdr))
    cumulative_stride *= 2;
  dimen_block->stride = cumulative_stride;
  dimen_block->fold_allowed = TRUE;
  cumulative_stride *= dimen_block->np_physical;
  dimen_block->negated_initially = FALSE;
  dimen_block->negate_on_fold = FALSE;

  result->dimensions = g_list_append(result->dimensions, g_list_singleton(dimen));

  return result;
}

/*
 * Convenience function to load an NIH spectrum
 * which lacks a Z dimension.
 */
HosSpectrum*
spectrum_nih_2d_from_file(gchar* fname)
{
  HosSpectrum* result;

  result = spectrum_nih_from_file(fname);
  result = spectrum_transpose(result, 2);
  result = spectrum_project(result);

  return result;
}

void
spectrum_nih_unfold(HosSpectrum *self,
		    guint dim,
		    guint downfield,
		    guint upfield,
		    gboolean negate_on_fold)
{
  HosDimensionBlock *dimen =
    (HosDimensionBlock*)g_list_nth_data((GList*)g_list_nth_data(self->dimensions, dim), 0);
    dimension_block_unfold(dimen, downfield, upfield, negate_on_fold);
}


