/*
 *  Copyright (C) 2008 Greg Benison
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

#include "spectrum_segmented.h"

static gboolean spectrum_segmented_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);
static gboolean spectrum_segmented_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

G_DEFINE_ABSTRACT_TYPE (HosSpectrumSegmented, hos_spectrum_segmented, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_segmented_class_init(HosSpectrumSegmentedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->accumulate = spectrum_segmented_accumulate;
  spectrum_class->tickle     = spectrum_segmented_tickle;
}

static void
hos_spectrum_segmented_init(HosSpectrumSegmented *self)
{
  /* FIXME anything? */
}

static gboolean
spectrum_segmented_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  /*
   * FIXME
   *
   * maintain per-'root' record of 'accumulation segment request' which the IO thread will consult
   * to decide on the next segment to obtain.
   *
   * set the accumulation request of 'root', then block waiting for it to be instantiated by IO thread
   * block by waiting on segment_ready_cond
   */
}

static gboolean
spectrum_segmented_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  /*
   * FIXME
   *
   * maintain per-'root' tickle-request-queue, sorted list of requested segments.
   * IO thread consults tickle-request-queue (together with accumulation-request-record) to
   * determine next segment to read.
   *
   * Grab point if the segment is instantiated; otherwise add segment to the tickle-request-queue.
   */
}

/* IO thread */
/* FIXME */
static void
spectrum_segmented_io_thread(HosSpectrumSegmented *self)
{
  while (1)
    {
      /* 
       * decide next segment -- 
       * consult all per-'root' accumulation-records and tickle-request-queues.
       */

      /* nothing new?  block waiting for cond_new_request from traversal threads. */

      /* read next segment */
      /* put next segment in cache */
      /* signal segment_ready_cond */
     
      /*
       * root list cleanup --
       * if any of the traversal 'roots' are in state 'COMPLETE' or 'LATENT', remove
       * their per-root records
       * FIXME: coordinate with traversal threads that add to the per-root table
       */
    }
}
