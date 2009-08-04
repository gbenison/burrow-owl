/*
 * Motivation:
 * bug found 1aug09 in which spectrum_traverse_blocking()
 * would deadlock due to spectrum_segmented iterators not being
 * free'd properly.
 * Triggered by: many blocking traversals of the same segmented
 * spectrum.
 */

#include "burrow/spectrum.h"
#include <math.h>
#include <glib.h>
#include "segment-sim.h"
#include "spectrum-ramp.h"

static const gint dx = 8;
static const gint dy = 8;

/*
 * Return the integrated intensity of
 * a 2D box centered at (x, y)
 * of plane 'z' of 'src'
 */
static gdouble
spectrum_integrate_around(HosSpectrum *src,
			  gint x,
			  gint y,
			  gint z)
{
  return
    (spectrum_peek
     (spectrum_integrate
      (spectrum_integrate
       (spectrum_extract
	(spectrum_transpose
	 (spectrum_extract
	  (spectrum_project(spectrum_transpose(src, 2), z),
	   x - dx,
	   x + dx),
	  1),
	 y - dy,
	 y + dy))),
      0));
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
  HosSpectrum *S2 = spectrum_convolute(S1, S1);
  HosSpectrum *S3 = spectrum_convolute(HOS_SPECTRUM(spectrum_ramp_new()),
				       S2); 

  g_print("Testing blocking traversal for deadlocks");

  gint i;
  for (i = 0; i < 256; ++i)
    {
      gdouble result =
	spectrum_integrate_around(S3,
				  g_random_int_range(0, spectrum_np(S3, 0) - 1),
				  g_random_int_range(0, spectrum_np(S3, 1) - 1),
				  g_random_int_range(0, spectrum_np(S3, 2) - 1));
      if ((i % 8) == 0)
	g_print(".");
    }
  g_printf("OK\n");

  return 0;
}

