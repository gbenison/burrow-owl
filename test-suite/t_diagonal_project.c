
#include <math.h>
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"


int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_printf("==== diagonal projection test ======\n");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);
  HosSpectrum *S3 = spectrum_diagonal_project(S2);

  gint s2_idx = 0;
  gint s2_np = spectrum_np_total(S2);

  gint i;
  for (i = 0; i < spectrum_np_total(S3); ++i)
    {
      gdouble actual = spectrum_peek(S3, i);
      
      while (1)
	{
	  if (s2_idx >= s2_np)
	    g_error("idx %d: cannot find matching point in source spec", i);
	  gdouble s2_actual = spectrum_peek(S2, s2_idx);
	  if (fabs(actual - s2_actual) < 0.01)
	    break;
	  ++s2_idx;
	}

      if ((i % 100) == 0) g_print(".");
    }
  
  g_print("OK\n\n");

  return 0;
}


