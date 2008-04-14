
#include <burrow.h>
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

  gint nx = spectrum_np(S3, 0);

  gint i;
  for (i = 0; i < spectrum_np_total(S3); ++i)
    {
      gdouble actual = spectrum_peek(S3, i);

      gint x = i % nx;
      gint y = i / nx;

      gdouble x_ppm = spectrum_pt2ppm(S3, 0, x);
      gdouble y_ppm = spectrum_pt2ppm(S3, 1, y);

      gint source_x0 = spectrum_ppm2pt(S2, 0, x_ppm);
      gint source_x1 = spectrum_ppm2pt(S2, 1, x_ppm);
      gint source_y  = spectrum_ppm2pt(S2, 2, y_ppm);

      gint source_idx =
	source_x0
	+ source_x1 * spectrum_np(S2, 0)
	+ source_y * spectrum_np(S2, 0) * spectrum_np(S2, 1);

      gdouble predicted = spectrum_peek(S2, source_idx);

      if (predicted > 0)
	{
	  g_assert(actual > (predicted * 0.9999999));
	  g_assert(actual < (predicted * 1.0000001));
	}
      if ((i % 100) == 0) g_print(".");
    }
  
  g_print("OK\n\n");

  return 0;
}


