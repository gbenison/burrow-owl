
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

  gint stride_x1 = spectrum_np(S2, 0);
  gint stride_y  = spectrum_np(S2, 0) * spectrum_np(S2, 1);

  gint i;
  for (i = 0; i < spectrum_np_total(S3); ++i)
    {
      gdouble actual = spectrum_peek(S3, i);

      gint x = i % nx;
      gint y = i / nx;

      gdouble x_ppm = spectrum_pt2ppm(S3, 0, x);

      gint source_x0 = spectrum_ppm2pt(S2, 0, x_ppm);
      gint source_x1 = spectrum_ppm2pt(S2, 1, x_ppm);

      gint source_idx_1 = source_x0 + (source_x1 - 1) * stride_x1 + y * stride_y;
      gint source_idx_2 = source_x0 + source_x1 * stride_x1 + y * stride_y;
      gint source_idx_3 = source_x0 + (source_x1 + 1) * stride_x1 + y * stride_y;

      gdouble error_1 = fabs(actual - spectrum_peek(S2, source_idx_1));
      gdouble error_2 = fabs(actual - spectrum_peek(S2, source_idx_2));
      gdouble error_3 = fabs(actual - spectrum_peek(S2, source_idx_3));

      if ((error_1 > 0.00001) &&
	  (error_2 > 0.00001) &&
	  (error_3 > 0.00001))
	g_error("Error: idx %d actual %f, errors %f, %f, %f", i, actual, error_1, error_2, error_3);
      if ((i % 100) == 0) g_print(".");
    }
  
  g_print("OK\n\n");

  return 0;
}


