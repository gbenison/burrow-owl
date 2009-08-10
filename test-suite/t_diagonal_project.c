
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

#define DX_TO_IDX(dx0, dx1)  (source_x0 + dx0 + (source_x1 - dx1) * stride_x1 + y * stride_y)
#define DX_TO_PEEK(dx0, dx1) (spectrum_peek(S2, DX_TO_IDX(dx0, dx1)))
#define DX_IS_BAD(dx0, dx1)  (fabs(actual - DX_TO_PEEK(dx0, dx1)) > 0.00001)

      if (DX_IS_BAD(-1, -1) &&
	  DX_IS_BAD(-1, 0) &&
	  DX_IS_BAD(-1, 1) &&

	  DX_IS_BAD(0, -1) &&
	  DX_IS_BAD(0, 0) &&
	  DX_IS_BAD(0, 1) &&

	  DX_IS_BAD(1, -1) &&
	  DX_IS_BAD(1, 0) &&
	  DX_IS_BAD(1, 1))
	g_error("Error: idx %d actual %f, no matching point found\n", i, actual);
      if ((i % 100) == 0) g_print(".");
    }
  
  g_print("OK\n\n");

  return 0;
}


