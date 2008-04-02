
#include <burrow.h>
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_ramp_new());
  g_message("Made ramped spectrum S1 with %d dimensions\n", spectrum_ndim(S1));
  HosSpectrum *S2 = spectrum_integrate(S1);
  g_message("Integrated S1 to obtain S2 with %d dimensions\n", spectrum_ndim(S2));

  guint np = spectrum_np(S1, 0);
  gdouble theoretical = (np * (np - 1) / 2);
  gdouble *data = spectrum_traverse_blocking(S2);
  gdouble actual = *data;
  g_print("S2 value: %f (%f)\n ", actual, theoretical);

#define ABOUT_EQUAL(a, b) (fabs((a) - (b)) < 0.001)

  g_assert(ABOUT_EQUAL(theoretical, actual));

  HosSpectrum *cube = HOS_SPECTRUM(spectrum_test_cube_new());
  g_message("Made new test cube spectrum with %d dimensions\n", spectrum_ndim(cube));

  gint start;
  static const gint n = 10;
  for (start = 1; start < 100000; start *= 4)
    {
      gint i;
      g_print("Cube (%6d, ...) = ", start);
      for (i = 0; i < n; ++i)
	{
	  g_print("%.2f ", spectrum_peek(cube, start + i));
	  g_assert(spectrum_peek(cube, start + i) == test_cube_predict(start + i));
	}
      g_print("\n");
    }

  HosSpectrum *S3 = spectrum_integrate(cube);
  g_print("S3 = integrate(cube)\n");
  g_print("S3 = ");
  gint i;
  for (i = 0; i < 10; ++i)
    {
      g_print("%.2f ", spectrum_peek(S3, i));
      g_assert(spectrum_peek(S3, i) == test_cube_I_predict(i));
    }
  g_print("\n\n");

  g_assert(spectrum_peek(S3, 1) == 104950);

  HosSpectrum *S4 = spectrum_integrate(S3);
  g_print("S4 = integrate(S3)\n");
  g_print("S4 = ");
  for (i = 0; i < 10; ++i)
    {
      g_print("%.2f ", spectrum_peek(S4, i));
      g_assert(spectrum_peek(S4, i) == test_cube_II_predict(i));
    }
  g_print("\n\n");

  return 0;
}

