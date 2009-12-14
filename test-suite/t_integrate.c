
#include "burrow/spectrum.h"
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"
#include "test-utils.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing spectrum integration");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_ramp_new());
  HosSpectrum *S2 = spectrum_integrate(S1);

  monitor_interval /= 4;
  spectrum_monitor(S2);

  guint np = spectrum_np(S1, 0);
  gdouble theoretical = (np * (np - 1) / 2);
  gdouble actual = spectrum_peek(S2, 0);

#define ABOUT_EQUAL(a, b) (fabs((a) - (b)) < 0.001)

  g_assert(ABOUT_EQUAL(theoretical, actual));

  HosSpectrum *cube = HOS_SPECTRUM(spectrum_test_cube_new());

  gint start;
  static const gint n = 10;
  for (start = 1; start < 100000; start *= 4)
    {
      if (start + n >= spectrum_np_total(cube))
	break;
      gint i;
      for (i = 0; i < n; ++i)
	g_assert(spectrum_peek(cube, start + i)
		 == test_cube_predict(start + i));
    }

  HosSpectrum *S3 = spectrum_integrate(cube);
  spectrum_monitor(S3);
  gint i;
  for (i = 0; i < 10; ++i)
    g_assert(spectrum_peek(S3, i) == test_cube_I_predict(i));

  HosSpectrum *S4 = spectrum_integrate(S3);
  spectrum_monitor(S4);
  for (i = 0; i < 10; ++i)
    g_assert(spectrum_peek(S4, i) == test_cube_II_predict(i));

  g_print("OK\n");

  return 0;
}

