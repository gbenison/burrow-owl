
#include <math.h>
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"
#include "test-utils.h"

static gdouble
grok_point(HosSpectrum* S, gint x, gint y, gint z)
{
  gint idx =
    x +
    y * spectrum_np(S, 0) +
    z * spectrum_np(S, 0) * spectrum_np(S, 1);
    
  return spectrum_peek(S, idx);
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_printf("Testing spectrum_unfold()");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);
  HosSpectrum *S3 = spectrum_unfold(S2, 1, 2, 1, FALSE);

  HosSpectrum *S4 = spectrum_transpose(S2, 1);
  HosSpectrum *S5 = spectrum_transpose(S3, 1);

  gdouble y1 = g_random_double_range(spectrum_giro_ppm(S4, 0), spectrum_orig_ppm(S4, 0));
  gdouble yn = g_random_double_range(spectrum_giro_ppm(S4, 0), spectrum_orig_ppm(S4, 0));

  HosSpectrum *S6 = spectrum_extract_ppm(S4, y1, yn);
  HosSpectrum *S7 = spectrum_extract_ppm(S5, y1, yn);

  monitor_interval /= 10;
  spectrum_monitor(S7);

  gint i;
  for (i = 0; i < 50; ++i)
    {
      gint x = g_random_int_range(0, spectrum_np(S6, 0));
      gint y = g_random_int_range(0, spectrum_np(S6, 1));
      gint z = g_random_int_range(0, spectrum_np(S6, 2));
      gdouble s6 = grok_point(S6, x, y, z);
      gdouble s7 = grok_point(S7, x, y, z);
      g_assert(fabs(s6 - s7) < 0.01);
    }

  g_print("OK\n");

  return 0;
}
