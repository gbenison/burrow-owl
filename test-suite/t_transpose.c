
#include <math.h>
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"

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

  g_printf("==== transpose test ======\n");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);
  HosSpectrum *S3 = spectrum_transpose(S2, 2);   /* 2 0 1 */
  HosSpectrum *S4 = spectrum_transpose(S3, 2);   /* 1 2 0 */

  g_assert(spectrum_np(S4, 0) == spectrum_np(S2, 1));
  g_assert(spectrum_np(S4, 1) == spectrum_np(S2, 2));
  g_assert(spectrum_np(S4, 2) == spectrum_np(S2, 0));

  g_assert(spectrum_sw(S4, 0) == spectrum_sw(S2, 1));
  g_assert(spectrum_sw(S4, 1) == spectrum_sw(S2, 2));
  g_assert(spectrum_sw(S4, 2) == spectrum_sw(S2, 0));

  gint i;
  for (i = 0; i < 50; ++i)
    {
      gint x = g_random_int_range(0, 99);
      gint y = g_random_int_range(0, 99);
      gint z = g_random_int_range(0, 99);
      gdouble s4 = grok_point(S4, y, z, x);
      gdouble s2 = grok_point(S2, x, y, z);
      g_assert(fabs(s2 - s4) < 0.01);
      g_print(".");
    }

  g_print("OK\n\n");

  return 0;
}
