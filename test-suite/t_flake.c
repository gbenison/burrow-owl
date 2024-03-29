
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"
#include "test-utils.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing spectrum_flakify()");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 0.999);
  HosSpectrum *S3 = spectrum_integrate(S2);
  HosSpectrum *S4 = spectrum_integrate(S3);

  spectrum_monitor(S4);
  spectrum_traverse_blocking(S4);

  guint i;
  for (i = 1; i < 100; ++i)
    g_assert(spectrum_peek(S2, i) == test_cube_predict(i));

  for (i = 0; i < 10; ++i)
    g_assert(spectrum_peek(S4, i) == test_cube_II_predict(i));

  g_print("OK\n");

  return 0;

}


