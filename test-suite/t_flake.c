
#include <burrow.h>
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);
  HosSpectrum *S3 = spectrum_integrate(S2);
  HosSpectrum *S4 = spectrum_integrate(S3);

  spectrum_traverse_blocking(S4);
  guint i;
  for (i = 0; i < 10; ++i)
    {
      g_print("%.2f ", spectrum_peek(S4, i));
      g_assert(spectrum_peek(S4, i) == test_cube_II_predict(i));
    }
  g_print("\n\n");

  return 0;

}


