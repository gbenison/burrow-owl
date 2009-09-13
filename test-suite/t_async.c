
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"
#include "test-utils.h"


static const guint n_spectra = 20;

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing asynchronous traversal");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0-1e-5);

  HosSpectrum *spectra[n_spectra];
  gint i;
  for (i = 0; i < n_spectra; ++i)
    {
      spectra[i] = spectrum_integrate(spectrum_integrate(S2));
      spectrum_traverse(spectra[i]);
    }

  for (i = 0; i < n_spectra; ++i)
    {
      g_thread_join(spectrum_monitor(spectra[i]));
      g_assert(spectrum_peek(spectra[i], 0) == test_cube_II_predict(0));
      g_assert(spectrum_peek(spectra[i], 1) == test_cube_II_predict(1));
    }

  g_print("OK\n");

  return 0;
}
