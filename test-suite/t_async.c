
#include <burrow.h>
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"


static const guint n_spectra = 20;

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *spectra[n_spectra];
  gint i;
  for (i = 0; i < n_spectra; ++i)
      spectra[i] =
	spectrum_integrate(spectrum_integrate(spectrum_flakify(HOS_SPECTRUM(spectrum_test_cube_new()), 1.0 - 1e-4)));

  g_print("Asynchronously traversing %d spectra...", n_spectra);
  for (i = 0; i < n_spectra; ++i)
    spectrum_traverse(spectra[i]);

  /* Wait for completion */
  while (1)
    {
      gint n_finished = 0;
      for (i = 0; i < n_spectra; ++i)
	if (spectra[i]->buf != NULL)
	  {
	    ++n_finished;
	    g_assert(spectrum_peek(spectra[i], 0) == test_cube_II_predict(0));
	    g_assert(spectrum_peek(spectra[i], 1) == test_cube_II_predict(1));
	  }

      g_print("%d...", n_finished, n_spectra);
      if (n_finished == n_spectra) break;
      g_usleep(200000);
    }

  g_print("done!\n\n");

  return 0;
}
