
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"


/*
 * If a spectrum is already instantiated, traversal of derived spectra
 * should just fetch the instantiated values, rather than triggering
 * the retrieval mechanism of the entire tree below.
 *
 * i.e., traversing a spectrum is a way of caching its contents, which
 * can make subsequent traversal of derived spectra faster.
 */

static const guint n_spectra = 20;

static void
test_traversal(HosSpectrum* S)
{
  HosSpectrum *spectra[n_spectra];
  gint i;
  for (i = 0; i < n_spectra; ++i)
      spectra[i] =
	spectrum_integrate(spectrum_integrate(S));
  for (i = 0; i < n_spectra; ++i)
    spectrum_traverse(spectra[i]);

  /* Wait for completion */
  while (1)
    {
      gint n_finished = 0;
      for (i = 0; i < n_spectra; ++i)
	if (spectrum_is_ready(spectra[i]) == TRUE)
	  {
	    ++n_finished;
	    g_assert(spectrum_peek(spectra[i], 0) == test_cube_II_predict(0));
	    g_assert(spectrum_peek(spectra[i], 1) == test_cube_II_predict(1));
	  }

      g_print(".", n_finished, n_spectra);
      if (n_finished == n_spectra) break;
      g_usleep(300000);
    }
  g_print("OK\n");
}

static gdouble flake_factor = 1.0-(1e-4);

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  GTimer* timer = g_timer_new();
  g_timer_start(timer);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, flake_factor);

  g_print("Testing asynchronous traversal (without caching)");
  gdouble start_time = g_timer_elapsed(timer, NULL);
  test_traversal(S2);
  gdouble end_time = g_timer_elapsed(timer, NULL);
  g_printf("Time elapsed (sec): %.2f\n", end_time - start_time);

  HosSpectrum *S3 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S4 = spectrum_flakify(S3, flake_factor);
  
  g_print("Testing asynchronous traversal (with caching)");
  start_time = g_timer_elapsed(timer, NULL);
  spectrum_traverse_blocking(S4);
  test_traversal(S4);
  end_time = g_timer_elapsed(timer, NULL);
  g_printf("Time elapsed (sec): %.2f\n", end_time - start_time);

  return 0;
}

