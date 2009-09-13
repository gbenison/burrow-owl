/*
 * Start an asynchronous traversal, then override with a blocking
 * traversal.
 */

#include "burrow/spectrum.h"
#include "segment-sim.h"
#include "test-utils.h"

#define N_SPECTRA 100

int
main()
{
  g_print("Testing blocking override of asynchronous traversal");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum* spectra[N_SPECTRA];
  gint cancel_id[N_SPECTRA];

  monitor_interval *= 4;

  gint i;
  for (i = 0; i < N_SPECTRA; ++i)
    {
      spectra[i] = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
      cancel_id[i] = spectrum_traverse(spectra[i]);
    }

  /* let some traversals proceed. */
  g_usleep(5000);

  /*
   * At random, override some traversals,
   * cancel then override others
   */
  for (i = 0; i < 5; ++i)
    {
      guint idx1 = g_random_int_range(0, N_SPECTRA);
      spectrum_traverse_cancel(spectra[idx1], cancel_id[idx1]);
      cancel_id[idx1] = -1;
      GThread *monitor1 = spectrum_monitor(spectra[idx1]);
      spectrum_traverse_blocking(spectra[idx1]);
      g_assert(spectrum_is_ready(spectra[idx1]));

      guint idx2 = g_random_int_range(0, N_SPECTRA);
      GThread *monitor2 = spectrum_monitor(spectra[idx2]);
      spectrum_traverse_blocking(spectra[idx2]);
      g_assert(spectrum_is_ready(spectra[idx2]));

      segment_sim_validate(HOS_SPECTRUM_SEGMENT_SIM(spectra[idx1]));
      segment_sim_validate(HOS_SPECTRUM_SEGMENT_SIM(spectra[idx2]));
    }

  g_print("OK\n");
  return 0;
}
