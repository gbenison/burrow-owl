/*
 * Test traversal cancellation.
 * Spawn a bunch of asynchronous traversals, cancel them right away,
 * wait for the last-requested traversal
 */

#include "burrow/spectrum.h"
#include "segment-sim.h"
#include "test-utils.h"

int
main()
{
  g_print("Testing cancellation of asynchronous traversal");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *main_spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  GThread *monitor = spectrum_monitor(main_spec);

  gint i;
  for (i = 0; i < 1000; ++i)
    {
      HosSpectrum *spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
      gint cancel_id = spectrum_traverse(spec);
      spectrum_traverse_cancel(spec, cancel_id);
      g_object_unref(spec);
    }

  /* start final traversal and wait */
  spectrum_traverse (main_spec);
  g_thread_join (monitor);

  /* validate */
  segment_sim_validate(HOS_SPECTRUM_SEGMENT_SIM(main_spec));
  g_print("OK\n");

  return 0;
}

