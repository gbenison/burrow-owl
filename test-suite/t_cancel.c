/*
 * Test traversal cancellation.
 * Spawn a bunch of asynchronous traversals, cancel them right away,
 * wait for the last-requested traversal
 */

#include "burrow/spectrum.h"
#include "segment-sim.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  gint i;
  for (i = 0; i < 1000; ++i)
    {
      HosSpectrum *spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
      traversal_token_t *token = spectrum_traverse(spec);
    }

  /* start final traversal and wait */
  HosSpectrum *spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
  spectrum_traverse (spec);
  while (1)
    {
      if (spectrum_is_ready(spec))
	break;
      g_usleep(100000);
      g_print(".");
    }

  /* validate */
  for (i = 0; i < spectrum_np(spec, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec), i)
	       == spectrum_peek(spec, i));
    }

  g_print("OK\n");

  /* FIXME perhaps add timer? */
  
  return 0;
}

