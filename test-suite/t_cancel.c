/*
 * Test traversal cancellation.
 * Spawn a bunch of asynchronous traversals, cancel them right away,
 * wait for the last-requested traversal
 */

#include "burrow/spectrum.h"
#include "segment-sim.h"

static void
report_thread_func(HosSpectrum *self)
{
  /* FIXME perhaps add timer? */
  while (1)
    {
      if (spectrum_is_ready(self))
	break;
      g_usleep(300000);
      g_print(".");
    }
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *main_spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  /* start report thread */
  GError *error = NULL;
  GThread *report_thread =
    g_thread_create((GThreadFunc)report_thread_func,
		    main_spec,
		    TRUE,
		    &error);
  g_assert(error == NULL);

  gint i;
  for (i = 0; i < 1000; ++i)
    {
      HosSpectrum *spec = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
      traversal_token_t *token = spectrum_traverse(spec);
      spectrum_traverse_cancel(token);
      g_object_unref(spec);
    }

  /* start final traversal and wait */
  spectrum_traverse (main_spec);
  g_thread_join(report_thread);

  /* validate */
  for (i = 0; i < spectrum_np(main_spec, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(main_spec), i)
	       == spectrum_peek(main_spec, i));
    }

  g_print("OK\n");

  return 0;
}

