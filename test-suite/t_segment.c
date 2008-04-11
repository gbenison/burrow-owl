
#include <burrow.h>
#include "segment-sim.h"

static gint i;

int
main()
{
  g_print("===== t_segment =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Making simulated segmented spectrum...\n");
  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  g_print("Forced segment load test...\n");

  for (i = 0; i < 100; ++i)
    {
      gint segid = g_random_int_range(0, (segment_sim_np / segment_sim_segment_size) - 1);
      g_printf("Loading segment %d...\n  ", segid);
      spectrum_segmented_test_load_segment(HOS_SPECTRUM_SEGMENTED(spec_sim), segid);
      gint j;
      for (j = 0; j < 10; ++j)
	{
	  gdouble value = 0;
	  guint idx = g_random_int_range(0, segment_sim_np);
	  gboolean success = spectrum_segmented_test_peek(HOS_SPECTRUM_SEGMENTED(spec_sim), &idx, &value);
	  if (success)
	    g_assert(value == segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), idx));
	  g_printf("%6d %s| ", idx, success ? "*" : " ");
	}
      g_printf("\n");
    }

  g_printf("Segment cache contents:\n");
  spectrum_segmented_test_print_cache(HOS_SPECTRUM_SEGMENTED(spec_sim));

  g_print("Traversing...\n");
  spectrum_traverse(HOS_SPECTRUM(spec_sim));
  while (1)
    {
      g_usleep(1000000);
      spectrum_segmented_test_print_cache(HOS_SPECTRUM_SEGMENTED(spec_sim));
      spectrum_segmented_report_request_status(HOS_SPECTRUM_SEGMENTED(spec_sim));
      if (spec_sim->buf != NULL)
	break;
    }
  g_print("\nDone.\n");
  g_print("Validating...\n");

  for (i = 1; i < spectrum_np(spec_sim, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), i) == spectrum_peek(spec_sim, i));
    }

  g_print("Testing simultaneous asynchronous traversals...\n");

  spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
  static const guint n_readers = 20;
  HosSpectrum* readers[n_readers];
  for (i = 0; i < n_readers; ++i)
    {
      readers[i] = spectrum_integrate(spec_sim);
      spectrum_traverse(readers[i]);
    }

  while (1)
    {
      g_usleep(500000);
      guint n_remaining = n_readers;
      for (i = 0; i < n_readers; ++i)
	if (readers[i]->buf != NULL) --n_remaining;
      g_printf("(%d) ", n_remaining);
      spectrum_segmented_test_print_cache(HOS_SPECTRUM_SEGMENTED(spec_sim));
      spectrum_segmented_report_request_status(HOS_SPECTRUM_SEGMENTED(spec_sim));
      if (n_remaining == 0)
	break;
    }

  /* validate readers */
  gdouble predicted = 
    ((gdouble)segment_sim_np * (gdouble)(segment_sim_np - 1)) / 2;
  for (i = 0; i < n_readers; ++i)
    {
      gdouble actual = spectrum_peek(readers[i], 0);
      g_assert((actual / predicted) > 0.999999);
      g_assert((actual / predicted) < 1.000001);
    }

  return 0;
}
