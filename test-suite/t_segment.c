
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
  g_print("Disabled.\n");

#ifdef UNDEF
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
	  if (success && (value > 0))
	    g_assert(value == segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), idx));
	  g_printf("%6d %s| ", idx, success ? "*" : " ");
	}
      g_printf("\n");
    }
#endif

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

  return 0;
}
