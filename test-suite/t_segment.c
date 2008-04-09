
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

  for (i = 0; i < 50; ++i)
    {
      gint segid = g_random_int_range(0, (100000 / 1024) - 1);
      g_printf("Loading segment %d...\n", segid);
      spectrum_segmented_test_load_segment(HOS_SPECTRUM_SEGMENTED(spec_sim), segid);
    }

  g_print("Traversing...\n");
  spectrum_traverse_blocking(HOS_SPECTRUM(spec_sim));
  g_print("Done.\n");
  g_print("Validating...\n");

  for (i = 0; i < spectrum_np(spec_sim, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), i) == spectrum_peek(spec_sim, i));
    }

  return 1; /* implementation not complete */
}
