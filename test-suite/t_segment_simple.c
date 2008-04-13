
#include <burrow.h>
#include "segment-sim.h"

static gint i;

int
main()
{
  g_print("===== t_segment_simple =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Making simulated segmented spectrum...\n");
  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  g_print("Traversing...\n");
  spectrum_traverse_blocking(HOS_SPECTRUM(spec_sim));

  g_print("Validating...\n");

  for (i = 1; i < spectrum_np(spec_sim, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), i) == spectrum_peek(spec_sim, i));
    }

  return 0; /* implementation incomplete */
}
