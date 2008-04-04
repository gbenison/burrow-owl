
#include <burrow.h>
#include "segment-sim.h"

int
main()
{
  g_print("===== t_segment =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Making simulated segmented spectrum...\n");
  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  g_print("Traversing...\n");
  spectrum_traverse_blocking(HOS_SPECTRUM(spec_sim));
  g_print("Done.\n");
  g_print("Validating...\n");

  gint i;
  for (i = 0; i < spectrum_np(spec_sim, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), i) == spectrum_peek(spec_sim, i));
    }

  return 1; /* implementation not complete */
}
