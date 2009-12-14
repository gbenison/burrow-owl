
#include "burrow/spectrum.h"
#include "segment-sim.h"

static gint i;

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing segmented spectra");

  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  spectrum_monitor(HOS_SPECTRUM(spec_sim));
  spectrum_traverse_blocking(HOS_SPECTRUM(spec_sim));

  for (i = 1; i < spectrum_np(spec_sim, 0); ++i)
    {
      g_assert(segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(spec_sim), i) == spectrum_peek(spec_sim, i));
    }

  g_print("OK\n");

  return 0;
}
