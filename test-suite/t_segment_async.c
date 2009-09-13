
#include "burrow/spectrum.h"
#include "segment-sim.h"
#include "test-utils.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing asynchronous traversal of segmented spectra");

  gint i;
  HosSpectrum *spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
  static const guint n_readers = 20;
  HosSpectrum* readers[n_readers];
  for (i = 0; i < n_readers; ++i)
    {
      readers[i] = spectrum_integrate(spec_sim);
      spectrum_traverse(readers[i]);
    }

  gint segment_sim_np = spectrum_np(spec_sim, 0);
  gdouble predicted = 
    ((gdouble)segment_sim_np * (gdouble)(segment_sim_np - 1)) / 2;

  for (i = 0; i < n_readers; ++i)
    {
      g_thread_join(spectrum_monitor(readers[i]));
      gdouble actual = spectrum_peek(readers[i], 0);
      if ((actual > 0) && (predicted > 0))
	g_assert(IS_ABOUT_EQUAL(actual, predicted));
    }

  g_print("OK\n");

  return 0;
}
