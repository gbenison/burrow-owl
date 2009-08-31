
#include "burrow/spectrum.h"
#include "segment-sim.h"

int
main()
{
  g_print("===== t_segment_async =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing simultaneous asynchronous traversals...\n");

  gint i;
  HosSpectrum *spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));
  static const guint n_readers = 20;
  HosSpectrum* readers[n_readers];
  for (i = 0; i < n_readers; ++i)
    {
      readers[i] = spectrum_integrate(spec_sim);
      spectrum_traverse(readers[i]);
    }

  gint tick = 0;
  while (1)
    {
      g_usleep(500000);
      ++tick;
      guint n_remaining = n_readers;
      for (i = 0; i < n_readers; ++i)
	if (spectrum_is_ready(readers[i]))
	  --n_remaining;
      g_message("(%3d) %3d remaining", tick, n_remaining);
      if (n_remaining == 0)
	break;
    }

  /* validate readers */
  gint segment_sim_np = spectrum_np(spec_sim, 0);
  gdouble predicted = 
    ((gdouble)segment_sim_np * (gdouble)(segment_sim_np - 1)) / 2;
  for (i = 0; i < n_readers; ++i)
    {
      gdouble actual = spectrum_peek(readers[i], 0);
      if ((actual > 0) && (predicted > 0))
	{
	  g_assert((actual / predicted) > 0.999999);
	  g_assert((actual / predicted) < 1.000001);
	}
    }

  return 0;
}
