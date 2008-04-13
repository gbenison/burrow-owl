
#include <burrow.h>
#include "segment-sim.h"
#include "spectrum_priv.h"

gint n_read = 0;

/*
 * pick an index at random from 'self' and ensure that it matches
 * the predicted value, in an infinite loop.
 */
static void
reader(HosSpectrum *self)
{
  gint np = spectrum_np(self, 0);
  while (1)
    {
      gint     idx       = g_random_int_range(0, np);
      gdouble  predicted = segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(self), idx);
      gdouble  actual    = spectrum_accumulate(self, self, &idx);

      if ((actual > 0) && (predicted > 0))
	{
	  g_assert((actual / predicted) > 0.999999);
	  g_assert((actual / predicted) < 1.000001);
	}
      ++n_read;
    }
}

int
main()
{
  g_print("===== t_segment_accumulate =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  GError *thread_error = NULL;

  static gint n_readers = 5;
  gint i;
  for (i = 0; i < n_readers; ++i)
    {
      g_thread_create((GThreadFunc)reader, spec_sim, FALSE, &thread_error);
      g_assert(thread_error == NULL);
    }

  for (i = 0; i < 100; ++i)
    {
      g_usleep(1000000);
      g_print(".");
    }

  g_printf("\n%d points verified\n", n_read);

  return 0;

}
