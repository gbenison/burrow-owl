
#include <burrow.h>
#include "segment-sim.h"
#include "spectrum_priv.h"

static guint successes = 0;
static guint failures  = 0;

/*
 * pick an index at random from 'self' and ensure that it matches
 * the predicted value, in an infinite loop.
 */
static void
validate(HosSpectrum *self)
{
  gint np = spectrum_np(self, 0);
  while (1)
    {
      gint     idx       = g_random_int_range(0, np);
      gdouble  predicted = segment_sim_predict(HOS_SPECTRUM_SEGMENT_SIM(self), idx);
      gdouble  actual;
      gboolean result    = spectrum_tickle(self, self, &idx, &actual);

      if (result == TRUE)
	{
	  successes++;
	  if ((actual > 0) && (predicted > 0))
	    {
	      g_assert((actual / predicted) > 0.999999);
	      g_assert((actual / predicted) < 1.000001);
	    }
	}
      else
	failures++;

    }
}

int
main()
{
  g_print("===== t_segment_2 =====\n");

  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum* spec_sim = HOS_SPECTRUM(g_object_new(HOS_TYPE_SPECTRUM_SEGMENT_SIM, NULL));

  GError *thread_error = NULL;

  static gint n_readers = 5;
  gint i;
  for (i = 0; i < n_readers; ++i)
    {
      g_thread_create((GThreadFunc)validate, spec_sim, FALSE, &thread_error);
      g_assert(thread_error == NULL);
    }

  for (i = 0; i < 1000; ++i)
    {
      gint segid = g_random_int_range(0, (segment_sim_np / segment_sim_segment_size) - 1);
      if ((i % 10) == 0) g_print(".");
      spectrum_segmented_test_load_segment(HOS_SPECTRUM_SEGMENTED(spec_sim), segid);
    }

  g_printf("\n%8d cache hits\n%8d misses\n", successes, failures);

  return 0;

}
