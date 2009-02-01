
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_printf("==== projection test ======\n");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);

  guint idx;
  for (idx = 0; idx < spectrum_np(S2, 0); ++idx)
    {
      g_printf("  Projecting index %d...", idx);
      HosSpectrum *S3 = spectrum_project(S2, idx);
      spectrum_traverse_blocking(S3);
      gint i;
      for (i = 0; i < spectrum_np_total(S3); ++i)
	{
	  gdouble actual    = spectrum_peek(S3, i);
	  gdouble predicted = test_cube_predict(i * spectrum_np(S2, 0) + idx);
	  if (predicted > 0)
	    {
	      g_assert(actual > (predicted * 0.9999999));
	      g_assert(actual < (predicted * 1.0000001));
	    }
	}
      g_printf("OK\n");
    }

  return 0;
}
