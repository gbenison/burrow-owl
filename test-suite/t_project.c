
#include <burrow.h>
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
      g_printf("  Projecting index %d...");
      HosSpectrum *S3 = spectrum_project(S2, idx);
      spectrum_traverse_blocking(S3);
      gint i;
      for (i = 0; i < spectrum_np_total(S3); ++i)
	g_assert(spectrum_peek(S3, i) == test_cube_predict(i * spectrum_np(S2, 0) + idx));
      g_printf("OK\n");
    }

  return 1; /* implementation incomplete */
}
