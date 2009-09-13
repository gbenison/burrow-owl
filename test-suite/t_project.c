
#include "burrow/spectrum.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"
#include "test-utils.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing spectrum_project()");

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);

  guint idx;
  for (idx = 0; idx < spectrum_np(S2, 0); ++idx)
    {
      HosSpectrum *S3 = spectrum_project(S2, idx);
      spectrum_traverse_blocking(S3);
      gint i;
      for (i = 0; i < spectrum_np_total(S3); ++i)
	g_assert
	  (IS_ABOUT_EQUAL(spectrum_peek(S3, i),
			  test_cube_predict(i * spectrum_np(S2, 0) + idx)));

      if ((idx % 4) == 0) g_print(".");
    }

  g_print("OK\n");

  return 0;
}
