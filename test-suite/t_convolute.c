
#include "burrow/spectrum.h"
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"
#include "test-utils.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing convoluted spectra");

  HosSpectrum *S1 = spectrum_flakify(HOS_SPECTRUM(spectrum_ramp_new()), 0.5);
  HosSpectrum *S2 = spectrum_flakify(HOS_SPECTRUM(spectrum_ramp_new()), 0.5);
  HosSpectrum *S3 = spectrum_convolute(S1, S2);

  monitor_interval /= 28;
  spectrum_monitor(S3);
			
  g_assert(spectrum_ndim(S3) == 2);
  g_assert(spectrum_np(S3, 0) == spectrum_np(S1, 0));
  g_assert(spectrum_np(S3, 1) == spectrum_np(S2, 0));

  gint np_1 = spectrum_np(S1, 0);
  gint np_2 = spectrum_np(S2, 0);

  gint i1, i2;
  for (i1 = 0; i1 < np_1; ++i1)
    for (i2 = 0; i2 < np_2; ++i2)
      g_assert
	(IS_ABOUT_EQUAL(spectrum_peek(S3, i1 + np_1 * i2),
			(spectrum_peek(S1, i1) * spectrum_peek(S2, i2))));

  g_print("OK\n");

  return 0;
}
