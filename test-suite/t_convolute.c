
#include <burrow.h>
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = spectrum_flakify(HOS_SPECTRUM(spectrum_ramp_new()), 0.5);
  HosSpectrum *S2 = spectrum_flakify(HOS_SPECTRUM(spectrum_ramp_new()), 0.5);
  HosSpectrum *S3 = spectrum_convolute(S1, S2);
			
  g_assert(spectrum_ndim(S3) == 2);
  g_assert(spectrum_np(S3, 0) == spectrum_np(S1, 0));
  g_assert(spectrum_np(S3, 1) == spectrum_np(S2, 0));

  gint i, j;
  gint peek_max = 10;
  if (peek_max >= spectrum_np(S1, 0))
    peek_max = spectrum_np(S1, 0);
  for (i = 0; i < peek_max; ++i)
    {
      for (j = 0; j < peek_max; ++j)
	g_print("%8.f ", spectrum_peek(S3, i + j * spectrum_np(S1, 0)));
      g_print("\n");
    }

  HosSpectrum *S4 = spectrum_integrate(S3);

  g_print("Integrated:\n");
  for (i = 0; i < peek_max; ++i)
    g_print("%8.f", spectrum_peek(S4, i));
  g_print("...\n\n");

  return 0;
}
