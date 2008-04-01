
#include <burrow.h>
#include <math.h>
#include "spectrum-ramp.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_ramp_new());
  g_message("Made ramped spectrum S1 with %d dimensions\n", spectrum_ndim(S1));
  HosSpectrum *S2 = spectrum_integrate(S1);
  g_message("Integrated S1 to obtain S2 with %d dimensions\n", spectrum_ndim(S2));

  guint np = spectrum_np(S1, 0);
  gdouble theoretical = (np * (np - 1) / 2);
  gdouble *data = spectrum_traverse_blocking(S2);
  gdouble actual = *data;
  g_print("S2 value: %f (%f)\n ", actual, theoretical);

#define ABOUT_EQUAL(a, b) (fabs((a) - (b)) < 0.001)

  g_assert(ABOUT_EQUAL(theoretical, actual));

  return 0;
}

