
#include <burrow.h>
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-flaky.h"

/*
 * Attempt to exercise the 'cache collision' code path, where one
 * traversal thread overwrites a point cache entry while another
 * is trying to read the same entry.
 */

// gint desired_size = 1024 * 1024 * 32;
gint desired_size = 1024 * 32;

static HosSpectrum*
spectrum_make_huge(HosSpectrum *spec, gint n)
{
  gint i;
  HosSpectrum *result = spec;
  for (i = 1; i < n; ++i)
    result = spectrum_convolute(result, spec);
  for (i = 0; i < n; ++i)
    result = spectrum_integrate(result);

  return result;
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_ramp_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 0.9);

  gint n = log(desired_size) / log(spectrum_np(S2, 0)) + 1;

  HosSpectrum *S3 = spectrum_make_huge(S2, n);
  HosSpectrum *S4 = spectrum_make_huge(spectrum_extract(S2, 1, 1e6), n);

  spectrum_traverse(S3);
  spectrum_traverse(S4);

  spectrum_traverse_blocking(S3);
  spectrum_traverse_blocking(S4);

  /* validate */
  gdouble predicted = pow((spectrum_np(S2, 0) * (spectrum_np(S2, 0) - 1) / 2), n);
  g_assert((spectrum_peek(S3, 0) / predicted) > 0.99);
  g_assert((spectrum_peek(S3, 0) / predicted) < 1.01);

  return 0;
}



