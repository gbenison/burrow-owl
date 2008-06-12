
#include <burrow.h>
#include <math.h>
#include "spectrum-ramp.h"
#include "spectrum-flaky.h"

/*
 * Attempt to exercise the 'cache collision' code path, where one
 * traversal thread overwrites a point cache entry while another
 * is trying to read the same entry.
 */

gint desired_size = 1024 * 32;
gint n_spectra = 5;

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

  g_print("==== point cache collision test ======\n");
  
  gint i;

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_ramp_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 0.9);

  /* FIXME enable following line to automatically adjust spectrum size */
  /* gint n = log(desired_size) / log(spectrum_np(S2, 0)) + 1; */
  gint n = 3;

  HosSpectrum *S3 = spectrum_make_huge(S2, n);
  for (i = 0; i < n_spectra; ++i)
    {
      HosSpectrum *S = spectrum_make_huge(spectrum_extract(S2, 2, spectrum_np(S2, 0) - i), n);
      spectrum_traverse(S);
    }

  spectrum_traverse(S3);

  while (1)
    {
      g_usleep(1 * G_USEC_PER_SEC);
      if (S3->buf != NULL)
	break;
      g_print(".");
    }

  /* validate */
  gdouble predicted = pow(((gdouble)spectrum_np(S2, 0) * (spectrum_np(S2, 0) - 1.0) / 2), n);
  gdouble actual    = spectrum_peek(S3, 0);
  g_assert((actual / predicted) > 0.9999);
  g_assert((actual / predicted) < 1.0001);

  g_printf("OK\n");

  return 0;
}



