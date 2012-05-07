
/*
 * A benchmark designed to stress the caching mechanism
 * by instantiating a lot of small spectra derived from
 * the same large one.
 */

#include "burrow/spectrum.h"

static char* fname = "test.DAT";

/*
 * A three-way convolution with signature
 * (H, N, H) -> (H, H, H)
 */
static HosSpectrum*
test_transform(HosSpectrum *s1)
{
  HosSpectrum *s1a = spectrum_transpose(s1, 2);    /* H, H, N */
  HosSpectrum *s2 = spectrum_diagonal_project(s1a); /* H, N */
  HosSpectrum *s3 = spectrum_convolute(s1, s2);    /* H1, N, H2, H', N' */
  HosSpectrum *s4 = spectrum_transpose(s3, 3);     /* H', H1, N, H2, N' */
  HosSpectrum *s5 = spectrum_diagonal_project(s4); /* H', N, H2, N' */
  HosSpectrum *s6 = spectrum_transpose(s5, 1);     /* N, H', H2, N' */
  HosSpectrum *s7 = spectrum_transpose(s6, 3);     /* N', N, H', H2 */
  HosSpectrum *s8 = spectrum_diagonal_project(s7); /* N', H', H2 */
  HosSpectrum *s9 = spectrum_convolute(s8, s1);    /* N', H', H2, H", N", H2" */
  HosSpectrum *s10 = spectrum_transpose(s9, 4);    /* N", N', H', H2, H", H2" */
  HosSpectrum *s11 = spectrum_diagonal_project(s10); /* N", H', H2, H", H2" */
  HosSpectrum *s12 = spectrum_project(s11, 0);       /* H', H2, H", H2" */
  HosSpectrum *s13 = spectrum_diagonal_project(s12); /* H', H", H2" */

  return s13;
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *s1 = spectrum_nih_from_file(fname);
  if (!s1)
    {
      fprintf(stderr, "Could not open %s -- need to generate with 'generate-test-file'?\n", fname);
      return 1;
    }
  if ((spectrum_ndim(s1) != 3) || (spectrum_np(s1, 0) != 790))
    {
      fprintf(stderr, "Unexpected characteristcs of input file %s; aborting", fname);
      return 1;
    }

  HosSpectrum *s2 = test_transform(s1);

  return 0;
}
