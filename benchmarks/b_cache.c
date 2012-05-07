
/*
 * A benchmark designed to stress the caching mechanism
 * by instantiating a lot of small spectra derived from
 * the same large one.
 */

#include <assert.h>
#include "burrow/spectrum.h"

static char* fname = "test.DAT";

/*
 * A three-way convolution with signature
 * (H, N, H) -> (H, H, H)
 */
static HosSpectrum*
test_transform(HosSpectrum *sa)
{
  HosSpectrum *sb = sa;
  HosSpectrum *sc = sa;

  HosSpectrum *s1 = spectrum_transpose(sa, 2);    /* H, H, N */
  HosSpectrum *s2 = spectrum_diagonal_project(s1); /* H, N */
  HosSpectrum *s3 = spectrum_convolute(sb, s2);    /* H1, N, H2, H', N' */
  HosSpectrum *s4 = spectrum_transpose(s3, 3);     /* H', H1, N, H2, N' */
  HosSpectrum *s5 = spectrum_diagonal_project(s4); /* H', N, H2, N' */
  HosSpectrum *s6 = spectrum_transpose(s5, 1);     /* N, H', H2, N' */
  HosSpectrum *s7 = spectrum_transpose(s6, 3);     /* N', N, H', H2 */
  HosSpectrum *s8 = spectrum_diagonal_project(s7); /* N', H', H2 */
  HosSpectrum *s9 = spectrum_convolute(s8, sc);    /* N', H', H2, H", N", H2" */
  HosSpectrum *s10 = spectrum_transpose(s9, 4);    /* N", N', H', H2, H", H2" */
  HosSpectrum *s11 = spectrum_diagonal_project(s10); /* N", H', H2, H", H2" */
  HosSpectrum *s12 = spectrum_project(s11, 0);       /* H', H2, H", H2" */
  HosSpectrum *s13 = spectrum_diagonal_project(s12); /* H', H", H2" */

  return s13;
}

/*
 * Blocking full integration of a 2d spectrum.
 */
static double
spectrum_integrate_2d(HosSpectrum *s)
{
  return spectrum_peek(spectrum_integrate(spectrum_integrate(s)), 0);
}

/*
 * Expected results of spectrum_integrate_2d(spectrum_project(s1, n))
 * where s1 is test_transform(-> "test.DAT" <-)
 */
static const double expected_results[] =
  {
    -7.08926e20,
    -7.19829e21,
    -6.39958e21,
    -1.80110e22,
    -4.46076e21
  };

static const int n_test_planes = 5;

#define EQUAL_ENOUGH(a, b) (0.99 < ((a)/(b)) < 1.01)

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

  /*
   * Create several instances of the transformed spectrum.
   */
  int i;

  /*** set 'A' ***/
  HosSpectrum *sA = test_transform(s1);
  HosSpectrum *sAp[n_test_planes];
  for (i = 0; i < n_test_planes; ++i)
    {
      sAp[i] = spectrum_project(sA, i);
      spectrum_traverse(sAp[i]);
    }

  /* ensure correctness of results (and incidentally, wait for traversals) */
  for (i = 0; i < n_test_planes; ++i)
    assert(EQUAL_ENOUGH(spectrum_integrate_2d(sAp[i]), expected_results[i])); 

  return 0;
}
