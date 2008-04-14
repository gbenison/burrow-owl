
#include <burrow.h>
#include "spectrum-test-cube.h"
#include "spectrum-flaky.h"

static guint
spectrum_ppm2idx(HosSpectrum *S, gdouble x, gdouble y, gdouble z)
{
  gint x_pt = spectrum_ppm2pt(S, 0, x);
  gint y_pt = spectrum_ppm2pt(S, 1, y);
  gint z_pt = spectrum_ppm2pt(S, 2, z);

  return
    x_pt +
    y_pt * spectrum_np(S, 0) +
    z_pt * spectrum_np(S, 0) * spectrum_np(S, 1);
}

static void
extract_test(HosSpectrum *S)
{
  gdouble x1 = g_random_double_range(spectrum_giro_ppm(S, 0), spectrum_orig_ppm(S, 0));
  gdouble xn = g_random_double_range(spectrum_giro_ppm(S, 0), spectrum_orig_ppm(S, 0));
  gdouble x = (x1 < xn) ? g_random_double_range(x1, xn) : g_random_double_range(xn, x1);
  gdouble y = g_random_double_range(spectrum_giro_ppm(S, 0), spectrum_orig_ppm(S, 0));
  gdouble z = g_random_double_range(spectrum_giro_ppm(S, 0), spectrum_orig_ppm(S, 0));

  HosSpectrum* Sprime = spectrum_extract_ppm(S, x1, xn);

  gdouble base    = spectrum_peek(S, spectrum_ppm2idx(S, x, y, z));
  gdouble derived = spectrum_peek(Sprime, spectrum_ppm2idx(Sprime, x, y, z));

  g_assert (base == derived);
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *S1 = HOS_SPECTRUM(spectrum_test_cube_new());
  HosSpectrum *S2 = spectrum_flakify(S1, 1.0 - 2e-4);

  g_printf("==== extraction test ======\n");
  spectrum_traverse_blocking(S2);

  gint i;
  for (i = 0; i < 50; ++i)
    {
      extract_test(S2);
      g_printf(".");
    }
  g_print("OK\n\n");

  return 0;
}

