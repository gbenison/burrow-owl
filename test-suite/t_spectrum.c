
#include <burrow.h>
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrumRamp *spec = spectrum_ramp_new();
  g_message("Made new ramped spectrum with %d dimensions\n", spectrum_ndim(spec));

  HosSpectrumTestCube *cube = spectrum_test_cube_new();
  g_message("Made new test cube spectrum with %d dimensions\n", spectrum_ndim(cube));

  spectrum_traverse_blocking(HOS_SPECTRUM(spec));
  g_print("Ramp values: ");
  int i;
  for (i = 0; i < 10; ++i)
    g_print("%f ", spectrum_peek(HOS_SPECTRUM(spec), i));
  g_print("...\n");

  spectrum_traverse_blocking(HOS_SPECTRUM(cube));
  gint start;
  static const gint n = 10;
  for (start = 1; start < 100000; start *= 4)
    {
      gint i;
      g_print("Cube (%6d, ...) = ", start);
      for (i = 0; i < n; ++i)
	g_print("%.2f ", spectrum_peek(cube, start + i));
      g_print("\n");
    }

  return 0;
}

