
#include "burrow/spectrum.h"
#include "spectrum-ramp.h"
#include "spectrum-test-cube.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing ramp spectrum");
  HosSpectrum *spec = HOS_SPECTRUM(spectrum_ramp_new());
  spectrum_traverse_blocking(HOS_SPECTRUM(spec));
  g_print("......OK\n");

  g_print("Testing test_cube");
  HosSpectrum *cube = HOS_SPECTRUM(spectrum_test_cube_new());
  spectrum_traverse_blocking(HOS_SPECTRUM(cube));
  g_print("......OK\n");

  return 0;
}

