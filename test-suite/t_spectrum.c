
#include <burrow.h>
#include "spectrum-ramp.h"

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrumRamp *spec = spectrum_ramp_new();
  g_message("Made new ramped spectrum with %d dimensions\n", HOS_SPECTRUM(spec)->ndim);

  spectrum_traverse_blocking(HOS_SPECTRUM(spec));
  g_print("Values: ");
  int i;
  for (i = 0; i < 10; ++i)
    g_print("%f ", spectrum_peek(HOS_SPECTRUM(spec), i));
  g_print("...\n");

  return 0;
}

