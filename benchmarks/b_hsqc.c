
#include <burrow.h>

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *spec = HOS_SPECTRUM(spectrum_nih_from_file("hsqc.DAT"));

  g_assert(spec != NULL);

  spectrum_peek(spec, 0);

  return 0;

}
