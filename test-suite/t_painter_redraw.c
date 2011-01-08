#include <glib.h>
#include <glib/gstdio.h>
#include "burrow/spectrum.h"

/*
 * Test making a hard-copy plot of a spectrum.
 */

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  const gchar* base = g_getenv("EXAMPLE_DIR");
  g_assert(base != NULL);

#define BUF_LEN 1024
  gchar path[BUF_LEN];
  g_assert(g_snprintf(path, BUF_LEN, "%s/cbcaconh.DAT", base) < BUF_LEN);

  HosSpectrum *spec = spectrum_nih_from_file(path);

  g_assert(HOS_IS_SPECTRUM(spec));

  gchar *outfile = NULL;
  GError *open_error = NULL;
  gint outfile_handle = g_file_open_tmp("burrow.XXXXXX",
					&outfile,
					&open_error);
  g_assert(open_error == NULL);
  g_assert(outfile != NULL);
  g_assert(outfile_handle > 0);

  HosPainter *painter = HOS_PAINTER(painter_bwps_new_file(outfile));
  painter_set_spectrum(painter, spec);
  painter_redraw(painter);

  struct stat statbuf;
  g_stat(outfile, &statbuf);
  g_assert(statbuf.st_size > 0);

  g_unlink(outfile);

  return 0;
}

