
#include <burrow.h>

#define N_STRIP 10

static const double x_width = 0.1;

double strip_x[N_STRIP] = { 8.22,  7.03, 10.55,  6.32,  8.04, 
                            9.01,  9.55,  7.66,  8.22, 10.44};
double strip_y[N_STRIP] = {120.0, 115.6, 110.2, 124.9, 130.5,
                           113.4, 112.9, 111.1, 105.6, 107.9};

HosSpectrum*
extract_2d(HosSpectrum *root, double x, double y)
{
  int y_pt = spectrum_ppm2pt(root, 1, y);
  int x_min_pt = spectrum_ppm2pt(root, 0, x - x_width);
  int x_max_pt = spectrum_ppm2pt(root, 0, x + x_width);

  return
    spectrum_extract
    (spectrum_project
     (spectrum_transpose
      (root, 1),
      y_pt),
     x_min_pt, x_max_pt);
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosSpectrum *spec_1[N_STRIP];
  HosSpectrum *spec_2[N_STRIP];

  HosSpectrum *base_1 = spectrum_nih_from_file("cbcaconh.DAT");
  HosSpectrum *base_2 = spectrum_nih_from_file("hncacb.DAT");

  int i;
  for (i = 0; i < N_STRIP; ++i)
    {
      spec_1[i] = extract_2d(base_1, strip_x[i], strip_y[i]);
      spectrum_traverse(spec_1[i]);
    }

  for (i = 0; i < N_STRIP; ++i)
    {
      spectrum_peek(spec_1[i], 0);
    }

  for (i = 0; i < N_STRIP; ++i)
    {
      spec_2[i] = extract_2d(base_2, strip_x[i], strip_y[i]);
      spectrum_traverse(spec_2[i]);
    }

  for (i = 0; i < N_STRIP; ++i)
    {
      spectrum_peek(spec_2[i], 0);
    }

  return 0;
}
