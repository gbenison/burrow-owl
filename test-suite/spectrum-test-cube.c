
#include "spectrum-test-cube.h"

static gdouble  test_cube_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean test_cube_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

G_DEFINE_TYPE (HosSpectrumTestCube, hos_spectrum_test_cube, HOS_TYPE_SPECTRUM)

static const int default_np = 100;

static void
hos_spectrum_test_cube_class_init(HosSpectrumTestCubeClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->tickle     = test_cube_tickle;
  spectrum_class->accumulate = test_cube_accumulate;
}

static void
hos_spectrum_test_cube_init(HosSpectrumTestCube *self)
{
  HosSpectrum* spectrum = HOS_SPECTRUM(self);
  spectrum->ndim  = 3;
  spectrum->np    = g_new0(gint, 3);
  spectrum->np[0] = default_np;
  spectrum->np[1] = default_np;
  spectrum->np[2] = default_np;
}

static gdouble
test_cube_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  return idx[0] + 1000 * idx[1] + 1e6 * idx[2];
}

static gboolean
test_cube_tickle (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = test_cube_accumulate(self, root, idx);
  return TRUE;
}

HosSpectrumTestCube*
spectrum_test_cube_new()
{
  return HOS_SPECTRUM_TEST_CUBE(g_object_new(HOS_TYPE_SPECTRUM_TEST_CUBE, NULL));
}
