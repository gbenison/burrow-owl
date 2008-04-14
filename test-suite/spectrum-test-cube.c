
#include "spectrum-test-cube.h"
#include "spectrum_priv.h"

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
  GList* dimensions = NULL;
  gint i;
  for (i = 0; i < 3; ++i)
    {
      dimension_t* dimen = g_new0(dimension_t, 1);
      dimen->np = default_np;
      dimensions = g_list_append(dimensions, dimen);
    }

  spectrum_set_dimensions(spectrum, dimensions);
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

/*
 * returns:
 * predicted value of spectrum_peek(spectrum_test_cube_new(), idx)
 */
gdouble
test_cube_predict (guint idx)
{
  gdouble result = 0;
  result += idx % default_np;
  idx /= default_np;
  result += (idx % default_np) * 1000;
  idx /= default_np;
  result += (idx % default_np) * 1e6;

  return result;
}

/*
 * returns:
 * predicted value of spectrum_peek(spectrum_integrate(spectrum_test_cube_new()), idx)
 */
gdouble
test_cube_I_predict (guint idx)
{
  gdouble start_value = (idx % default_np) * 1000;
  idx /= default_np;
  start_value += (idx % default_np) * 1e6;
  
  return (default_np / 2) * (start_value + start_value + default_np - 1);
}

/*
 * returns:
 * predicted value of spectrum_peek(spectrum_integrate(spectrum_integrate(spectrum_test_cube_new())), idx)
 */
gdouble
test_cube_II_predict (guint idx)
{
  gdouble start = test_cube_I_predict(idx * default_np);
  gdouble stop  = test_cube_I_predict(idx * default_np + default_np - 1);

  return (start + stop) * (default_np / 2);
}
