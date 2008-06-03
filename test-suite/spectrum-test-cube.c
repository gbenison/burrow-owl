
#include "spectrum-test-cube.h"
#include "spectrum_priv.h"

static struct spectrum_iterator* test_cube_construct_iterator (HosSpectrum *self);
static void                      test_cube_free_iterator      (struct spectrum_iterator* self);
static gdouble   test_cube_wait    (struct spectrum_iterator* self);
static gboolean  test_cube_tickle  (struct spectrum_iterator* self, gdouble *dest);

G_DEFINE_TYPE (HosSpectrumTestCube, hos_spectrum_test_cube, HOS_TYPE_SPECTRUM)

static const int default_np = 100;
static gint np;

static void
hos_spectrum_test_cube_class_init(HosSpectrumTestCubeClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->construct_iterator = test_cube_construct_iterator;
  spectrum_class->free_iterator      = test_cube_free_iterator;
}

static void
hos_spectrum_test_cube_init(HosSpectrumTestCube *self)
{
  HosSpectrum* spectrum = HOS_SPECTRUM(self);
  GList* dimensions = NULL;
  gint i;

  np = default_np;
  const gchar *np_str = g_getenv("CUBE_NP");
  if (np_str != NULL)
    np = (gint)(g_ascii_strtod(np_str, NULL));

  for (i = 0; i < 3; ++i)
    {
      dimension_t* dimen = g_new0(dimension_t, 1);
      dimen->np   = np;
      dimen->sw   = 5000 * (i + 1);
      dimen->sf   = 1000;
      dimen->orig = 10000 + i * 200;
      dimensions  = g_list_append(dimensions, dimen);
    }

  spectrum_set_dimensions(spectrum, dimensions);
}

static struct spectrum_iterator*
test_cube_construct_iterator(HosSpectrum *self)
{
  struct spectrum_iterator* result = g_new0(struct spectrum_iterator, 1);

  result->tickle = test_cube_tickle;
  result->wait   = test_cube_wait;

  return result;
}

static void
test_cube_free_iterator(struct spectrum_iterator* self)
{
  g_free(self);
}

static gdouble
test_cube_wait (struct spectrum_iterator* self)
{
  return self->idx[0] + 1000 * self->idx[1] + 1e6 * self->idx[2];
}

static gboolean
test_cube_tickle     (struct spectrum_iterator* self, gdouble *dest)
{
  *dest = test_cube_wait(self);
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
  result += idx % np;
  idx /= np;
  result += (idx % np) * 1000;
  idx /= np;
  result += (idx % np) * 1e6;

  return result;
}

/*
 * returns:
 * predicted value of spectrum_peek(spectrum_integrate(spectrum_test_cube_new()), idx)
 */
gdouble
test_cube_I_predict (guint idx)
{
  gdouble start_value = (idx % np) * 1000;
  idx /= np;
  start_value += (idx % np) * 1e6;
  
  return (np / 2) * (start_value + start_value + np - 1);
}

/*
 * returns:
 * predicted value of spectrum_peek(spectrum_integrate(spectrum_integrate(spectrum_test_cube_new())), idx)
 */
gdouble
test_cube_II_predict (guint idx)
{
  gdouble start = test_cube_I_predict(idx * np);
  gdouble stop  = test_cube_I_predict(idx * np + np - 1);

  return (start + stop) * (np / 2);
}
