
#include <burrow.h>

typedef struct _HosTestSpectrumClass HosTestSpectrumClass;
typedef struct _HosTestSpectrum      HosTestSpectrum;

struct _HosTestSpectrumClass
{
  HosSpectrumClass parent_class;
};

struct _HosTestSpectrum
{
  HosSpectrum parent_instance;
};

static gboolean test_spectrum_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);
static gboolean test_spectrum_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

G_DEFINE_TYPE (HosTestSpectrum, hos_test_spectrum, HOS_TYPE_SPECTRUM)

static void
hos_test_spectrum_class_init (HosTestSpectrumClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->tickle     = test_spectrum_tickle;
  spectrum_class->accumulate = test_spectrum_accumulate;
}

static void
hos_test_spectrum_init(HosTestSpectrum* self)
{
  HosSpectrum* spectrum = HOS_SPECTRUM(self);
  spectrum->ndim = 1;
  spectrum->np   = g_new0(gint, 1);
  spectrum->np[0] = 100;
}

static gboolean
test_spectrum_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = *idx;
  return TRUE;
}

static gboolean
test_spectrum_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = *idx;
  return TRUE;
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  HosTestSpectrum *spec = g_object_new(hos_test_spectrum_get_type(), NULL);
  g_message("Made new test spectrum with %d dimensions\n", HOS_SPECTRUM(spec)->ndim);

  spectrum_traverse_blocking(HOS_SPECTRUM(spec));
  g_print("Values: ");
  int i;
  for (i = 0; i < 10; ++i)
    g_print("%f ", spectrum_peek(HOS_SPECTRUM(spec), i));
  g_print("...\n");

  return 0;
}

