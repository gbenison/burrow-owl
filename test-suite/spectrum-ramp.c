
#include "spectrum-ramp.h"
#include "spectrum_priv.h"


static struct spectrum_iterator* spectrum_ramp_construct_iterator (HosSpectrum *self);
static void                      spectrum_ramp_free_iterator      (struct spectrum_iterator* self);
static gboolean                  spectrum_ramp_tickle     (struct spectrum_iterator* self, gdouble *dest);
static gdouble                   spectrum_ramp_accumulate (struct spectrum_iterator* self);

G_DEFINE_TYPE (HosSpectrumRamp, hos_spectrum_ramp, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_ramp_class_init (HosSpectrumRampClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->construct_iterator = spectrum_ramp_construct_iterator;
  spectrum_class->free_iterator      = spectrum_ramp_free_iterator;
}

static void
hos_spectrum_ramp_init(HosSpectrumRamp* self)
{
  HosSpectrum* spectrum = HOS_SPECTRUM(self);

  dimension_t* dimen = g_new0(dimension_t, 1);
  dimen->np = 100;

  spectrum_set_dimensions(spectrum, g_list_append(NULL, dimen));
}

static struct spectrum_iterator*
spectrum_ramp_construct_iterator (HosSpectrum *self)
{
  struct spectrum_iterator* result = g_new0(struct spectrum_iterator, 1);

  result->tickle     = spectrum_ramp_tickle;
  result->accumulate = spectrum_ramp_accumulate;

  return result;
}

static void
spectrum_ramp_free_iterator(struct spectrum_iterator* self)
{
  g_free(self);
}

static gboolean
spectrum_ramp_tickle(struct spectrum_iterator* self, gdouble *dest)
{
  *dest = (gdouble)(*(self->idx));
  return TRUE;
}

static gdouble
spectrum_ramp_accumulate(struct spectrum_iterator* self)
{
  return (gdouble)(*(self->idx));
}


HosSpectrumRamp*
spectrum_ramp_new()
{
  return HOS_SPECTRUM_RAMP(g_object_new(HOS_TYPE_SPECTRUM_RAMP, NULL));
}
