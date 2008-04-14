
#include "spectrum-ramp.h"
#include "spectrum_priv.h"

static gdouble  spectrum_ramp_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_ramp_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

G_DEFINE_TYPE (HosSpectrumRamp, hos_spectrum_ramp, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_ramp_class_init (HosSpectrumRampClass *klass)
{
  HosSpectrumClass* spectrum_class = HOS_SPECTRUM_CLASS(klass);

  spectrum_class->tickle     = spectrum_ramp_tickle;
  spectrum_class->accumulate = spectrum_ramp_accumulate;
}

static void
hos_spectrum_ramp_init(HosSpectrumRamp* self)
{
  HosSpectrum* spectrum = HOS_SPECTRUM(self);

  dimension_t* dimen = g_new0(dimension_t, 1);
  dimen->np = 100;

  spectrum_set_dimensions(spectrum, g_list_append(NULL, dimen));
}

static gboolean
spectrum_ramp_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = *idx;
  return TRUE;
}

static gdouble
spectrum_ramp_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  return (gdouble)(*idx);
}

HosSpectrumRamp*
spectrum_ramp_new()
{
  return HOS_SPECTRUM_RAMP(g_object_new(HOS_TYPE_SPECTRUM_RAMP, NULL));
}
