
#include "spectrum-ramp.h"


static gboolean spectrum_ramp_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);
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
  spectrum->ndim = 1;
  spectrum->np   = g_new0(gint, 1);
  spectrum->np[0] = 100;
}

static gboolean
spectrum_ramp_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = *idx;
  return TRUE;
}

static gboolean
spectrum_ramp_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  *dest = *idx;
  return TRUE;
}

HosSpectrumRamp*
spectrum_ramp_new()
{
  return HOS_SPECTRUM_RAMP(g_object_new(HOS_TYPE_SPECTRUM_RAMP, NULL));
}
