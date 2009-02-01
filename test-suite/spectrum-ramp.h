

#ifndef HAVE_SPECTRUM_RAMP
#define HAVE_SPECTRUM_RAMP

#include "burrow/spectrum.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_RAMP             (hos_spectrum_ramp_get_type())
#define HOS_SPECTRUM_RAMP(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_RAMP, HosSpectrumRamp))

typedef struct _HosSpectrumRampClass HosSpectrumRampClass;
typedef struct _HosSpectrumRamp      HosSpectrumRamp;

struct _HosSpectrumRampClass
{
  HosSpectrumClass parent_class;
};

struct _HosSpectrumRamp
{
  HosSpectrum parent_instance;
};

HosSpectrumRamp* spectrum_ramp_new(void);

GType hos_spectrum_ramp_get_type (void);


G_END_DECLS

#endif /* not  HAVE_SPECTRUM_RAMP */
