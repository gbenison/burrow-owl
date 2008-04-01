
#ifndef HAVE_FLAKY_H
#define HAVE_FLAKY_H

#include "burrow.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_FLAKY             (hos_spectrum_flaky_get_type())
#define HOS_SPECTRUM_FLAKY(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_FLAKY, HosSpectrumFlaky))

typedef struct _HosSpectrumFlakyClass HosSpectrumFlakyClass;
typedef struct _HosSpectrumFlaky      HosSpectrumFlaky;

struct _HosSpectrumFlakyClass
{
  HosSpectrumClass parent_class;
};

struct _HosSpectrumFlaky
{
  HosSpectrum parent_instance;

  HosSpectrum *flakand;
  gdouble      flake_factor;
};

HosSpectrum* spectrum_flakify(HosSpectrum *self, gdouble flake_factor);

GType hos_spectrum_flaky_get_type (void);


G_END_DECLS


#endif  /* not HAVE_FLAKY_H */

