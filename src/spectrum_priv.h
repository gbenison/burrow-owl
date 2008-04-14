
/* private API of 'spectrum' object */

#ifndef _HOS_HAVE_SPECTRUM_PRIV_H
#define _HOS_HAVE_SPECTRUM_PRIV_H

#include "burrow/spectrum.h"

/*
 * 'spectrum->buf' contains the spectral data.
 * A special value of 'spectrum->buf[i]' indicates that
 * value 'i' is not yet known.
 */
#define DATUM_UNKNOWN_VALUE             0
#define DATUM_UNKNOWN_VALUE_SUBSTITUTE  G_MINDOUBLE
#define DATUM_ENSURE_KNOWN(x)           {if ((x) == DATUM_UNKNOWN_VALUE) (x) = DATUM_UNKNOWN_VALUE_SUBSTITUTE;}
#define DATUM_IS_KNOWN(x)               ((x) != DATUM_UNKNOWN_VALUE)

typedef struct _dimension dimension_t;
struct _dimension
{
  guint   np;
  gdouble orig;
  gdouble sf;
  gdouble sw;
};

gdouble  spectrum_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
gboolean spectrum_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

GList*   spectrum_copy_dimensions (HosSpectrum *self);
void     spectrum_set_dimensions  (HosSpectrum *self, GList *dimensions);

#endif /* not  _HOS_HAVE_SPECTRUM_PRIV_H */

