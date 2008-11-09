
#include "spectrum.h"

#define HOS_TYPE_SPECTRUM_TRANSPOSED              (hos_spectrum_transposed_get_type())
#define HOS_SPECTRUM_TRANSPOSED(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_TRANSPOSED, HosSpectrumTransposed))
#define HOS_SPECTRUM_TRANSPOSED_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_TRANSPOSED, HosSpectrumTransposedClass))
#define HOS_IS_SPECTRUM_TRANSPOSED(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_TRANSPOSED))
#define HOS_IS_SPECTRUM_TRANSPOSED_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_TRANSPOSED))
#define HOS_SPECTRUM_TRANSPOSED_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_TRANSPOSED, HosSpectrumTransposedClass))
					
typedef struct _HosSpectrumTransposed       HosSpectrumTransposed;
typedef struct _HosSpectrumTransposedClass  HosSpectrumTransposedClass;

struct _HosSpectrumTransposed
{
  HosSpectrum parent_instance;
};

struct _HosSpectrumTransposedClass
{
  HosSpectrumClass parent_instance;
};

HosSpectrum* CONSTRUCTOR spectrum_transpose        (HosSpectrum* self, guint dim);
