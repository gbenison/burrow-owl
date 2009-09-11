
#ifndef HAVE_SEGMENT_SIM_H
#define HAVE_SEGMENT_SIM_H

#include "burrow/spectrum.h"
#include "spectrum_segmented.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_SEGMENT_SIM  (hos_spectrum_segment_sim_get_type())
#define HOS_SPECTRUM_SEGMENT_SIM(obj)  (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_SEGMENT_SIM, HosSpectrumSegmentSim))

typedef struct _HosSpectrumSegmentSimClass HosSpectrumSegmentSimClass;
typedef struct _HosSpectrumSegmentSim      HosSpectrumSegmentSim;

struct _HosSpectrumSegmentSimClass
{
  HosSpectrumSegmentedClass parent_class;
};

struct _HosSpectrumSegmentSim
{
  HosSpectrumSegmented parent_instance;
};

gdouble segment_sim_predict (HosSpectrumSegmentSim* self, guint idx);
void    segment_sim_validate (HosSpectrumSegmentSim* self);

G_END_DECLS

#endif /* not HAVE_SEGMENT_SIM_H */


