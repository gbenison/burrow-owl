
#include "segment-sim.h"
#include "spectrum_segmented.h"

G_DEFINE_TYPE (HosSpectrumSegmentSim, hos_spectrum_segment_sim, HOS_TYPE_SPECTRUM_SEGMENTED)

static void
hos_spectrum_segment_sim_class_init(HosSpectrumSegmentSimClass *klass)
{
  HosSpectrumSegmentedClass* segmented_class = HOS_SPECTRUM_SEGMENTED_CLASS(klass);
}

static void
hos_spectrum_segment_sim_init(HosSpectrumSegmentSim *self)
{
}


