
#include "segment-sim.h"
#include "spectrum_segmented.h"
#include "spectrum_priv.h"

static void sim_idx2segment  (HosSpectrumSegmented *self, guint *idx, gint *segid, gint *pt);
static void sim_read_segment (HosSpectrumSegmented *self, guint segid, gdouble *buf);

G_DEFINE_TYPE (HosSpectrumSegmentSim, hos_spectrum_segment_sim, HOS_TYPE_SPECTRUM_SEGMENTED)

static const guint segment_size = 1024;
static const guint np = 100000;

static void
hos_spectrum_segment_sim_class_init(HosSpectrumSegmentSimClass *klass)
{
  HosSpectrumSegmentedClass* segmented_class = HOS_SPECTRUM_SEGMENTED_CLASS(klass);

  segmented_class->idx2segment  = sim_idx2segment;
  segmented_class->read_segment = sim_read_segment;
}

static void
hos_spectrum_segment_sim_init(HosSpectrumSegmentSim *self)
{
  spectrum_set_ndim(HOS_SPECTRUM(self), 1);
  spectrum_set_np(HOS_SPECTRUM(self), 0, np);

  HOS_SPECTRUM_SEGMENTED(self)->segment_size = segment_size;
}

static void
sim_idx2segment(HosSpectrumSegmented *self, guint *idx, gint *segid, gint *pt)
{
  *pt    = idx[0] % segment_size;
  *segid = idx[0] / segment_size;
}

gdouble
segment_sim_predict(HosSpectrumSegmentSim* self, guint idx)
{
  return idx;
}

/*
 * Fill 'buf' (which must be big enough) with points of segment 'segid'
 */
static void
sim_read_segment (HosSpectrumSegmented *self, guint segid, gdouble *buf)
{
  gint offset = segid * segment_size;
  gint i;
  for (i = 0; i < segment_size; ++i)
    buf[i] = offset + i;
}
