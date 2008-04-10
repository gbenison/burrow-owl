
#include <math.h>
#include "segment-sim.h"
#include "spectrum_segmented.h"
#include "spectrum_priv.h"

static void sim_idx2segment  (HosSpectrumSegmented *self, guint *idx, gint *segid, gint *pt);
static void sim_read_segment (HosSpectrumSegmented *self, guint segid, gdouble *buf);

G_DEFINE_TYPE (HosSpectrumSegmentSim, hos_spectrum_segment_sim, HOS_TYPE_SPECTRUM_SEGMENTED)

const  guint segment_sim_segment_size = 1024;
const  guint segment_sim_np = 100000;
static guint n_segment;

static guint* block2segment;
static guint* segment2block;


static void
hos_spectrum_segment_sim_class_init(HosSpectrumSegmentSimClass *klass)
{
  HosSpectrumSegmentedClass* segmented_class = HOS_SPECTRUM_SEGMENTED_CLASS(klass);

  segmented_class->idx2segment  = sim_idx2segment;
  segmented_class->read_segment = sim_read_segment;

  n_segment = (guint)ceil((double)segment_sim_np / segment_sim_segment_size);
  block2segment = g_new0(guint, n_segment);
  segment2block = g_new0(guint, n_segment);

  int i;
  for (i = 0; i < n_segment; ++i)
    segment2block[i] = i;
  for (i = 0; i < n_segment; ++i)
    {
      guint j = g_random_int_range(0, n_segment);
      guint tmp = segment2block[i];
      segment2block[i] = segment2block[j];
      segment2block[j] = tmp;
    }
  for (i = 0; i < n_segment; ++i)
    block2segment[segment2block[i]] = i;
}

static void
hos_spectrum_segment_sim_init(HosSpectrumSegmentSim *self)
{
  spectrum_set_ndim(HOS_SPECTRUM(self), 1);
  spectrum_set_np(HOS_SPECTRUM(self), 0, segment_sim_np);

  spectrum_segmented_set_segment_size(HOS_SPECTRUM_SEGMENTED(self), segment_sim_segment_size);

}

static void
sim_idx2segment(HosSpectrumSegmented *self, guint *idx, gint *segid, gint *pt)
{
  *pt    = idx[0] % segment_sim_segment_size;
  *segid = block2segment[idx[0] / segment_sim_segment_size];
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
  gint delay = g_random_int_range(0, 10);
  g_usleep(delay * 1e5);

  gint offset = segment2block[segid] * segment_sim_segment_size;
  gint i;
  for (i = 0; i < segment_sim_segment_size; ++i)
    buf[i] = offset + i;
}
