
#include <math.h>
#include "segment-sim.h"
#include "spectrum_segmented.h"
#include "spectrum_priv.h"

static void sim_idx2segment  (gpointer env, guint *idx, gint *segid, gint *pt);
static void sim_read_segment (gpointer env, guint segid, gdouble *buf);
static gint env_get_int(const gchar *name, gint default_value);

G_DEFINE_TYPE (HosSpectrumSegmentSim, hos_spectrum_segment_sim, HOS_TYPE_SPECTRUM_SEGMENTED)

static guint segment_sim_segment_size;
static guint segment_sim_np;
static guint segment_sim_n_segment;

static guint* block2segment;
static guint* segment2block;


static void
hos_spectrum_segment_sim_class_init(HosSpectrumSegmentSimClass *klass)
{
  HosSpectrumSegmentedClass* segmented_class = HOS_SPECTRUM_SEGMENTED_CLASS(klass);

  segmented_class->idx2segment  = sim_idx2segment;
  segmented_class->read_segment = sim_read_segment;

  segment_sim_segment_size = env_get_int("SEGMENT_SIZE", 1024);
  segment_sim_n_segment    = env_get_int("N_SEGMENT", 128);
  segment_sim_np = segment_sim_segment_size * segment_sim_n_segment;

  block2segment = g_new0(guint, segment_sim_n_segment);
  segment2block = g_new0(guint, segment_sim_n_segment);

  int i;
  for (i = 0; i < segment_sim_n_segment; ++i)
    segment2block[i] = i;
  for (i = 0; i < segment_sim_n_segment; ++i)
    {
      guint j = g_random_int_range(0, segment_sim_n_segment);
      guint tmp = segment2block[i];
      segment2block[i] = segment2block[j];
      segment2block[j] = tmp;
    }
  for (i = 0; i < segment_sim_n_segment; ++i)
    block2segment[segment2block[i]] = i;
}

static void
hos_spectrum_segment_sim_init(HosSpectrumSegmentSim *self)
{
  dimension_t* dimen = g_new0(dimension_t, 1);
  dimen->np = segment_sim_np;

  spectrum_set_dimensions(HOS_SPECTRUM(self),
			  g_list_append(NULL, dimen));

  spectrum_segmented_set_segment_size(HOS_SPECTRUM_SEGMENTED(self), segment_sim_segment_size);

}

static void
sim_idx2segment(gpointer env, guint *idx, gint *segid, gint *pt)
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
sim_read_segment (gpointer env, guint segid, gdouble *buf)
{
  gint delay = g_random_int_range(0, 10);
  g_usleep(delay * 1e4);

  gint offset = segment2block[segid] * segment_sim_segment_size;
  gint i;
  for (i = 0; i < segment_sim_segment_size; ++i)
    buf[i] = offset + i;
}

static gint
env_get_int(const gchar *name, gint default_value)
{
  const gchar *value = g_getenv(name);
  if (value != NULL)
    return g_ascii_strtoll(value, NULL, 0);
  else
    return default_value;
}
