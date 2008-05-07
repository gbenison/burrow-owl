/*
 *  Copyright (C) 2008 Greg Benison
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "burrow/spectrum.h"
#include "spectrum_priv.h"
#include "utils.h"

/* no new public fields */
typedef HosSpectrum      HosSpectrumExtracted;
typedef HosSpectrumClass HosSpectrumExtractedClass;

#define HOS_TYPE_SPECTRUM_EXTRACTED              (hos_spectrum_extracted_get_type())

#define SPECTRUM_EXTRACTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_EXTRACTED, HosSpectrumExtractedPrivate))
#define SPECTRUM_EXTRACTED_PRIVATE(o, field) ((SPECTRUM_EXTRACTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumExtractedPrivate HosSpectrumExtractedPrivate;

struct _HosSpectrumExtractedPrivate
{
  HosSpectrum *base;
  guint offset;
};

static gdouble  spectrum_extracted_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
static gboolean spectrum_extracted_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

static void   spectrum_extracted_dispose  (GObject *object);
static void   spectrum_extracted_finalize (GObject *object);


G_DEFINE_TYPE (HosSpectrumExtracted, hos_spectrum_extracted, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_extracted_class_init(HosSpectrumExtractedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_extracted_dispose;
  gobject_class->finalize    = spectrum_extracted_finalize;

  spectrum_class->accumulate = spectrum_extracted_accumulate;
  spectrum_class->tickle     = spectrum_extracted_tickle;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumExtractedPrivate));
}

static void
hos_spectrum_extracted_init(HosSpectrumExtracted* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_extracted_accumulate(HosSpectrum* self, HosSpectrum* root, guint* idx)
{
  HosSpectrumExtractedPrivate *priv = SPECTRUM_EXTRACTED_GET_PRIVATE(self);
  idx[0] += priv->offset;
  gdouble result = spectrum_accumulate(priv->base, root, idx);
  idx[0] -= priv->offset;
  return result;
}

static gboolean
spectrum_extracted_tickle(HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest)
{
  HosSpectrumExtractedPrivate *priv = SPECTRUM_EXTRACTED_GET_PRIVATE(self);
  idx[0] += priv->offset;
  gboolean result = spectrum_tickle(priv->base, root, idx, dest);
  idx[0] -= priv->offset;
  return result;
}

static void
spectrum_extracted_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_EXTRACTED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_extracted_parent_class)->dispose (object);
}

static void
spectrum_extracted_finalize(GObject *object)
{
  HosSpectrumExtractedPrivate *priv = SPECTRUM_EXTRACTED_GET_PRIVATE(object);
  G_OBJECT_CLASS(hos_spectrum_extracted_parent_class)->finalize (object);
}

/*
 * Return the extraction of 'self' between points 'start' and 'stop'.
 * e.g.
 *
 * S'(i, j, k, ...) = S(start + i, j, k, ...)
 */
HosSpectrum*
spectrum_extract(HosSpectrum *self, guint start, guint stop)
{
  /* clamp values */
  if (stop < start)
    {
      guint tmp = start;
      start = stop;
      stop = tmp;
    }

  if (start < 0) start = 0;
  if (stop >= spectrum_np(self, 0))
    stop = spectrum_np(self, 0) - 1;

  if ((start == 0) && (stop == spectrum_np(self, 0)))
    return self;
  else
    {
      HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_EXTRACTED, NULL);
      GList* dimensions   = spectrum_copy_dimensions(self);

      gdouble pt2hz = spectrum_sw(self, 0) / spectrum_np(self, 0);

      dimension_t* first_dimension = (dimension_t*)(dimensions->data);
      first_dimension->np   = stop - start + 1;
      first_dimension->orig = first_dimension->orig - start * pt2hz;
      first_dimension->sw   = first_dimension->np * pt2hz;

      g_object_ref(self);

      HosSpectrumExtractedPrivate *priv = SPECTRUM_EXTRACTED_GET_PRIVATE(result);

      priv->base      = self;
      priv->offset    = start;

      spectrum_set_dimensions(result, dimensions);
      
      return result;
    }
  
  g_assert_not_reached();

}
