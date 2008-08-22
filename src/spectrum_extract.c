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

struct extracted_iterator
{
  struct spectrum_iterator parent;

  struct spectrum_iterator    *base;
  HosSpectrumExtractedPrivate *priv;
};

static gdouble  spectrum_extracted_wait       (struct spectrum_iterator* self);
static gboolean spectrum_extracted_tickle     (struct spectrum_iterator* self, gdouble* dest);
static void     spectrum_extracted_increment  (struct spectrum_iterator* self, guint dim, gint delta);
static void     spectrum_extracted_mark       (struct spectrum_iterator* self);
static void     spectrum_extracted_restore    (struct spectrum_iterator* self);
static gboolean spectrum_extracted_probe      (struct spectrum_iterator* self);

static struct spectrum_iterator* spectrum_extracted_construct_iterator (HosSpectrum *self);
static void                      spectrum_extracted_free_iterator      (struct spectrum_iterator* self);


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

  spectrum_class->construct_iterator = spectrum_extracted_construct_iterator;
  spectrum_class->free_iterator      = spectrum_extracted_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumExtractedPrivate));
}

static void
hos_spectrum_extracted_init(HosSpectrumExtracted* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_extracted_wait(struct spectrum_iterator* self)
{
  return iterator_wait(((struct extracted_iterator*)self)->base);
}

static gboolean
spectrum_extracted_tickle(struct spectrum_iterator* self, gdouble* dest)
{
  return iterator_tickle(((struct extracted_iterator*)self)->base, dest);
}

static void
spectrum_extracted_increment(struct spectrum_iterator* self, guint dim, gint delta)
{
  iterator_increment(((struct extracted_iterator*)self)->base, dim, delta);
}

static void
spectrum_extracted_mark(struct spectrum_iterator* self)
{
  struct extracted_iterator* extracted_iterator = (struct extracted_iterator*)self;
  iterator_mark(extracted_iterator->base);
}

static void
spectrum_extracted_restore(struct spectrum_iterator* self)
{
  struct extracted_iterator* extracted_iterator = (struct extracted_iterator*)self;
  iterator_restore(extracted_iterator->base);
}

static gboolean
spectrum_extracted_probe(struct spectrum_iterator* self)
{
  struct extracted_iterator* extracted_iterator = (struct extracted_iterator*)self;
  iterator_probe(extracted_iterator->base);
}

static struct spectrum_iterator*
spectrum_extracted_construct_iterator (HosSpectrum *self)
{
  struct extracted_iterator *result            = g_new0(struct extracted_iterator, 1);
  struct spectrum_iterator  *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv = SPECTRUM_EXTRACTED_GET_PRIVATE(self);
  result->base = spectrum_construct_iterator(result->priv->base);

  iterator_increment(result->base, 0, result->priv->offset);

  spectrum_iterator->tickle     = spectrum_extracted_tickle;
  spectrum_iterator->mark       = spectrum_extracted_mark;
  spectrum_iterator->restore    = spectrum_extracted_restore;
  spectrum_iterator->wait       = spectrum_extracted_wait;
  spectrum_iterator->probe      = spectrum_extracted_probe;
  spectrum_iterator->increment  = spectrum_extracted_increment;

  return spectrum_iterator;
}

static void
spectrum_extracted_free_iterator(struct spectrum_iterator* self)
{
  iterator_free(((struct extracted_iterator*)self)->base);
  g_free(self);
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
