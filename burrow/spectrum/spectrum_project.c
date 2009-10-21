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

#include <string.h>
#include "spectrum_project.h"
#include "spectrum.h"
#include "spectrum_priv.h"
#include "utils.h"

#define SPECTRUM_PROJECTED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_PROJECTED, HosSpectrumProjectedPrivate))
#define SPECTRUM_PROJECTED_PRIVATE(o, field) ((SPECTRUM_PROJECTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumProjectedPrivate HosSpectrumProjectedPrivate;

struct _HosSpectrumProjectedPrivate
{
  HosSpectrum *base;
  
  guint offset;
};

struct projected_iterator
{
  struct spectrum_iterator parent;

  struct spectrum_iterator    *base;
  HosSpectrumProjectedPrivate *priv;
};

static gboolean spectrum_projected_tickle     (struct spectrum_iterator* self, gdouble* dest);
static void     spectrum_projected_mark       (struct spectrum_iterator* self);
static void     spectrum_projected_restore    (struct spectrum_iterator* self);
static gdouble  spectrum_projected_wait       (struct spectrum_iterator* self);
static gboolean spectrum_projected_probe      (struct spectrum_iterator* self);
static void     spectrum_projected_increment  (struct spectrum_iterator* self, guint dim, gint delta);

static struct spectrum_iterator* spectrum_projected_construct_iterator (HosSpectrum *self);
static void                      spectrum_projected_free_iterator      (struct spectrum_iterator* self);

static void   spectrum_projected_dispose  (GObject *object);
static void   spectrum_projected_finalize (GObject *object);

G_DEFINE_TYPE (HosSpectrumProjected, hos_spectrum_projected, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_projected_class_init(HosSpectrumProjectedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_projected_dispose;
  gobject_class->finalize    = spectrum_projected_finalize;

  spectrum_class->construct_iterator = spectrum_projected_construct_iterator;
  spectrum_class->free_iterator      = spectrum_projected_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumProjectedPrivate));
}

static void
hos_spectrum_projected_init(HosSpectrumProjected* self)
{
  /* FIXME */
  /* anything? */
}

static gdouble
spectrum_projected_wait(struct spectrum_iterator* self)
{
  return iterator_wait(((struct projected_iterator*)self)->base);
}

static gboolean
spectrum_projected_tickle(struct spectrum_iterator* self, gdouble* dest)
{
  return iterator_tickle(((struct projected_iterator*)self)->base, dest);
}

static void
spectrum_projected_increment(struct spectrum_iterator* self, guint dim, gint delta)
{
  HosSpectrumProjectedPrivate *priv = ((struct projected_iterator*)self)->priv;
  struct spectrum_iterator    *base = ((struct projected_iterator*)self)->base;

  iterator_increment(base, dim + 1, delta);
}

static void
spectrum_projected_mark(struct spectrum_iterator* self)
{
  struct projected_iterator* projected_iterator = (struct projected_iterator*)self;
  iterator_mark(projected_iterator->base);
}

static void
spectrum_projected_restore(struct spectrum_iterator* self)
{
  struct projected_iterator* projected_iterator = (struct projected_iterator*)self;
  iterator_restore(projected_iterator->base);
}

static gboolean
spectrum_projected_probe(struct spectrum_iterator* self)
{
  return iterator_probe(((struct projected_iterator*)self)->base);
}

static void
spectrum_projected_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_PROJECTED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_projected_parent_class)->dispose (object);
}

static void
spectrum_projected_finalize(GObject *object)
{
  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(object);

  G_OBJECT_CLASS(hos_spectrum_projected_parent_class)->finalize (object);
}

/**
 * @brief    Obtain a projection (section) of a spectrum
 * @ingroup  HosSpectrum
 *
 * Keep only points having a particular index in the leading dimension,
 * discarding all other points, to obtain a resulting spectrum with
 * one fewer dimensions.  Can also be thought of as taking a slice through
 * the leading dimension.
 *
 * For example if S' = spectrum_project(S, idx):
 *
 * @f$ S'(i, j, k, ...) = S(idx, i, j, k, ...) @f$
 *
 * @param   self   Spectrum to project
 * @param   idx    index (in spectrum points) of the retained index in dimension 0
 */
HosSpectrum*
spectrum_project(HosSpectrum *self, guint idx)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_PROJECTED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);

  g_free(g_list_nth_data(dimensions, 0));
  dimensions = g_list_delete_link(dimensions, dimensions);

  g_object_ref(self);

  HosSpectrumProjectedPrivate *priv = SPECTRUM_PROJECTED_GET_PRIVATE(result);

  priv->base   = self;
  priv->offset = idx;

  spectrum_set_dimensions(result, dimensions);

  return result;

}

static struct spectrum_iterator*
spectrum_projected_construct_iterator(HosSpectrum *self)
{
  struct projected_iterator *result            = g_new0(struct projected_iterator, 1);
  struct spectrum_iterator  *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv = SPECTRUM_PROJECTED_GET_PRIVATE(self);
  result->base = spectrum_construct_iterator(result->priv->base);

  iterator_increment(result->base, 0, result->priv->offset);

  spectrum_iterator->tickle     = spectrum_projected_tickle;
  spectrum_iterator->wait       = spectrum_projected_wait;
  spectrum_iterator->increment  = spectrum_projected_increment;
  spectrum_iterator->mark       = spectrum_projected_mark;
  spectrum_iterator->restore    = spectrum_projected_restore;
  spectrum_iterator->probe      = spectrum_projected_probe;

  return spectrum_iterator;
}

static void
spectrum_projected_free_iterator(struct spectrum_iterator* self)
{
  iterator_free(((struct projected_iterator*)self)->base);
  g_free(self);
}

/**
 * @ingroup HosSpectrum
 * @brief   Obtain a projection, specifying the position in ppm
 *
 * Like spectrum_project(), but the position argument is specified
 * in units of ppm, rather than spectral points
 */
HosSpectrum*
spectrum_project_ppm(HosSpectrum* self, const gdouble ppm)
{
  return spectrum_project(self, spectrum_ppm2pt(self, 0, ppm));
}

/**
 * @ingroup HosSpectrum
 * @brief   Return a spectrum of no more than @ndim dimensions,
 *          obtained by projecting the highest dimension of @self
 *          until only @ndim remain.
 */
HosSpectrum*
spectrum_cap_ndim (HosSpectrum* self, const guint ndim)
{
  guint self_ndim = spectrum_ndim(self);

  return self_ndim > ndim ?
    spectrum_cap_ndim (spectrum_project
		       (spectrum_transpose (self, self_ndim - 1), 0), ndim) :
    self;
}
