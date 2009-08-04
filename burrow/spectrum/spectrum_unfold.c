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
#include "spectrum.h"
#include "spectrum_priv.h"
#include "spectrum_unfold.h"
#include "utils.h"


#define SPECTRUM_UNFOLDED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_UNFOLDED, HosSpectrumUnfoldedPrivate))
#define SPECTRUM_UNFOLDED_PRIVATE(o, field) ((SPECTRUM_UNFOLDED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumUnfoldedPrivate HosSpectrumUnfoldedPrivate;

struct _HosSpectrumUnfoldedPrivate
{
  HosSpectrum *base;
  
  guint    base_ndim;
  guint    base_idx;
  guint    base_np;

  gboolean negate_on_fold;
  gboolean negate_first;
};

struct unfolded_iterator
{
  struct spectrum_iterator parent;

  struct spectrum_iterator    *base;
  HosSpectrumUnfoldedPrivate  *priv;

  gboolean negated;
};

static gboolean spectrum_unfolded_tickle     (struct spectrum_iterator* self, gdouble* dest);
static void     spectrum_unfolded_mark       (struct spectrum_iterator* self);
static void     spectrum_unfolded_restore    (struct spectrum_iterator* self);
static gdouble  spectrum_unfolded_wait       (struct spectrum_iterator* self);
static gboolean spectrum_unfolded_probe      (struct spectrum_iterator* self);
static void     spectrum_unfolded_increment  (struct spectrum_iterator* self, guint dim, gint delta);

static struct spectrum_iterator* spectrum_unfolded_construct_iterator (HosSpectrum *self);
static void                      spectrum_unfolded_free_iterator      (struct spectrum_iterator* self);

static void   spectrum_unfolded_dispose  (GObject *object);
static void   spectrum_unfolded_finalize (GObject *object);

G_DEFINE_TYPE (HosSpectrumUnfolded, hos_spectrum_unfolded, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_unfolded_class_init(HosSpectrumUnfoldedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_unfolded_dispose;
  gobject_class->finalize    = spectrum_unfolded_finalize;

  spectrum_class->construct_iterator = spectrum_unfolded_construct_iterator;
  spectrum_class->free_iterator      = spectrum_unfolded_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumUnfoldedPrivate));
}

static void
hos_spectrum_unfolded_init(HosSpectrumUnfolded* self)
{
  /* FIXME */
  /* anything? */
}

static void
spectrum_unfolded_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_UNFOLDED_PRIVATE(object, base));
  G_OBJECT_CLASS(hos_spectrum_unfolded_parent_class)->dispose (object);
}

static void
spectrum_unfolded_finalize(GObject *object)
{
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(object);
  G_OBJECT_CLASS(hos_spectrum_unfolded_parent_class)->finalize (object);
}

/*
 * Unfold dimension 'idx' of 'self' by appending identical copies
 * of 'self', possibly negating every other one.
 * e.g.
 * spectrum_np(S', idx) = spectrum_np(S, idx) * (1 + downfield + upfield)
 */
HosSpectrum*
spectrum_unfold(HosSpectrum* self,
		const guint idx,
		const guint downfield,
		const guint upfield,
		const gboolean negate_on_fold)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_UNFOLDED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);
  HosSpectrumUnfoldedPrivate *priv = SPECTRUM_UNFOLDED_GET_PRIVATE(result);

  priv->base      = self;
  priv->base_ndim = spectrum_ndim(self);
  g_object_ref(self);

  priv->base_idx  = idx;
  priv->base_np   = spectrum_np(self, idx);

  priv->negate_on_fold = negate_on_fold;
  priv->negate_first = ((downfield % 2) == 1);
  
  dimension_t* unfolded_dimension = g_list_nth_data(dimensions, idx);
  unfolded_dimension->np   *= (1 + downfield + upfield);
  unfolded_dimension->sw   *= (1 + downfield + upfield);
  unfolded_dimension->orig += downfield * spectrum_sw(self, idx);

  spectrum_set_dimensions(result, dimensions);

  return result;
}

static gboolean
spectrum_unfolded_probe(struct spectrum_iterator *self)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;
  return iterator_probe(unfolded_iterator->base);
}

static gboolean
spectrum_unfolded_tickle(struct spectrum_iterator* self, gdouble* dest)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;

  gboolean result = iterator_tickle(unfolded_iterator->base, dest);

  if (result && unfolded_iterator->negated)
    *dest = -*dest;

  return result;
}

static void
spectrum_unfolded_mark(struct spectrum_iterator* self)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;
  iterator_mark(unfolded_iterator->base);
}

static void
spectrum_unfolded_restore(struct spectrum_iterator* self)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;
  iterator_restore(unfolded_iterator->base);
}

static gdouble
spectrum_unfolded_wait(struct spectrum_iterator* self)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;
  gdouble result = iterator_wait(unfolded_iterator->base);
  if (unfolded_iterator->negated)
    result = -result;
  return result;
}

static void
spectrum_unfolded_increment(struct spectrum_iterator* self, guint dim, gint delta)
{
  struct unfolded_iterator   *unfolded_iterator = (struct unfolded_iterator*)self;
  HosSpectrumUnfoldedPrivate *priv = unfolded_iterator->priv;
  struct spectrum_iterator   *base = unfolded_iterator->base;

  if (dim != priv->base_idx)
    iterator_increment(base, dim, delta);
  else
    {
      /* note: self->idx has already been updated */
      gboolean flip = FALSE;

      gint old_idx = base->idx[priv->base_idx];
      while ((old_idx + delta) < 0)
	{
	  delta += priv->base_np;
	  flip   = !flip;
	}

      while ((old_idx + delta) >= priv->base_np)
	{
	  delta -= priv->base_np;
	  flip   = !flip;
	}

      iterator_increment(base, dim, delta);

      if (flip && priv->negate_on_fold)
	unfolded_iterator->negated = !unfolded_iterator->negated;
    }
}

static struct spectrum_iterator*
spectrum_unfolded_construct_iterator(HosSpectrum *self)
{
  struct unfolded_iterator *result            = g_new0(struct unfolded_iterator, 1);
  struct spectrum_iterator *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv = SPECTRUM_UNFOLDED_GET_PRIVATE(self);
  result->base = spectrum_construct_iterator(result->priv->base);

  spectrum_iterator->tickle     = spectrum_unfolded_tickle;
  spectrum_iterator->wait       = spectrum_unfolded_wait;
  spectrum_iterator->increment  = spectrum_unfolded_increment;
  spectrum_iterator->mark       = spectrum_unfolded_mark;
  spectrum_iterator->restore    = spectrum_unfolded_restore;
  spectrum_iterator->probe      = spectrum_unfolded_probe;

  return spectrum_iterator;
}

static void
spectrum_unfolded_free_iterator(struct spectrum_iterator* self)
{
  struct unfolded_iterator *unfolded_iterator = (struct unfolded_iterator*)self;
  iterator_free(unfolded_iterator->base);
  g_free(self);
}


