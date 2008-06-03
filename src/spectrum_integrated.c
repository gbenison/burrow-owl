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

#include "spectrum_integrated.h"
#include "spectrum_priv.h"
#include "utils.h"

#define SPECTRUM_INTEGRATED_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedPrivate))
#define SPECTRUM_INTEGRATED_PRIVATE(o, field) ((SPECTRUM_INTEGRATED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumIntegratedPrivate HosSpectrumIntegratedPrivate;

struct _HosSpectrumIntegratedPrivate
{
  HosSpectrum *integrand;
};

struct integrated_iterator
{
  struct spectrum_iterator      parent;
  HosSpectrumIntegratedPrivate *priv;
  struct spectrum_iterator     *integrand;
};

static void     spectrum_integrated_increment (struct spectrum_iterator *self, guint dim, gint delta);
static gboolean spectrum_integrated_tickle    (struct spectrum_iterator *self, gdouble *dest);
static gdouble  spectrum_integrated_wait      (struct spectrum_iterator *self);
static void     spectrum_integrated_mark      (struct spectrum_iterator *self);
static void     spectrum_integrated_restore   (struct spectrum_iterator *self);
static void     spectrum_integrated_sync      (struct spectrum_iterator *self);

/* FIXME needs a 'probe' implementation */

static struct spectrum_iterator* spectrum_integrated_construct_iterator (HosSpectrum *self);
static void                      spectrum_integrated_free_iterator      (struct spectrum_iterator *self);

static void   spectrum_integrated_dispose  (GObject *object);
static void   spectrum_integrated_finalize (GObject *object);

G_DEFINE_TYPE (HosSpectrumIntegrated, hos_spectrum_integrated, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_integrated_class_init(HosSpectrumIntegratedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_integrated_dispose;
  gobject_class->finalize    = spectrum_integrated_finalize;

  spectrum_class->construct_iterator = spectrum_integrated_construct_iterator;
  spectrum_class->free_iterator      = spectrum_integrated_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumIntegratedPrivate));
}

static void
hos_spectrum_integrated_init(HosSpectrumIntegrated* self)
{
  /* FIXME */
  /* anything? */
}

static void
spectrum_integrated_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_INTEGRATED_PRIVATE(object, integrand));
  G_OBJECT_CLASS(hos_spectrum_integrated_parent_class)->dispose (object);
}

static void
spectrum_integrated_finalize(GObject *object)
{
  HosSpectrumIntegrated *spectrum_integrated = HOS_SPECTRUM_INTEGRATED(object);
  HosSpectrumIntegratedPrivate *priv  = SPECTRUM_INTEGRATED_GET_PRIVATE(object);

  G_OBJECT_CLASS(hos_spectrum_integrated_parent_class)->finalize (object);
}

/*
 * Returns:
 *  S' where S'(j, k) = Sum_i(self(i, j, k))
 */
HosSpectrum*
spectrum_integrate (HosSpectrum* self)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_INTEGRATED, NULL);
  GList* dimensions   = spectrum_copy_dimensions(self);

  g_free(g_list_nth_data(dimensions, 0));
  dimensions = g_list_delete_link(dimensions, dimensions);
  spectrum_set_dimensions(result, dimensions);

  guint integrand_ndim = spectrum_ndim(self);

  g_object_ref(self);

  /* set up accumulation buffer */
  HosSpectrumIntegratedPrivate *priv = SPECTRUM_INTEGRATED_GET_PRIVATE(result);
  priv->integrand      = self;

  return result;
}

static void
spectrum_integrated_increment(struct spectrum_iterator *self, guint dim, gint delta)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
  iterator_increment(integrated_iterator->integrand, dim + 1, delta);
}

static gboolean
spectrum_integrated_tickle(struct spectrum_iterator *self, gdouble *dest)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;

  iterator_mark(integrated_iterator->integrand);

  gdouble sum = 0;
  gboolean result = TRUE;
  gint i;
  for (i = 0; i < integrated_iterator->integrand->np[0]; ++i)
    {
      g_assert(integrated_iterator->integrand->idx[0] == i);

      gdouble value = 0;
      gboolean hit = iterator_tickle(integrated_iterator->integrand, &value);
      if (hit == TRUE)
	sum += value;
      else
	result = FALSE;

      iterator_increment(integrated_iterator->integrand, 0, 1);
    }

  iterator_restore(integrated_iterator->integrand);

  if (result == TRUE)
    *dest = sum;
  return result;
}

static gdouble
spectrum_integrated_wait(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;

  spectrum_integrated_sync(self);

  iterator_mark(integrated_iterator->integrand);

  gdouble sum = 0;
  gint i;
  for (i = 0; i < integrated_iterator->integrand->np[0]; ++i)
    {
      g_assert(integrated_iterator->integrand->idx[0] == i);
      gdouble delta = iterator_wait(integrated_iterator->integrand);
      sum += delta;
      iterator_increment(integrated_iterator->integrand, 0, 1);
    }

  iterator_restore(integrated_iterator->integrand);

  return sum;
}

static void
spectrum_integrated_sync(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;

  /* synchronize the iterators */
  gint d;
  for (d = 0; d < self->ndim; ++d)
    {
      gint delta = (self->idx[d] - integrated_iterator->integrand->idx[d + 1]);
      if (delta != 0)
	iterator_increment(integrated_iterator->integrand, d + 1, delta);
    }
  if (integrated_iterator->integrand->idx[0] != 0)
    iterator_increment(integrated_iterator->integrand, 0, -integrated_iterator->integrand->idx[0]);
}

static void
spectrum_integrated_restore(struct spectrum_iterator *self)
{
  spectrum_integrated_sync(self);
}

static struct spectrum_iterator*
spectrum_integrated_construct_iterator(HosSpectrum *self)
{
  struct integrated_iterator *result           = g_new0(struct integrated_iterator, 1);
  struct spectrum_iterator   *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv      = SPECTRUM_INTEGRATED_GET_PRIVATE(self);
  result->integrand = spectrum_construct_iterator(result->priv->integrand);

  spectrum_iterator->tickle     = spectrum_integrated_tickle;
  spectrum_iterator->wait       = spectrum_integrated_wait;
  spectrum_iterator->increment  = spectrum_integrated_increment;
  spectrum_iterator->restore    = spectrum_integrated_restore;

  return spectrum_iterator;
}

static void
spectrum_integrated_free_iterator(struct spectrum_iterator *self)
{
  struct integrated_iterator *integrated_iterator = (struct integrated_iterator*)self;
}


