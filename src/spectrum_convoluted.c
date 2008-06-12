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

#include "spectrum_convoluted.h"
#include "spectrum_priv.h"
#include "utils.h"

#define SPECTRUM_CONVOLUTED_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SPECTRUM_CONVOLUTED, HosSpectrumConvolutedPrivate))
#define SPECTRUM_CONVOLUTED_PRIVATE(o, field) ((SPECTRUM_CONVOLUTED_GET_PRIVATE(o))->field)
typedef struct _HosSpectrumConvolutedPrivate HosSpectrumConvolutedPrivate;

struct _HosSpectrumConvolutedPrivate
{
  HosSpectrum *A;
  HosSpectrum *B;
  guint A_ndim;
};

struct convoluted_iterator
{
  struct spectrum_iterator parent;

  struct spectrum_iterator     *A;
  struct spectrum_iterator     *B;
  HosSpectrumConvolutedPrivate *priv;
};

static void     spectrum_convoluted_increment (struct spectrum_iterator *self, guint dim, gint delta);
static gboolean spectrum_convoluted_tickle    (struct spectrum_iterator *self, gdouble *dest);
static gdouble  spectrum_convoluted_wait      (struct spectrum_iterator *self);
static void     spectrum_convoluted_mark      (struct spectrum_iterator *self);
static void     spectrum_convoluted_restore   (struct spectrum_iterator *self);
static gboolean spectrum_convoluted_probe     (struct spectrum_iterator *self);

static struct spectrum_iterator* spectrum_convoluted_construct_iterator (HosSpectrum *self);
static void                      spectrum_convoluted_free_iterator      (struct spectrum_iterator *self);

static void   spectrum_convoluted_dispose  (GObject *object);
static void   spectrum_convoluted_finalize (GObject *object);

G_DEFINE_TYPE (HosSpectrumConvoluted, hos_spectrum_convoluted, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_convoluted_class_init(HosSpectrumConvolutedClass *klass)
{
  GObjectClass     *gobject_class  = G_OBJECT_CLASS(klass);
  HosSpectrumClass *spectrum_class = HOS_SPECTRUM_CLASS(klass);

  gobject_class->dispose     = spectrum_convoluted_dispose;
  gobject_class->finalize    = spectrum_convoluted_finalize;

  spectrum_class->construct_iterator = spectrum_convoluted_construct_iterator;
  spectrum_class->free_iterator      = spectrum_convoluted_free_iterator;

  g_type_class_add_private(gobject_class, sizeof(HosSpectrumConvolutedPrivate));
}

static void
hos_spectrum_convoluted_init(HosSpectrumConvoluted *self)
{
}

static void
spectrum_convoluted_dispose(GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_CONVOLUTED_PRIVATE(object, A));
  G_OBJECT_UNREF_AND_CLEAR(SPECTRUM_CONVOLUTED_PRIVATE(object, B));
  G_OBJECT_CLASS(hos_spectrum_convoluted_parent_class)->dispose (object);
}

static void
spectrum_convoluted_finalize(GObject *object)
{
}

/*
 * Returns: the convolution of spectrum A and spectrum B.
 *
 * Definition:  let C <- spectrum_convolute(A, B);
 *   then  C(a', b', a, b) = A(a', b') * B(a, b)
 *
 */
HosSpectrum*
spectrum_convolute (HosSpectrum *A, HosSpectrum *B)
{
  HosSpectrum *result = g_object_new(HOS_TYPE_SPECTRUM_CONVOLUTED, NULL);

  GList* dimensions   = g_list_concat(spectrum_copy_dimensions(A),
				      spectrum_copy_dimensions(B));
  spectrum_set_dimensions(result, dimensions);

  SPECTRUM_CONVOLUTED_PRIVATE(result, A) = A;
  SPECTRUM_CONVOLUTED_PRIVATE(result, B) = B;
  SPECTRUM_CONVOLUTED_PRIVATE(result, A_ndim) = spectrum_ndim(A);

  g_object_ref(A);
  g_object_ref(B);

  return result;
}

static void
spectrum_convoluted_increment(struct spectrum_iterator *self, guint dim, gint delta)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;

  if (dim < convoluted_iterator->priv->A_ndim)
    iterator_increment(convoluted_iterator->A, dim, delta);
  else
    iterator_increment(convoluted_iterator->B, dim - convoluted_iterator->priv->A_ndim, delta);
}

static gboolean
spectrum_convoluted_probe(struct spectrum_iterator *self)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;
  return iterator_probe(convoluted_iterator->A) && iterator_probe(convoluted_iterator->B);
}

static gboolean
spectrum_convoluted_tickle(struct spectrum_iterator *self, gdouble *dest)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;

  gdouble A, B;

  gboolean result_A = iterator_tickle(convoluted_iterator->A, &A);
  gboolean result_B = iterator_tickle(convoluted_iterator->B, &B);

  if (result_A && result_B)
    {
      *dest = A * B;
      return TRUE;
    }
  else
    return FALSE;

}

static gdouble
spectrum_convoluted_wait(struct spectrum_iterator *self)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;
  gdouble result = iterator_wait(convoluted_iterator->A) * iterator_wait(convoluted_iterator->B);

  /* FIXME verify indices */
  gint i;
  guint A_ndim = convoluted_iterator->priv->A_ndim;
  for (i = 0; i < self->ndim; ++i)
    {
      if (i < A_ndim)
	g_assert(self->idx[i] == convoluted_iterator->A->idx[i]);
      else
	g_assert(self->idx[i] == convoluted_iterator->B->idx[i - A_ndim]);
    }

  return result;
}

static void
spectrum_convoluted_mark(struct spectrum_iterator *self)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;

  iterator_mark(convoluted_iterator->A);
  iterator_mark(convoluted_iterator->B);
}

static void
spectrum_convoluted_restore(struct spectrum_iterator *self)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;

  iterator_restore(convoluted_iterator->A);
  iterator_restore(convoluted_iterator->B);
}

static struct spectrum_iterator*
spectrum_convoluted_construct_iterator(HosSpectrum *self)
{
  struct convoluted_iterator *result           = g_new0(struct convoluted_iterator, 1);
  struct spectrum_iterator   *spectrum_iterator = (struct spectrum_iterator*)result;

  result->priv = SPECTRUM_CONVOLUTED_GET_PRIVATE(self);
  result->A    = spectrum_construct_iterator(result->priv->A);
  result->B    = spectrum_construct_iterator(result->priv->B);

  spectrum_iterator->tickle     = spectrum_convoluted_tickle;
  spectrum_iterator->wait       = spectrum_convoluted_wait;
  spectrum_iterator->increment  = spectrum_convoluted_increment;
  spectrum_iterator->mark       = spectrum_convoluted_mark;
  spectrum_iterator->restore    = spectrum_convoluted_restore;
  spectrum_iterator->probe      = spectrum_convoluted_probe;

  return spectrum_iterator;
}

static void
spectrum_convoluted_free_iterator(struct spectrum_iterator *self)
{
  struct convoluted_iterator *convoluted_iterator = (struct convoluted_iterator*)self;
  iterator_free(convoluted_iterator->A);
  iterator_free(convoluted_iterator->B);
  g_free(self);
}



