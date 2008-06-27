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

#include <math.h>
#include "model-subtypes.h"
#include "model-subtypes-gen.c"

/*** model_gaussian ***/

static void
model_gaussian_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  model_iterator_t *arg = (model_iterator_t*)(self->data);
  arg->fill(arg, dest);
  int i;
  for (i = 0; i < self->np; ++i)
    dest[i] = exp(- (dest[i] * dest[i]));
}

static void
model_gaussian_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  model_iterator_t *arg = model_iterator_new(HOS_MODEL_GAUSSIAN(self->root)->argument, orig, delta, np);
  self->data = arg;
  self->np = arg->np;
}

static void
model_gaussian_iterator_free(model_iterator_t* self)
{
  model_iterator_free((model_iterator_t*)(self->data));
}

HosModel*
model_gaussian(HosModel *src)
{
  HosModel *result = g_object_new(HOS_TYPE_MODEL_GAUSSIAN, NULL);
  HOS_MODEL_GAUSSIAN(result)->argument = src;
  result->ndim = src->ndim;
  return result;
}

/** model_dimension **/

struct dimension_data
{
  gdouble orig;
  gdouble delta;
};

static void
model_dimension_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  int i;
  struct dimension_data *data = (struct dimension_data*)(self->data);
  gdouble y = data->orig;
  for (i = 0; i < self->np; ++i)
    {
      dest[i] = y;
      y += data->delta;
    }
}

static void
model_dimension_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  struct dimension_data *data = g_new0(struct dimension_data, 1);
  data->orig  = orig[0];
  data->delta = -delta[0];
  self->np    = np[0];
  self->data  = data;
}

static void model_dimension_iterator_free(model_iterator_t *self) { }

/** model_sum **/

struct pair_data
{
  model_iterator_t *iterator[2];
  gdouble          *buffer[2];
};

static void
model_sum_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  struct pair_data *args = (struct pair_data*)(self->data);

  model_iterator_fill(args->iterator[0], args->buffer[0]);
  model_iterator_fill(args->iterator[1], args->buffer[1]);

  int i;
  int np_0 = (args->iterator[0])->np;
  int np_1 = (args->iterator[1])->np;

  for (i = 0; i < self->np; ++i)
    dest[i] = (args->buffer[0])[i % np_0] + (args->buffer[1])[i % np_1];
}

static void
model_sum_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  struct pair_data *data = g_new0(struct pair_data, 1);
  self->data = data;
  HosModelSum *model_sum = HOS_MODEL_SUM(self->root);

  data->iterator[0] = model_iterator_new(model_sum->A, orig, delta, np);
  data->iterator[1] = model_iterator_new(model_sum->B, orig, delta, np);

  gint np_0 = (data->iterator[0])->np;
  gint np_1 = (data->iterator[0])->np;

  g_assert(np_0 > 0);
  g_assert(np_1 > 0);

  data->buffer[0] = g_new(gdouble, np_0);
  data->buffer[1] = g_new(gdouble, np_1);

  self->np = MAX(np_0, np_1);
}

static void
model_sum_iterator_free(model_iterator_t *self)
{
  /* FIXME free argument iterators */
}

HosModel*
model_sum(HosModel *A, HosModel *B)
{
  HosModel    *result    = g_object_new(HOS_TYPE_MODEL_SUM, NULL);
  HosModelSum *model_sum = HOS_MODEL_SUM(result);
  model_sum->A = A;
  model_sum->B = B;

  result->ndim = MAX(A->ndim, B->ndim);

  return result;
}

/** model_product **/

static void
model_product_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  struct pair_data *args = (struct pair_data*)(self->data);

  model_iterator_fill(args->iterator[0], args->buffer[0]);
  model_iterator_fill(args->iterator[1], args->buffer[1]);

  int i;
  int np_0 = (args->iterator[0])->np;
  int np_1 = (args->iterator[1])->np;

  for (i = 0; i < self->np; ++i)
    dest[i] = (args->buffer[0])[i % np_0] * (args->buffer[1])[i / np_0];
}

static void
model_product_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  struct pair_data *data = g_new0(struct pair_data, 0);
  self->data = data;
  HosModelProduct *model_product = HOS_MODEL_PRODUCT(self->root);

  int nA = model_product->A->ndim;

  data->iterator[0] = model_iterator_new(model_product->A, orig, delta, np);
  data->iterator[1] = model_iterator_new(model_product->B, orig + nA, delta + nA, np + nA);

  gint np_0 = (data->iterator[0])->np;
  gint np_1 = (data->iterator[0])->np;

  g_assert(np_0 > 0);
  g_assert(np_1 > 0);

  data->buffer[0] = g_new(gdouble, np_0);
  data->buffer[1] = g_new(gdouble, np_1);

  self->np = np_0 * np_1;
}

static void
model_product_iterator_free(model_iterator_t *self)
{
  /* FIXME free argument iterators */
}

HosModel*
model_product(HosModel *A, HosModel *B)
{
  HosModel        *result    = g_object_new(HOS_TYPE_MODEL_PRODUCT, NULL);
  HosModelProduct *model_product = HOS_MODEL_PRODUCT(result);
  model_product->A = A;
  model_product->B = B;
  
  result->ndim = A->ndim + B->ndim;

  return result;
}
