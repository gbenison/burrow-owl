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
#include <gsl/gsl_randist.h>
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
  struct pair_data *args = (struct pair_data*)(self->data);
  model_iterator_free(args->iterator[0]);
  model_iterator_free(args->iterator[1]);

  g_free(args->buffer[0]);
  g_free(args->buffer[1]);
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
  struct pair_data *data = g_new0(struct pair_data, 1);
  self->data = data;
  HosModelProduct *model_product = HOS_MODEL_PRODUCT(self->root);

  int nA = model_product->A->ndim;

  data->iterator[0] = model_iterator_new(model_product->A, orig, delta, np);
  data->iterator[1] = model_iterator_new(model_product->B, orig + nA, delta + nA, np + nA);

  gint np_0 = (data->iterator[0])->np;
  gint np_1 = (data->iterator[1])->np;

  g_assert(np_0 > 0);
  g_assert(np_1 > 0);

  data->buffer[0] = g_new(gdouble, np_0);
  data->buffer[1] = g_new(gdouble, np_1);

  self->np = np_0 * np_1;
}

static void
model_product_iterator_free(model_iterator_t *self)
{
  struct pair_data *args = (struct pair_data*)(self->data);
  model_iterator_free(args->iterator[0]);
  model_iterator_free(args->iterator[1]);

  g_free(args->buffer[0]);
  g_free(args->buffer[1]);
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

/** model_noise **/

static void
model_noise_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  static gsl_rng *rng = NULL;
  if (rng == NULL)
    {
      gsl_rng_env_setup();
      rng = gsl_rng_alloc(gsl_rng_default);
    }

  HosModelNoise *model_noise = HOS_MODEL_NOISE(self->root);
  model_iterator_t *arg = (model_iterator_t*)(self->data);
  arg->fill(arg, dest);
  int i;
  for (i = 0; i < self->np; ++i)
    dest[i] += gsl_ran_gaussian(rng, model_noise->noise);
}

static void
model_noise_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  model_iterator_t *arg = model_iterator_new(HOS_MODEL_NOISE(self->root)->argument, orig, delta, np);
  self->data = arg;
  self->np   = arg->np;
}

static void model_noise_iterator_free(model_iterator_t *self) {  /* no-op */ }

HosModel*
model_add_noise(HosModel *src, gdouble noise)
{
  HosModel      *result      = g_object_new(HOS_TYPE_MODEL_NOISE, NULL);
  HosModelNoise *model_noise = HOS_MODEL_NOISE(result);
  
  model_noise->argument = src;
  model_noise->noise = noise;
  result->ndim = src->ndim;

  return result;
}

/** model_projection **/

static void
model_projection_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  model_iterator_t *arg = (model_iterator_t*)(self->data);
  model_iterator_fill(arg, dest);
}

static void
model_projection_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{
  HosModelProjection *model_projection = HOS_MODEL_PROJECTION(self->root);
  gint arg_ndim = model_projection->argument->ndim;

  gdouble arg_orig[arg_ndim];
  gdouble arg_delta[arg_ndim];
  guint arg_np[arg_ndim];

  arg_orig[0] = model_projection->coordinate;
  arg_delta[0] = 0;
  arg_np[0] = 1;

  int i;
  for (i = 1; i < arg_ndim; ++i)
    {
      arg_orig[i]  = orig[i - 1];
      arg_delta[i] = delta[i - 1];
      arg_np[i]    = np[i - 1];
    }

  model_iterator_t *arg_iterator = model_iterator_new(model_projection->argument,
						      arg_orig,
						      arg_delta,
						      arg_np);
  self->data = arg_iterator;
  self->np = arg_iterator->np;
}

static void
model_projection_iterator_free(model_iterator_t *self)
{
  model_iterator_free((model_iterator_t*)(self->data));
}

HosModel*
model_project(HosModel *src, gdouble coordinate)
{
  HosModel           *result           = g_object_new(HOS_TYPE_MODEL_PROJECTION, NULL);
  HosModelProjection *model_projection = HOS_MODEL_PROJECTION(result);

  g_return_if_fail(src->ndim > 0);

  model_projection->argument   = src;
  model_projection->coordinate = coordinate;

  result->ndim = src->ndim - 1;

  return result;
}

/*** model_transposition ***/

static void
model_transposition_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  model_iterator_t *arg = (model_iterator_t*)(self->data);
  model_iterator_fill(arg, dest);

  HosModelTransposition *model_transposition = HOS_MODEL_TRANSPOSITION(self->root);

  gdouble tmp;

  guint n_i = model_transposition->n_i;
  guint n_j = model_transposition->n_j;
  guint n_k = model_transposition->n_k;

  guint i, j, k; 
  gdouble *orig = dest;

  for (k = 0; k < n_k; ++k)
    {
      for (i = 0; i < n_i; ++i)
	for (j = 0; j < n_j; ++j)
	  {
	    gdouble A = *(orig + (i + n_i * j));
	    gdouble B = *(orig + (j + i * n_j));
	    tmp = A;
	    A = B;
	    B = tmp;
	  }
      orig += n_i * n_j;
    }
}

static void
model_transposition_iterator_init(model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np)
{

  HosModelTransposition *model_transposition = HOS_MODEL_TRANSPOSITION(self->root);
  gint ndim = HOS_MODEL(self->root)->ndim;
  guint idx = model_transposition->idx;

  gdouble arg_orig[ndim];
  gdouble arg_delta[ndim];
  guint   arg_np[ndim];

  arg_orig[0]  = orig[idx];
  arg_delta[0] = delta[idx];
  arg_np[0]    = np[idx];

  int i;
  for (i = 1; i < ndim; ++i)
    {
      guint src_idx = (i > idx) ? i : i - 1;
      arg_orig[i]  = orig[src_idx];
      arg_delta[i] = delta[src_idx];
      arg_np[i]    = np[src_idx];
    }

  model_iterator_t *arg_iterator = model_iterator_new(model_transposition->argument,
						      arg_orig,
						      arg_delta,
						      arg_np);
  self->data = arg_iterator;
  self->np = arg_iterator->np;

  /* set up dimensions for transpose */
  model_transposition->n_i = 1;
  model_transposition->n_j = 1;
  model_transposition->n_k = 1;

  for (i = 0; i < idx; ++i)
    model_transposition->n_i *= arg_np[i];

  model_transposition->n_j = arg_np[idx];

  for (i = idx + 1; i < ndim; ++i)
    model_transposition->n_k *= arg_np[i];

}

static void
model_transposition_iterator_free(model_iterator_t *self)
{
  model_iterator_free((model_iterator_t*)(self->data));
}

HosModel*
model_transpose(HosModel *src, guint idx)
{
  HosModel           *result           = g_object_new(HOS_TYPE_MODEL_TRANSPOSITION, NULL);
  HosModelTransposition *model_transposition = HOS_MODEL_TRANSPOSITION(result);

  g_return_if_fail(HOS_IS_MODEL(src));

  if (idx == 0)
    return src;

  g_return_if_fail(src->ndim > 0);

  model_transposition->argument = src;
  model_transposition->idx      = idx;

  result->ndim = src->ndim;

  return result;
}

