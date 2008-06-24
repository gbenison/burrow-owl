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

#include "model-subtypes.h"

#include "model-subtypes-gen.c"

static void
model_gaussian_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  model_iterator_t *arg = (model_iterator_t*)(self->data);
  arg->fill(arg, dest);
  int i;
  for (i = 0; i < self->np[0]; ++i)
    dest[i] = exp(- (dest[i] * dest[i]));
}

static void
model_gaussian_iterator_init(model_iterator_t* self)
{
  self->data =
    model_iterator_new(HOS_MODEL_GAUSSIAN(self->root)->argument, self->orig, self->delta, self->np);
}

static void
model_gaussian_iterator_free(model_iterator_t* self)
{
  model_iterator_free((model_iterator_t*)(self->data));
}


static void
model_dimension_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  int i;
  gdouble y = self->orig[0];
  for (i = 0; i < self->np[0]; ++i)
    {
      dest[i] = y;
      y += self->delta[0];
    }
}

static void model_dimension_iterator_init(model_iterator_t* self) { }
static void model_dimension_iterator_free(model_iterator_t* self) { }

/** model_sum **/

struct argument_pair
{
  model_iterator_t *iterator[2];
  gdouble          *buffer[2];
};

static void
model_sum_iterator_fill(model_iterator_t* self, gdouble *dest)
{
  struct argument_pair *args = (struct argument_pair*)(self->data);

  model_iterator_fill(args->iterator[0], args->buffer[0]);
  model_iterator_fill(args->iterator[1], args->buffer[1]);

  int i;
  int np_0 = (args->iterator[0])->np[0];
  int np_1 = (args->iterator[1])->np[0];

  for (i = 0; i < self->np[0]; ++i)
    dest[i] = (args->buffer[0])[i % np_0] + (args->buffer[1])[i % np_1];
}

static void model_sum_iterator_init(model_iterator_t* self) { }
static void model_sum_iterator_free(model_iterator_t* self) { }

