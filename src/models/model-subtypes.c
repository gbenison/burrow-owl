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
    *dest = exp(- (*dest * *dest));
}

static void
model_gaussian_iterator_init(model_iterator_t* self)
{
  self->data =
    model_iterator_new(HOS_MODEL_GAUSSIAN(self->root)->argument, self->orig, self->delta, self->np);
}
