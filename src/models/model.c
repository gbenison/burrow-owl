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

#include "model.h"

G_DEFINE_ABSTRACT_TYPE (HosModel, hos_model, G_TYPE_OBJECT)

static void
hos_model_class_init(HosModelClass *klass)
{
  GObjectClass *gobject_class;
}

static void
hos_model_init(HosModel *self)
{
}

model_iterator_t* 
model_construct_iterator (HosModel *self, gdouble *orig, gdouble *sw, guint *np)
{
}

void
model_iterator_eval(model_iterator_t *self, gdouble *dest)
{
}

void
model_iterator_free(model_iterator_t *self)
{
  g_assert(self != NULL);
  g_assert(self->root != NULL);

  HosModelClass *class = HOS_MODEL_GET_CLASS(self->root);

  if (class->iterator_free)
    class->iterator_free(self);
}



