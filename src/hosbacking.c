/*
 *  Copyright (C) 2005 Greg Benison
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

/*
 * this is a compatibility macro; it must come before any header
 * includes.  It defines which features of the C library will be
 * available.  In this file it is needed for proper round() behavior
 * (at least).
 */
#define _GNU_SOURCE

#include <assert.h>
#include <math.h>
#include "hosbacking.h"


static GObjectClass *parent_class = NULL;

static void hos_backing_class_init (HosBackingClass *klass);
static void hos_backing_init(HosBacking*);

GType
hos_backing_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosBackingClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_backing_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosBacking),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_backing_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "HosBacking",
				     &info,
				     G_TYPE_FLAG_ABSTRACT);
    }

  return type;
}

static void
hos_backing_class_init (HosBackingClass *klass)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  /*
  gobject_class->set_property = hos_spectrum_set_property;
  gobject_class->get_property = hos_spectrum_get_property;
  */

/*

  PROPERTIES GO HERE
  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}

static void
hos_backing_init(HosBacking  *self)
{
  /* FIXME */
}

void
backing_reset(HosBacking* self, gpointer data)
{
  if (HOS_BACKING_GET_CLASS(self)->reset)
    HOS_BACKING_GET_CLASS(self)->reset(self);

  self->negated = FALSE;
}

HosBacking*
backing_copy(HosBacking *self)
{
  HosBacking *result = g_object_new(G_TYPE_FROM_INSTANCE(self), NULL);

  if (HOS_BACKING_GET_CLASS(self)->copy != NULL)
    HOS_BACKING_GET_CLASS(self)->copy(self, result);

  return result;
}

/*
 * Use as a callback for GList's of backing objects.
 */
void
backing_accumulate(HosBacking* backing, gdouble *accum)
{
  *accum *= HOS_BACKING_GET_CLASS(backing)->peek(backing);
  if (backing->negated)
    *accum = -*accum;
}

void
backing_negate(HosBacking* self)
{
  self->negated = self->negated ? FALSE : TRUE;
}

void
backing_lock(HosBacking* self, gulong lock_id)
{
  if (self->lock_id == lock_id)
    return;

  if (!self->lock)
    self->lock = g_mutex_new();

  g_mutex_lock(self->lock);
  {
     HosBackingClass *class = HOS_BACKING_GET_CLASS(self);

     if (class->lock_method)
       class->lock_method(self, lock_id);
  }

}

void
backing_unlock(HosBacking* self)
{
  self->lock_id = 0;

  if (self->lock)
    {
      HosBackingClass *class = HOS_BACKING_GET_CLASS(self);

      g_mutex_unlock(self->lock);

      if (class->unlock_method)
	class->unlock_method(self);
    }

}

static GMutex* global_lock = NULL;

void
backing_global_lock()
{
  if (!global_lock)
    global_lock = g_mutex_new();

  g_mutex_lock(global_lock);
}

void
backing_global_unlock()
{
  if (global_lock)
    g_mutex_unlock(global_lock);
}


