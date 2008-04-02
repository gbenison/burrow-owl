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
#include "hosdimension.h"


static GObjectClass *parent_class = NULL;

static void hos_dimension_class_init (HosDimensionClass *klass);
static void hos_dimension_init(HosDimension*);
static void hos_dimension_set_property (GObject         *object,
				       guint            prop_id,
				       const GValue    *value,
				       GParamSpec      *pspec);
static void hos_dimension_get_property (GObject         *object,
				       guint            prop_id,
				       GValue          *value,
				       GParamSpec      *pspec);


static HosDimension* dimen_copy(HosDimension* self);
/* static void dimension_clip_lower(HosDimension* self, const guint pt); */
static void dimension_clip_upper(HosDimension* self, const guint pt);


/*
 * Performs a deep copy of a list of dimensions;
 * both the list itself and its contents are copied.
 */
GList*
dimen_list_copy(GList *dimens)
{
  GList *result = g_list_copy(dimens);
  GList *list_iter;
  
  for (list_iter = result; list_iter != NULL; list_iter = list_iter->next)
    list_iter->data = dimen_copy((HosDimension*)(list_iter->data));

  return result;
}

guint
dimension_np(HosDimension* self)
{
  return self->np;
}

gdouble
dimension_orig(HosDimension* self)
{
  return self->orig;
}

gdouble
dimension_sf(HosDimension* self)
{
  return self->sf;
}

gdouble
dimension_sw(HosDimension* self)
{
  return self->sw;
}

void
dimension_extract(HosDimension* self, const gdouble A, const gdouble B)
{
  gdouble downfield = A > B ? A : B;
  gdouble upfield   = A > B ? B : A;

  dimension_clip_lower(self, dimension_hz2pt(self, downfield));
  dimension_clip_upper(self, dimension_hz2pt(self, upfield));

}

void
dimension_extract_ppm(HosDimension* self, const gdouble A, const gdouble B)
{
  gdouble downfield = A > B ? A : B;
  gdouble upfield   = A > B ? B : A;

  dimension_clip_lower(self, dimension_ppm2pt(self, downfield));
  dimension_clip_upper(self, dimension_ppm2pt(self, upfield));
}

void
dimension_clip_lower(HosDimension* self, const guint pt)
{
  if (HOS_DIMENSION_GET_CLASS(self)->clip_lower)
    HOS_DIMENSION_GET_CLASS(self)->clip_lower(self, pt);

  self->orig -= (self->sw * ((gdouble)pt / (gdouble)self->np));
  self->sw *= 1.0 - ((gdouble)pt / (gdouble)self->np);
  self->np -= pt;
}

static void
dimension_clip_upper(HosDimension* self, const guint pt)
{
  if (HOS_DIMENSION_GET_CLASS(self)->clip_upper)
    HOS_DIMENSION_GET_CLASS(self)->clip_upper(self, pt);

  self->sw *= ((gdouble)pt / (gdouble)self->np);
  self->np = pt;
}

gdouble
dimension_hz2pt(HosDimension* self, const gdouble hz)
{
  gdouble result = ((self->orig - hz) / self->sw) * self->np;

  if (result < 0) result = 0;
  if (result >= self->np) result = (self->np - 1);

  return result;

}

gdouble
dimension_ppm2pt(HosDimension* self, const gdouble ppm)
{
  return dimension_hz2pt(self, ppm * self->sf);
}

gdouble
dimension_pt2hz(HosDimension* self, const gdouble pt)
{
  return (self->orig - (self->sw * ((gdouble)pt / (gdouble)(self->np))));
}

gdouble
dimension_pt2ppm(HosDimension* self, const gdouble pt)
{
  return (dimension_pt2hz(self, pt) / self->sf);
}

/*
 * Interpolate this dimension to fit in the requested number
 * of points without changing the sweep width.
 *
 * The second argument is passed as a gpointer so that this
 * can be used as a list traversal callback.
 */
void
dimension_interpolate(HosDimension *self, gpointer new_np)
{
  if (HOS_DIMENSION_GET_CLASS(self)->interpolate)
    HOS_DIMENSION_GET_CLASS(self)->interpolate(self, GPOINTER_TO_UINT(new_np));
  self->np = GPOINTER_TO_UINT(new_np);
}

/*
 * A dimension incrementor that can be used as a callback during
 * glist of dimensions traversal
 */
void
dimension_increment(HosDimension* self, gboolean *done)
{
  HosDimensionClass *my_class;

  assert(self->iterate_index < self->np);

  if ((self->iterate_index + 1) == self->np)
    {
      *done = TRUE;
      return;
    }
  my_class = HOS_DIMENSION_GET_CLASS(self);

  my_class->increment(self, self->backing);
  self->iterate_index++;

}

void
dimension_reset(HosDimension* self, gpointer data)
{
  HOS_DIMENSION_GET_CLASS(self)->reset(self, self->backing);
  self->iterate_index = 0;
}

/*
 * Like a reset but assumes that state
 * information in the dimension is not synchronized
 * with state information in the backing object and
 * that the backing object has been reset to its starting
 * place.
 */
void
dimension_prime(HosDimension* self, gpointer data)
{
  HOS_DIMENSION_GET_CLASS(self)->prime(self, self->backing);
  self->iterate_index = 0;
}



/*
 * FIXME -- is there a clearer way to do this?
 * Dispatch to this class's copy method to return
 * a copy of this dimension; mangling the copy will not
 * alter the original.
 *
 * Important: the backing object in the original and
 * the copy should be identical. (i.e. not deep copied;
 * it should be the same object in memory.)
 */
static HosDimension*
dimen_copy(HosDimension* self)
{
  HosDimension* result = g_object_new(G_TYPE_FROM_INSTANCE(self), NULL);
  HosDimensionClass* self_class = HOS_DIMENSION_GET_CLASS(self);

  if (self_class->copy)
    self_class->copy(self, result);

  return result;

}

/*
 * An example of a dimension class's copy function.
 * This function should always chain up to the parent
 * class copy func (no parent in this case.)
 * Copies all instance-specific data.
 */
static void
dimension_copy_func(HosDimension* src, HosDimension* dest)
{
#define COPY_THE(x) (dest->x) = (src->x)
  COPY_THE(np);
  COPY_THE(orig);
  COPY_THE(sf);
  COPY_THE(sw);

  COPY_THE(np);
  COPY_THE(backing);

#undef COPY_THE
}

GType
hos_dimension_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosDimensionClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_dimension_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosDimension),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_dimension_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "HosDimension",
				     &info,
				     0);
    }

  return type;
}

static void
hos_dimension_class_init (HosDimensionClass *klass)
{
  GObjectClass *gobject_class;

  gobject_class = G_OBJECT_CLASS (klass);
  
  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_dimension_set_property;
  gobject_class->get_property = hos_dimension_get_property;

  klass->copy = dimension_copy_func;

/*

  PROPERTIES GO HERE
  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}



static void
hos_dimension_init(HosDimension  *self)
{
}

static void
hos_dimension_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  HosDimension *self = HOS_DIMENSION(object);

  self=self; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_LABEL:
      gtk_button_set_label (button, g_value_get_string (value));
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
hos_dimension_get_property (GObject         *object,
			   guint            prop_id,
			   GValue          *value,
			   GParamSpec      *pspec)
{
  HosDimension *self = HOS_DIMENSION(object);

  self=self; /* to eliminate warning */

  switch (prop_id)
    {
      /*
    case PROP_IMAGE:
      g_value_set_object (value, (GObject *)priv->image);
      break;
      */
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}


