/*
 *  Copyright (C) 2006 Greg Benison
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
#include "hosdimensionsim.h"
#include "hosbackingsim.h"


static HosDimensionClass *parent_class = NULL;

static void hos_dimension_sim_class_init (HosDimensionSimClass *klass);
static void hos_dimension_sim_init(HosDimensionSim*);
static void hos_dimension_sim_set_property (GObject         *object,
					      guint            prop_id,
					      const GValue    *value,
					      GParamSpec      *pspec);
static void hos_dimension_sim_get_property (GObject         *object,
					      guint            prop_id,
					      GValue          *value,
					      GParamSpec      *pspec);
static void hos_dimension_sim_init(HosDimensionSim *spec);

static void sim_copy_func(HosDimensionSim*, HosDimensionSim*);
static void sim_reset(HosDimensionSim *self, HosBackingSim *backing);
static void sim_prime(HosDimensionSim *self, HosBackingSim *backing);

static void
sim_copy_func(HosDimensionSim* src, HosDimensionSim* dest)
{
  parent_class->copy((HosDimension*)src, (HosDimension*)dest);

#define COPY_THE(x) (dest->x) = (src->x)
  COPY_THE(backing_dim);
#undef COPY_THE

}

GType
hos_dimension_sim_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosDimensionSimClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_dimension_sim_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosDimensionSim),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_dimension_sim_init,
      };

      type = g_type_register_static (HOS_TYPE_DIMENSION,
				     "HosDimensionSim",
				     &info,
				     0);

    }

  return type;
}

/* note: this is called _before_ self->iterate_index++ */
static void
sim_increment(HosDimensionSim *self, HosBackingSim *backing)
{
  /* note: pt2ppm is called _before_ index increments! */
  HosDimension* dimen = HOS_DIMENSION(self);
  gdouble ppm = dimension_pt2ppm(dimen, dimen->iterate_index + 1);

  backing_sim_set_pos(HOS_BACKING_SIM(dimen->backing), self->backing_dim, ppm);

}

static void
sim_reset(HosDimensionSim *self, HosBackingSim *backing)
{
  backing_sim_set_pos(HOS_BACKING_SIM(HOS_DIMENSION(self)->backing),
		      self->backing_dim,
		      dimension_pt2ppm(HOS_DIMENSION(self), 0));
}

static void
sim_prime(HosDimensionSim *self, HosBackingSim *backing)
{
  sim_reset(self, backing);
}

static gdouble
sim_cost(HosDimensionSim* self)
{
  return 1.0;
}

static void
hos_dimension_sim_class_init (HosDimensionSimClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  HosDimensionClass *dimension_class = HOS_DIMENSION_CLASS(klass);

  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_dimension_sim_set_property;
  gobject_class->get_property = hos_dimension_sim_get_property;

  dimension_class->copy = (DimenCopyFunc)sim_copy_func;
  dimension_class->increment = (IncrementFunc)sim_increment;
  dimension_class->reset = (IncrementFunc)sim_reset;
  dimension_class->prime = (IncrementFunc)sim_prime;
  dimension_class->cost = (DimenCostFunc)sim_cost;

/*

  PROPERTIES GO HERE
  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}

static void
hos_dimension_sim_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  HosDimensionSim *sim = HOS_DIMENSION_SIM(object);

  sim=sim; /* to eliminate warning */

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
hos_dimension_sim_get_property (GObject         *object,
			   guint            prop_id,
			   GValue          *value,
			   GParamSpec      *pspec)
{
  HosDimensionSim *sim = HOS_DIMENSION_SIM(object);

  sim=sim; /* to eliminate warning */

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

static void
hos_dimension_sim_init(HosDimensionSim* self)
{
  /* FIXME */
}

HosDimensionSim*
dimension_sim_new(gdouble min, gdouble max, guint np)
{
  HosDimensionSim* result = g_object_new(HOS_TYPE_DIMENSION_SIM, NULL);
  HosDimension* dimen = HOS_DIMENSION(result);

  dimen->sf = 1000.0;   /* will this software still be in use when sf really is 1GHz?? */
  dimen->sw = (max - min) * dimen->sf;
  dimen->orig = max * dimen->sf;
  dimen->np = np;

  return result;
}

