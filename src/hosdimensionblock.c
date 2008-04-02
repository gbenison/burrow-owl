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
 * Base type for the 'spectrum' abstraction--
 * An object which is capable of providing iterators
 *
 * intended to be subclassed to provide things like
 * convoluted spectra, specific spectrum implementations
 * i.e. block float format, simulated spectra
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
#include "hosdimensionblock.h"
#include "hosbackingblock.h"


static HosDimensionClass *parent_class = NULL;

static void hos_dimension_block_class_init (HosDimensionBlockClass *klass);
static void hos_dimension_block_init(HosDimensionBlock*);
static void hos_dimension_block_set_property (GObject         *object,
					      guint            prop_id,
					      const GValue    *value,
					      GParamSpec      *pspec);
static void hos_dimension_block_get_property (GObject         *object,
					      guint            prop_id,
					      GValue          *value,
					      GParamSpec      *pspec);
static void hos_dimension_block_init(HosDimensionBlock *spec);

static void block_copy_func(HosDimensionBlock*, HosDimensionBlock*);
static void block_clip_lower(HosDimensionBlock *self, const guint np);
static void block_prime(HosDimensionBlock *self, HosBackingBlock *backing);

/*
 * Note: we have to deep-copy the schedule too, because
 * when this dimension is clipped or interpolated, its old
 * schedule is freed.
 */
static void
block_copy_func(HosDimensionBlock* src, HosDimensionBlock* dest)
{
  parent_class->copy((HosDimension*)src, (HosDimension*)dest);

#define COPY_THE(x) (dest->x) = (src->x)
  COPY_THE(sw_physical);
  COPY_THE(np_physical);
  COPY_THE(stride);
  COPY_THE(initial_offset);
  COPY_THE(pointer_physical);
  COPY_THE(negated_initially);
  COPY_THE(negate_on_fold);
#undef COPY_THE

  if (src->schedule)
    {
      int i;

      dest->schedule = g_new(guint, HOS_DIMENSION(src)->np);
      for (i = 0; i < HOS_DIMENSION(src)->np; ++i)
	dest->schedule[i] = src->schedule[i];
    }

  dest->schedule_ptr = dest->schedule;

}

void
block_interpolate(HosDimensionBlock *self, const guint new_np)
{
  HosDimension *dimen = HOS_DIMENSION(self);

  /* In the actual data, how many hz per point are there? */
  gdouble digital_resolution = self->sw_physical / self->np_physical;

  /* How many physical points correspond to this sweep width? */
  gdouble physical_pt_sw = dimen->sw / digital_resolution;

  /* How many physical points must I increment for each client point? */
  gdouble pt_delta = physical_pt_sw / new_np;

  /*  gdouble pt_delta = (dimen->sw / self->sw_physical) * self->np_physical / new_np; */

  gdouble pt_accumulator = 0;
  int i;
  guint* schedule = g_new(guint, new_np);

  /* first we fill the schedule with absolute point indices */
  for (i = 0; i < new_np; ++i)
    {
      pt_accumulator += pt_delta;
      schedule[i] = round(pt_accumulator);
    }

  /* then we take the discrete differences */
  for (i = new_np - 1; i > 0; --i)
    schedule[i] -= schedule[i - 1];

  /* then set the dimension's schedule to our new one */
  if (self->schedule != NULL)
    g_free(self->schedule);

  self->schedule = schedule;

}

GType
hos_dimension_block_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo info =
      {
	sizeof (HosDimensionBlockClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_dimension_block_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosDimensionBlock),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_dimension_block_init,
      };

      type = g_type_register_static (HOS_TYPE_DIMENSION,
				     "HosDimensionBlock",
				     &info,
				     0);

    }

  return type;
}

static void
block_increment(HosDimensionBlock *self, HosBackingBlock *backing)
{
  guint old_pos = self->pointer_physical;

  if (self->schedule == NULL)
    self->pointer_physical += 1;
  else
    {
      self->pointer_physical += *(self->schedule_ptr);
      self->schedule_ptr++;
    }

  /* folding correction */
  while ((self->pointer_physical) >= self->np_physical)
    {
      self->pointer_physical -= self->np_physical;
      if (self->negate_on_fold)
	{
	  self->negated = self->negated ? FALSE : TRUE;
	  backing_negate(HOS_BACKING(backing));
	}
    }

  backing_block_seek_cur(backing, (self->pointer_physical - old_pos) * self->stride);

}

static void
block_reset(HosDimensionBlock *self, HosBackingBlock *backing)
{
  int n_retract;

  n_retract = self->initial_offset - self->pointer_physical;
  self->pointer_physical = self->initial_offset;
  backing_block_seek_cur(backing, n_retract * self->stride);
  self->schedule_ptr = self->schedule;

  if ((self->negated_initially == FALSE) && (self->negated == TRUE))
    backing_negate(HOS_BACKING(backing));
  if ((self->negated_initially == TRUE) && (self->negated == FALSE))
    backing_negate(HOS_BACKING(backing));

  self->negated = self->negated_initially;

}

static void
block_prime(HosDimensionBlock *self, HosBackingBlock *backing)
{
  self->pointer_physical = self->initial_offset;
  self->schedule_ptr = self->schedule;
  backing_block_seek_cur(backing, self->pointer_physical * self->stride);
  if (self->negated_initially == TRUE)
    backing_negate(HOS_BACKING(backing));

  self->negated = self->negated_initially;
}

static void
hos_dimension_block_class_init (HosDimensionBlockClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  HosDimensionClass *dimension_class = HOS_DIMENSION_CLASS(klass);

  parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = hos_dimension_block_set_property;
  gobject_class->get_property = hos_dimension_block_get_property;

  dimension_class->copy = (DimenCopyFunc)block_copy_func;
  dimension_class->interpolate = (InterpolateFunc)block_interpolate;
  dimension_class->increment = (IncrementFunc)block_increment;
  dimension_class->reset = (ResetFunc)block_reset;
  dimension_class->prime = (ResetFunc)block_prime;
  dimension_class->clip_lower = (void_func_dimen_guint)block_clip_lower;

/* here is where you would set klass->member etc. */
  
/*

  PROPERTIES GO HERE
  SIGNALS GO HERE
  PRIVATE GOES HERE:

  g_type_class_add_private (gobject_class, sizeof (GtkButtonPrivate));  

*/
}

/*
 * Adjust this spectrum's number of points by
 * unfolding the requested number of times in the
 * given dimension.
 */
void
dimension_block_unfold(HosDimensionBlock *self,
		       const guint downfield,
		       const guint upfield,
		       const gboolean negate_on_fold)
{
  HosDimension* dimen = HOS_DIMENSION(self);

  g_return_if_fail(self->fold_allowed == TRUE);
  self->fold_allowed = FALSE;

  /* adjust np, sweepwidth... */
  self->negate_on_fold = negate_on_fold;
  if ((downfield % 2) == 1)
    self->negated_initially = TRUE;
  dimen->orig += downfield * dimen->sw;
  dimen->sw *= downfield + upfield + 1;
  dimen->np *= downfield + upfield + 1;

}

/*
 * Remove the first 'np' points from this block dimension.
 */
static void
block_clip_lower(HosDimensionBlock *self, const guint np)
{
  int i;
  HosDimension *dimen = HOS_DIMENSION(self);

  /*
   * The argument 'np' is in 'client points'-- the point
   * index seen when traversing the spectrum.
   * We must adjust the 'initial_offset' member which is
   * in physical points-- the index in the underlying
   * data representation.
   * These indices are different due to folding and interpolation.
   *
   * A NULL schedule is equivalent to a schedule of all 1's.
   */
  self->schedule_ptr = self->schedule;
  for (i = 0; i < np; ++i)
    {
      if (self->schedule)
	{
	  self->initial_offset += *self->schedule_ptr;
	  self->schedule_ptr++;
	}
      else
	self->initial_offset += 1;
      while (self->initial_offset >= self->np_physical)
	{
	  self->initial_offset -= self->np_physical;
	  if (self->negate_on_fold)
	    self->negated_initially = self->negated_initially ? FALSE : TRUE;
	}
    }

  /* If the schedule exists, clip it */
  if (self->schedule)
    {
      guint *new_schedule = g_new(guint, dimen->np - np);
      int i;

      for (i = np; i < dimen->np; ++i)
	new_schedule[i - np] = self->schedule[i];

      g_free(self->schedule);
      self->schedule = new_schedule;
      self->schedule_ptr = self->schedule;
    }
}

static void
hos_dimension_block_set_property (GObject         *object,
			   guint            prop_id,
			   const GValue    *value,
			   GParamSpec      *pspec)
{
  HosDimensionBlock *block = HOS_DIMENSION_BLOCK(object);

  block=block; /* to eliminate warning */

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
hos_dimension_block_get_property (GObject         *object,
			   guint            prop_id,
			   GValue          *value,
			   GParamSpec      *pspec)
{
  HosDimensionBlock *block = HOS_DIMENSION_BLOCK(object);

  block=block; /* to eliminate warning */

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
hos_dimension_block_init(HosDimensionBlock* self)
{
  self->fold_allowed = TRUE;
}

