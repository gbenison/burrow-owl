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

#include <math.h>
#include "contour.h"

enum {
  CONFIGURATION_CHANGED,
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_THRES_ADJUSTMENT
};

static void contour_configuration_changed (HosContour *contour);
static void contour_adjustment_changed(GtkAdjustment *adjustment, gpointer data);
static void sync_params(HosContour* self);
static void hos_contour_class_init(HosContourClass *klass);
static void hos_contour_init(HosContour *contour);

static GObjectClass *parent_class = NULL;
static guint contour_signals[LAST_SIGNAL] = { 0 };

GType
contour_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo _info =
      {
	sizeof (HosContourClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) hos_contour_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (HosContour),
	16,		/* n_preallocs */
	(GInstanceInitFunc) hos_contour_init,
      };

      type = g_type_register_static (G_TYPE_OBJECT,
				     "HosContour",
				     &_info,
				     0 /* flags */ );

    }

  return type;
}

static void
hos_contour_class_init(HosContourClass *klass)
{
  parent_class = g_type_class_peek_parent(klass);
  klass->configuration_changed = contour_configuration_changed;

  contour_signals[CONFIGURATION_CHANGED] =
    g_signal_new ("configuration-changed",
		  G_OBJECT_CLASS_TYPE(klass),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET(HosContourClass, configuration_changed),
		  NULL, NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE, 0);

}

static void
hos_contour_init(HosContour *contour)
{
  contour->levels = NULL;
  contour->lines = NULL;

  /* set some reasonable default contouring parameters */
  contour_set_thres_adjustment(contour,
			       GTK_ADJUSTMENT(gtk_adjustment_new(6.0,  /* value */
								 0.0,  /* lower */
								 12.0, /* upper */
								 0.1,  /* step_increment */
								 0.0,  /* page_increment */
								 0.0))); /* page_size */
  contour_set_factor_adjustment(contour,
				GTK_ADJUSTMENT(gtk_adjustment_new(1.2, 1.0, 5.0, 0.01, 0.0, 0.0)));
  contour_set_nlvl_adjustment(contour,
			      GTK_ADJUSTMENT(gtk_adjustment_new(20.0, 0.0, 100.0, 1.0, 0.0, 0.0)));

  /* set some default pleasing hues */
  contour_set_color_positive(contour, 5000, 65000,  0, 0,  40000, 60000);
  contour_set_color_negative(contour, 30000, 60000, 5000, 40000, 0, 0);

  sync_params(contour);

}

void
contour_set_color_positive(HosContour* self, guint16 red_min,   guint16 red_max,
                                              guint16 green_min, guint16 green_max,
                                              guint16 blue_min,  guint16 blue_max)
{
  self->red_min_pos    =  red_min;
  self->red_max_pos 	  =  red_max;
  self->blue_min_pos	  =  blue_min;
  self->blue_max_pos	  =  blue_max;
  self->green_min_pos  =  green_min;
  self->green_max_pos  =  green_max;

  sync_params(self);
}

void
contour_set_color_negative(HosContour* self, guint16 red_min,   guint16 red_max,
                                              guint16 green_min, guint16 green_max,
                                              guint16 blue_min,  guint16 blue_max)
{
  self->red_min_neg    =  red_min;
  self->red_max_neg 	  =  red_max;
  self->blue_min_neg	  =  blue_min;
  self->blue_max_neg	  =  blue_max;
  self->green_min_neg  =  green_min;
  self->green_max_neg  =  green_max;

  sync_params(self);
}

/*
 * 'configuration-changed' signal is emitted upon any change
 * in this contour's drawing parameters (n-lvl, thres, etc.)
 */
static void
contour_configuration_changed (HosContour *contour)
{
}

static void
contour_adjustment_changed(GtkAdjustment *adjustment,
			   gpointer data)
{
  HosContour *contour = HOS_CONTOUR(data);
  sync_params(contour);
}

/*
 * Make contour drawing parameters internally consistent;
 * usually called after a parameter changes.
 * e.g. if the contour threshold changes, then all the levels
 * must be recalculated.
 *
 * The 'configuration-changed' signal is emitted after updates are complete.
 */
static void
sync_params(HosContour* self)
{
  int n_contours, index;
  guint n_lvl;

  g_return_if_fail(HOS_IS_CONTOUR(self));

  n_lvl = contour_get_nlvl(self);
  if (n_lvl <= 0)
    return;

  n_contours = self->draw_negative ? n_lvl * 2 : n_lvl;
  self->levels = (gdouble*)g_realloc(self->levels, n_contours * sizeof(gdouble));
  self->lines = (struct contour_linestyle_struct*)
    g_realloc(self->lines, n_contours * sizeof(struct contour_linestyle_struct));

  if (self->draw_negative)
    {
      guint16 delta_red = (self->red_max_neg - self->red_min_neg) / n_lvl;
      guint16 delta_blue = (self->blue_max_neg - self->blue_min_neg) / n_lvl;
      guint16 delta_green = (self->green_max_neg - self->green_min_neg) / n_lvl;

      index = n_lvl - 1;

      self->levels[index] = -contour_get_thres(self);
      self->lines[index].red = self->red_min_neg;
      self->lines[index].blue = self->blue_min_neg;
      self->lines[index].green = self->green_min_neg;

      for (; index > 0; --index)
	{
	  self->levels[index - 1] = self->levels[index] * contour_get_factor(self);
	  self->lines[index - 1].red = self->lines[index].red + delta_red;
	  self->lines[index - 1].blue = self->lines[index].blue + delta_blue;
	  self->lines[index - 1].green = self->lines[index].green + delta_green;
	}
    }

  {

    guint16 delta_red = (self->red_max_pos - self->red_min_pos) / n_lvl;
    guint16 delta_blue = (self->blue_max_pos - self->blue_min_pos) / n_lvl;
    guint16 delta_green = (self->green_max_pos - self->green_min_pos) / n_lvl;
    
    index = self->draw_negative ? n_lvl : 0;

    self->levels[index] = contour_get_thres(self);
    self->lines[index].red = self->red_min_pos;
    self->lines[index].blue = self->blue_min_pos;
    self->lines[index].green = self->green_min_pos;

    for (; index < n_contours - 1; index++)
      {
	self->levels[index + 1] = self->levels[index] * contour_get_factor(self);
	self->lines[index + 1].red = self->lines[index].red + delta_red;
	self->lines[index + 1].blue = self->lines[index].blue + delta_blue;
	self->lines[index + 1].green = self->lines[index].green + delta_green;
      }
  }
  g_signal_emit (self, contour_signals[CONFIGURATION_CHANGED], 0);
}

void
contour_set_draw_negative(HosContour *self, gboolean draw_negative)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (draw_negative == self->draw_negative)
    return;
  self->draw_negative = draw_negative;

  sync_params(self);

}

static gdouble
adjustment_value_if_set(GtkAdjustment* adj)
{
  return GTK_IS_ADJUSTMENT(adj) ?
    gtk_adjustment_get_value(adj) : 0;
}

gdouble
contour_get_thres(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), 0);
  return (pow (10, (adjustment_value_if_set(contour->thres_adjustment))));
}

gdouble
contour_get_factor(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), 0);
  return adjustment_value_if_set(contour->factor_adjustment);
}

guint
contour_get_nlvl(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), 0);
  return adjustment_value_if_set(contour->nlvl_adjustment);
}

gdouble*
contour_get_levels(HosContour* contour)
{
  g_return_val_if_fail(HOS_IS_CONTOUR(contour), NULL);
  return contour->levels;
}

guint
contour_get_n_contours(HosContour *contour)
{
  guint result = contour_get_nlvl(contour);
  if (contour->draw_negative)
    result *= 2;
  return result;
}

GtkAdjustment*
contour_get_thres_adjustment(HosContour *self)
{
  return self->thres_adjustment;
}

GtkAdjustment*
contour_get_factor_adjustment(HosContour *self)
{
  return self->factor_adjustment;
}

GtkAdjustment*
contour_get_nlvl_adjustment(HosContour *self)
{
  return self->nlvl_adjustment;
}

void
contour_set_thres_adjustment(HosContour *self, GtkAdjustment *adjustment)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (!adjustment)
    adjustment = (GtkAdjustment*) gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  else
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));

  if (self->thres_adjustment != adjustment)
    {
      if (self->thres_adjustment)
	{
	  g_signal_handlers_disconnect_by_func (self->thres_adjustment,
						contour_adjustment_changed,
						self);
	  g_object_unref(self->thres_adjustment);
	}
      self->thres_adjustment = adjustment;
      g_object_ref(adjustment);

      g_signal_connect(adjustment, "value_changed",
		       G_CALLBACK(contour_adjustment_changed),
		       self);

      sync_params(self);
    }
}

void
contour_set_factor_adjustment(HosContour *self, GtkAdjustment *adjustment)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (!adjustment)
    adjustment = (GtkAdjustment*) gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  else
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));

  if (self->factor_adjustment != adjustment)
    {
      if (self->factor_adjustment)
	{
	  g_signal_handlers_disconnect_by_func (self->factor_adjustment,
						contour_adjustment_changed,
						self);
	  g_object_unref(self->factor_adjustment);
	}
      self->factor_adjustment = adjustment;
      g_object_ref(adjustment);

      g_signal_connect(adjustment, "value_changed",
		       G_CALLBACK(contour_adjustment_changed),
		       self);

      sync_params(self);
    }
}

void
contour_set_nlvl_adjustment(HosContour *self, GtkAdjustment *adjustment)
{
  g_return_if_fail(HOS_IS_CONTOUR(self));
  if (!adjustment)
    adjustment = (GtkAdjustment*) gtk_adjustment_new (0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  else
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));

  if (self->nlvl_adjustment != adjustment)
    {
      if (self->nlvl_adjustment)
	{
	  g_signal_handlers_disconnect_by_func (self->nlvl_adjustment,
						contour_adjustment_changed,
						self);
	  g_object_unref(self->nlvl_adjustment);
	}
      self->nlvl_adjustment = adjustment;
      g_object_ref(adjustment);

      g_signal_connect(adjustment, "value_changed",
		       G_CALLBACK(contour_adjustment_changed),
		       self);

      sync_params(self);
    }
}


