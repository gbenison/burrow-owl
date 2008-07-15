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

#include "solver.h"

#define SOLVER_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SOLVER, HosSolverPrivate))
#define SOLVER_PRIVATE(o, field) ((SOLVER_GET_PRIVATE(o))->field)
typedef struct _HosSolverPrivate HosSolverPrivate;

struct _HosSolverPrivate
{
  gint place_holder;
};

struct solver_target
{
  HosSpectrum      *spectrum;
  HosModel         *model;
  gint              np_total;
  model_iterator_t *model_iterator;
  gdouble          *model_buffer;
  gdouble           noise;
};

static struct solver_target* solver_find_target(HosSolver *self, HosSpectrum *spectrum);

G_DEFINE_ABSTRACT_TYPE (HosSolver, hos_solver, G_TYPE_OBJECT)

static void
hos_solver_class_init(HosSolverClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  g_type_class_add_private(gobject_class, sizeof(HosSolverPrivate));
}

static void
hos_solver_init(HosSolver *self)
{
}

/*
 * (return_value)->spectrum == spectrum, or NULL
 */
static struct solver_target*
solver_find_target(HosSolver *self, HosSpectrum *spectrum)
{
  GList *target;
  for (target = self->targets; target != NULL; target = target->next)
    if (((struct solver_target*)(target->data))->spectrum == spectrum)
      return target->data;
  
  return NULL;
}

/*
 * Ensure target (spectrum, model) is present in solver 'self'.
 * 'model' replaces any previous HosModel associated with 'spectrum' in 'self'.
 * Noise is set to rmsd of 'spectrum'.
 */
void
solver_set_model(HosSolver *self, HosSpectrum *spectrum, HosModel *model)
{
  g_return_if_fail(HOS_IS_SOLVER(self));
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));
  g_return_if_fail(HOS_IS_MODEL(model));

  struct solver_target* target = solver_find_target(self, spectrum);
  if (target == NULL)
    {
      target = g_new0(struct solver_target, 1);
      self->targets = g_list_append(self->targets, target);
    }

  target->spectrum       = spectrum;
  target->model          = model;
  target->model_buffer   = g_new(gdouble, spectrum_np_total(spectrum));
  target->noise          = spectrum_stddev(spectrum);
  target->np_total       = spectrum_np_total(spectrum);
}

void
solver_set_noise(HosSolver *self, HosSpectrum *spectrum, gdouble noise)
{
  g_return_if_fail(HOS_IS_SOLVER(self));
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  struct solver_target *target = solver_find_target(self, spectrum);
  g_return_if_fail(target == NULL);

  target->noise = noise;
}

void
solver_append_parameter(HosSolver *self, HosParameter *parameter)
{
  g_return_if_fail(HOS_IS_PARAMETER(parameter));
  g_return_if_fail(HOS_IS_SOLVER(self));

  if (g_list_index(self->parameters, parameter) < 0)
    self->parameters = g_list_append(self->parameters, parameter);

  /* FIXME note any 'model-caching' is now invalid */
    
}

/*
 * Returns:
 * sum for all self->targets:
 *   sum of all ((S(i) - M(i)) / noise)^2
 */
gdouble
solver_square_error(HosSolver *self)
{
  gdouble result = 0;

  GList *target_p;
  for (target_p = self->targets; target_p != NULL; target_p = target_p->next)
    {
      struct solver_target* target = (struct solver_target*)(target_p->data);
      HosSpectrum *spectrum = target->spectrum;
      gdouble *spec_data = spectrum_traverse_blocking(spectrum);
      if (target->model_iterator == NULL)
	{
	  int ndim = spectrum_ndim(spectrum);
	  gdouble orig[ndim];
	  gdouble delta[ndim];
	  gint np[ndim];
	  int i;
	  for (i = 0; i < ndim; ++i)
	    {
	      orig[i]  = spectrum_orig_ppm(spectrum, i);
	      delta[i] = spectrum_sw_ppm(spectrum, i) / spectrum_np(spectrum, i);
	      np[i]    = spectrum_np(spectrum, i);
	    }
	  target->model_iterator = model_iterator_new(target->model, orig, delta, np);
	}
      model_iterator_fill(target->model_iterator, target->model_buffer);

      gdouble this_result = 0;
      int j;
      for (j = 0; j < target->np_total; ++j)
	{
	  gdouble delta = (spec_data[j] - target->model_buffer[j]);
	  this_result += delta * delta;
	}
      this_result /= (target->noise * target->noise);
      result += this_result;
    }
  return result;
}
