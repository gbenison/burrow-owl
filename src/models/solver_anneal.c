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

#include "solver_anneal.h"
#include <math.h>
#include <gsl/gsl_randist.h>

#define SOLVER_ANNEAL_GET_PRIVATE(o)    (G_TYPE_INSTANCE_GET_PRIVATE ((o), HOS_TYPE_SOLVER_ANNEAL, HosSolverAnnealPrivate))
#define SOLVER_ANNEAL_PRIVATE(o, field) ((SOLVER_ANNEAL_GET_PRIVATE(o))->field)
typedef struct _HosSolverAnnealPrivate HosSolverAnnealPrivate;

struct _HosSolverAnnealPrivate
{
  gint place_holder;
};

G_DEFINE_TYPE (HosSolverAnneal, hos_solver_anneal, HOS_TYPE_SOLVER)

static void spherical_step
(
 const gsl_rng* r,
 double const * step_size,
 double* dest,
 const int n_values,
 const double step_size_factor
);
static gsl_rng* rng_default(void);


static void
hos_solver_anneal_class_init(HosSolverAnnealClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
  g_type_class_add_private(gobject_class, sizeof(HosSolverAnnealPrivate));
}

static void
hos_solver_anneal_init(HosSolverAnneal *self)
{
}

int
solver_anneal_sample(HosSolverAnneal *self, int n_step, double step_scale, double temperature)
{
  HosSolver *solver = HOS_SOLVER(self);

  int n_par     = g_list_length(solver->parameters);
  int n_spectra = g_list_length(solver->targets);

  gint n_accept = 0;

  g_return_if_fail(n_spectra > 0);
  g_return_if_fail(n_par > 0);

  gdouble old_par_values[n_par];
  gdouble delta[n_par];
  gdouble step_size_array[n_par];
  gdouble sum_x[n_par];
  gdouble sum_xx[n_par];

  gdouble last_energy = 0;

  GList *par;
  int j = 0;
  for (par = solver->parameters; par != NULL; par = par->next)
    {
      step_size_array[j] = HOS_PARAMETER(par->data)->prior_stddev;
      sum_x[j] = 0;
      sum_xx[j] = 0;
      j++;
    }

  /* FIXME cache non-mutable models? */

  int i;
  for (i = 0; i < n_step; ++i)
    {
      /* take a random step */
      spherical_step(rng_default(), step_size_array, delta, n_par, step_scale);

      gdouble energy = 0;

      /* nudge parameters, saving the old values */
      GList *par_list;
      int j = 0;
      for (par_list = solver->parameters; par_list != NULL; par_list = par_list->next)
	{
	  HosParameter *parameter = HOS_PARAMETER(par_list->data);
	  old_par_values[j] = parameter->value;
	  parameter->value += delta[j];
	  gdouble par_delta = (parameter->value - parameter->prior_mean) / parameter->prior_stddev;
	  energy += par_delta * par_delta;
	  j++;
	}

      energy += solver_square_error(solver);

      /* FIXME either accept or keep new values */
      gboolean accepted;
      if ((energy < last_energy) || (last_energy == 0))
	accepted = TRUE;
      else
	accepted = (exp((last_energy - energy) / temperature) > gsl_rng_uniform(rng_default()));
      if (accepted)
	{
	  last_energy = energy;
	  ++n_accept;
	  /* FIXME accumulate parameters */
	  GList *par_list;
	  int j = 0;
	  for (par_list = solver->parameters; par_list != NULL; par_list = par_list->next)
	    {
	      HosParameter *parameter = HOS_PARAMETER(par_list->data);
	      sum_x[j] += parameter->value;
	      sum_xx[j] += (parameter->value * parameter->value);
	      j++;
	    }
	}
      else
	{
	  /* restore parameter values */
	  GList *par_list;
	  int j = 0;
	  for (par_list = solver->parameters; par_list != NULL; par_list = par_list->next)
	    {
	      HosParameter *parameter = HOS_PARAMETER(par_list->data);
	      parameter->value = old_par_values[j];
	      j++;
	    }
	}
    }

  return n_accept;

}

/*
 * take a step in a spherical parameter space,
 * scaled by the given step_size values.
 * The radius of the step in spherical space is
 * an exponential distribution with mean step_size_factor.
 * The direction in spherical space is random.
 *
 * argument 'step_size' must be an array of scaling factors
 * consisting of n_values entries.
 *
 * The resulting steps will be placed in 'dest', which must
 * have sufficient storage for n_values items.
 */
static void
spherical_step
(
 const gsl_rng* r,
 double const * step_size,
 double* dest,
 const int n_values,
 const double step_size_factor
)
{
  int i;
  double radius_sqr = 0;
  double radius;
  double radius_mean;

  g_assert(n_values > 0);
  g_assert((dest != NULL) && (step_size != NULL));

  /* generate spherical steps */
  for (i=0; i < n_values; ++i)
    {
      double step;

      step = gsl_ran_gaussian(r, 1.0);
      dest[i] = step;
      radius_sqr += step * step;
    }
  radius = sqrt(radius_sqr);

  /* generate a desired radius */
  radius_mean = gsl_ran_exponential(r, step_size_factor);

  /* scale steps to radius, and requested step size */
  for (i=0; i < n_values; ++i)
    {
      dest[i] *= ( step_size[i] * radius_mean / radius );
    }

}

static gsl_rng*
rng_default(void)
{
  static gsl_rng* rng = NULL;
  if (rng == NULL)
    {
      gsl_rng_env_setup();
      rng = gsl_rng_alloc(gsl_rng_default);
    }
  return rng;
}
