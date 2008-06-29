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

struct solver_target
{
  HosSpectrum *spectrum;
  HosModel    *model;
  gdouble      noise;
};

struct _HosSolverPrivate
{
  GList    *targets;
  GList    *parameters;
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
  for (target = SOLVER_PRIVATE(self, targets); target != NULL; target = target->next)
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
      HosSolverPrivate *priv = SOLVER_GET_PRIVATE(self);
      priv->targets = g_list_append(priv->targets, target);
    }

  target->spectrum = spectrum;
  target->model    = model;
  target->noise    = spectrum_stddev(spectrum);
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

  HosSolverPrivate *priv = SOLVER_GET_PRIVATE(self);

  if (g_list_index(priv->parameters, parameter) < 0)
    priv->parameters = g_list_append(priv->parameters, parameter);

  /* FIXME note any 'model-caching' is now invalid */
    
}
