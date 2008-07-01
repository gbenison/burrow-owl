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

#ifndef _HAVE_SOLVER_ANNEAL_H
#define _HAVE_SOLVER_ANNEAL_H

#include "solver.h"

G_BEGIN_DECLS

#define HOS_TYPE_SOLVER_ANNEAL              (hos_solver_anneal_get_type())
#define HOS_SOLVER_ANNEAL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SOLVER_ANNEAL, HosSolverAnneal))
#define HOS_SOLVER_ANNEAL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SOLVER_ANNEAL, HosSolverAnnealClass))
#define HOS_IS_SOLVER_ANNEAL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SOLVER_ANNEAL))
#define HOS_IS_SOLVER_ANNEAL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SOLVER_ANNEAL))
#define HOS_SOLVER_ANNEAL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SOLVER_ANNEAL, HosSolverAnnealClass))

typedef struct _HosSolverAnneal       HosSolverAnneal;
typedef struct _HosSolverAnnealClass  HosSolverAnnealClass;

struct _HosSolverAnneal
{
  HosSolver parent_instance;
};

struct _HosSolverAnnealClass
{
  HosSolverClass parent_class;
};

int solver_anneal_sample(HosSolverAnneal *self, int n_step, double step_scale, double temperature);

G_END_DECLS

#endif /* not _HAVE_SOLVER_ANNEAL_H */

