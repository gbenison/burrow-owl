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

#ifndef _HAVE_SOLVER_H
#define _HAVE_SOLVER_H

#include "model.h"
#include "parameter.h"
#include "burrow/spectrum.h"

G_BEGIN_DECLS

#define HOS_TYPE_SOLVER              (hos_solver_get_type())
#define HOS_SOLVER(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SOLVER, HosSolver))
#define HOS_SOLVER_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SOLVER, HosSolverClass))
#define HOS_IS_SOLVER(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SOLVER))
#define HOS_IS_SOLVER_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SOLVER))
#define HOS_SOLVER_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SOLVER, HosSolverClass))

typedef struct _HosSolver       HosSolver;
typedef struct _HosSolverClass  HosSolverClass;

struct _HosSolver
{
  GObject parent_instance;

  GList *targets;
  GList *parameters;
};

struct _HosSolverClass
{
  GObjectClass parent_class;
};

void    solver_set_model        (HosSolver *self, HosSpectrum *spectrum, HosModel *model);
void    solver_set_noise        (HosSolver *self, HosSpectrum *spectrum, gdouble noise);
void    solver_append_parameter (HosSolver *self, HosParameter *parameter);
gdouble solver_square_error     (HosSolver *self);

GType hos_solver_get_type(void);

G_END_DECLS

#endif /* not _HAVE_SOLVER_H */

