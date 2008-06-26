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

#ifndef _HAVE_MODEL_SUBTYPES_H
#define _HAVE_MODEL_SUBTYPES_H

#include "model.h"
#include "model-subtypes-gen.h"

G_BEGIN_DECLS

struct _HosModelSum
{
  HosModel parent_instance;

  HosModel *A;
  HosModel *B;
};

struct _HosModelSumClass
{
  HosModelClass parent_class;
};

struct _HosModelProduct
{
  HosModel parent_instance;

  HosModel *A;
  HosModel *B;
};

struct _HosModelProductClass
{
  HosModelClass parent_class;
};

struct _HosModelGaussian
{
  HosModel parent_instance;
  HosModel *argument;
};

struct _HosModelGaussianClass
{
  HosModelClass parent_class;
};

struct _HosModelDimension
{
  HosModel parent_instance;
};

struct _HosModelDimensionClass
{
  HosModelClass parent_class;
};

#define CONSTRUCTOR  /* empty */

HosModel* model_sum      (HosModel *A, HosModel *B);
HosModel* model_product  (HosModel *A, HosModel *B);
HosModel* model_gaussian (HosModel *src);

G_END_DECLS

#endif /* not  _HAVE_MODEL_SUBTYPES_H */


