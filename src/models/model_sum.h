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

#ifndef _HAVE_MODEL_SUM_H
#define _HAVE_MODEL_SUM_H

#define HOS_TYPE_MODEL_SUM              (hos_model_sum_get_type())
#define HOS_MODEL_SUM(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_MODEL_SUM, HosModelSum))
#define HOS_MODEL_SUM_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_MODEL_SUM, HosModelSumClass))
#define HOS_IS_MODEL_SUM(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_MODEL_SUM))
#define HOS_IS_MODEL_SUM_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_MODEL_SUM))
#define HOS_MODEL_SUM_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_MODEL_SUM, HosModelSumClass))
					
typedef struct _HosModelSum       HosModelSum;
typedef struct _HosModelSumClass  HosModelSumClass;

struct _HosModel
{
  HosModel parent_instance;
};

struct _HosModelClass
{
  HosModelClass parent_class;
};

#endif /*  _HAVE_MODEL_SUM_H */
