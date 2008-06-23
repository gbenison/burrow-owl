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

#ifndef _HAVE_MODEL_H
#define _HAVE_MODEL_H

#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define HOS_TYPE_MODEL              (hos_model_get_type())
#define HOS_MODEL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_MODEL, HosModel))
#define HOS_MODEL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_MODEL, HosModelClass))
#define HOS_IS_MODEL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_MODEL))
#define HOS_IS_MODEL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_MODEL))
#define HOS_MODEL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_MODEL, HosModelClass))

typedef struct _HosModel       HosModel;
typedef struct _HosModelClass  HosModelClass;

struct model_iterator
{
  HosModel *root;

  gdouble  *orig;
  gdouble  *sw;
  guint    *np;

  gpointer  data;
};

typedef struct model_iterator model_iterator_t;

struct _HosModel
{
  GObject parent_instance;
  guint   ndim;
};

struct _HosModelClass
{
  GObjectClass parent_class;

  model_iterator_t* (*iterator_construct) (HosModel* self, gdouble *orig, gdouble *sw, guint *np);
  void              (*iterator_eval)      (model_iterator_t* self,  gdouble *dest);
  void              (*iterator_free)      (model_iterator_t* self);
};

/* 
 * The 'CONSTRUCTOR' tag is inserted into function prototypes
 * so that h2def.py will mark them as constructors and allow
 * the caller to own the reference to the return value.
 * The CONSTRUCTOR tag has no influence on C compilation.
 */
#define CONSTRUCTOR  /* empty */

model_iterator_t* model_construct_iterator (HosModel *self, gdouble *orig, gdouble *sw, guint *np);
void              model_iterator_eval      (model_iterator_t *self, gdouble *dest);
void              model_iterator_free      (model_iterator_t *self);

#endif /*  _HAVE_MODEL_H */

