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

#ifndef _HAVE_SPECTRUM_MODEL_H
#define _HAVE_SPECTRUM_MODEL_H

#include "burrow/spectrum.h"
#include "model.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_MODEL              (hos_spectrum_model_get_type())
#define HOS_SPECTRUM_MODEL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_MODEL, HosSpectrumModel))
#define HOS_SPECTRUM_MODEL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_MODEL, HosSpectrumModelClass))
#define HOS_IS_SPECTRUM_MODEL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_MODEL))
#define HOS_IS_SPECTRUM_MODEL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_MODEL))
#define HOS_SPECTRUM_MODEL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_MODEL, HosSpectrumModelClass))

typedef struct _HosSpectrumModel       HosSpectrumModel;
typedef struct _HosSpectrumModelClass  HosSpectrumModelClass;

struct _HosSpectrumModel
{
  HosSpectrum parent_instance;

  HosModel *model;
};

struct _HosSpectrumModelClass
{
  HosSpectrumClass parent_class;
};

#define CONSTRUCTOR  /* empty */

HosSpectrum* CONSTRUCTOR spectrum_from_model    (HosModel *model, gdouble *orig, gdouble *sw, guint *np, gint ndim);
HosSpectrum* CONSTRUCTOR spectrum_1d_from_model (HosModel *model, gdouble orig, gdouble sw, guint np);

GType hos_spectrum_model_get_type (void);

G_END_DECLS

#endif /* not _HAVE_SPECTRUM_MODEL_H */
