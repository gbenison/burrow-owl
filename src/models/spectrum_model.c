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

#include "spectrum_model.h"

G_DEFINE_TYPE (HosSpectrumModel, hos_spectrum_model, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_model_class_init(HosSpectrumModelClass *klass)
{
  GObjectClass *gobject_class;
}

static void
hos_spectrum_model_init(HosSpectrumModel *self)
{
}

HosSpectrum*
spectrum_from_model(HosModel *model, gdouble *orig, gdouble *sw, guint *np, gint ndim)
{
  /* FIXME check dim count */
  HosSpectrum      *result         = g_object_new(HOS_TYPE_SPECTRUM_MODEL, NULL);
  HosSpectrumModel *spectrum_model = HOS_SPECTRUM_MODEL(result);

  spectrum_model->model = model;
  /* FIXME */

  return result;
}

HosSpectrum*
spectrum_1d_from_model(HosModel *model, gdouble orig, gdouble sw, guint np)
{
  return spectrum_from_model(model, &orig, &sw, &np, 1);
}
