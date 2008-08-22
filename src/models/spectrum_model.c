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
#include "spectrum_priv.h"
#include "utils.h"

static void spectrum_model_dispose (GObject *object);

G_DEFINE_TYPE (HosSpectrumModel, hos_spectrum_model, HOS_TYPE_SPECTRUM)

static void
hos_spectrum_model_class_init(HosSpectrumModelClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  gobject_class->dispose = spectrum_model_dispose;
}

static void
hos_spectrum_model_init(HosSpectrumModel *self)
{
}

HosSpectrum*
spectrum_from_model(HosModel *model, gdouble *orig, gdouble *sw, guint *np, gint ndim)
{
  g_return_if_fail(ndim == model->ndim);

  HosSpectrum      *result         = g_object_new(HOS_TYPE_SPECTRUM_MODEL, NULL);
  HosSpectrumModel *spectrum_model = HOS_SPECTRUM_MODEL(result);

  int i;

  /* initialize the dimensions */
  GList *dimensions = NULL;
  for (i = 0; i < ndim; ++i)
    {
      dimension_t* dimen = g_new0(dimension_t, 1);
      dimen->np   = np[i];
      dimen->sw   = sw[i];
      dimen->sf   = 1.0;
      dimen->orig = orig[i];
      dimensions = g_list_append(dimensions, dimen);
    }

  spectrum_set_dimensions(result, dimensions);

  spectrum_model->model = model;
  g_object_ref(spectrum_model->model);
  int total_np = spectrum_np_total(result);
  gdouble *buf = g_new(gdouble, total_np);

  /* instantiate the model and copy to spectrum buffer */
  gdouble delta[ndim];
  for (i = 0; i < ndim; ++i)
    delta[i] = sw[i] / np[i];

  model_iterator_t *model_iterator = model_iterator_new(model, orig, delta, np);
  model_iterator_fill(model_iterator, buf);
  model_iterator_free(model_iterator);

  spectrum_set_contents(result, buf);

  return result;
}

HosSpectrum*
spectrum_1d_from_model(HosModel *model, gdouble orig, gdouble sw, guint np)
{
  return spectrum_from_model(model, &orig, &sw, &np, 1);
}

HosSpectrum*
spectrum_2d_from_model (HosModel *model,
			gdouble orig1, gdouble sw1, guint np1,
			gdouble orig2, gdouble sw2, guint np2)
{
  gdouble orig[2] = {orig1, orig2};
  gdouble sw[2]   = {sw1, sw2};
  guint   np[2]   = {np1, np2};

  return spectrum_from_model(model, orig, sw, np, 2);
}

static void
spectrum_model_dispose (GObject *object)
{
  G_OBJECT_UNREF_AND_CLEAR(HOS_SPECTRUM_MODEL(object)->model);
  G_OBJECT_CLASS(hos_spectrum_model_parent_class)->dispose(object);
}
