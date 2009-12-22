/*
 *  Copyright (C) 2008, 2009 Greg Benison
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

#include "spectrum-profile.h"

/**
 * @defgroup HosSpectrumProfile
 * @brief    Visual representation of a 1D spectrum
 *
 * Draws a 1D #HosSpectrum as a continuous curve (chemical shift vs.
 * intensity) on a #HosCanvas.
 *
 * @{
 */

enum {
  LAST_SIGNAL
};

enum {
  PROP_0,
  PROP_SPECTRUM,     /**< The spectrum being represented */
  PROP_ORIENTATION   /**< orientation of chemical shift axis */
};

/* guint spectrum_profile_signals[LAST_SIGNAL] = { 0 }; */

static void spectrum_profile_set_property   (GObject         *object,
					     guint            prop_id,
					     const GValue    *value,
					     GParamSpec      *pspec);
static void spectrum_profile_get_property   (GObject         *object,
					     guint            prop_id,
					     GValue          *value,
					     GParamSpec      *pspec);

static void spectrum_profile_configure      (HosCanvasItem *self);
static void sync_points                     (HosSpectrumProfile *self);

G_DEFINE_TYPE (HosSpectrumProfile, hos_spectrum_profile, HOS_TYPE_LINE)

static void
hos_spectrum_profile_init(HosSpectrumProfile *self)
{
}

static void
hos_spectrum_profile_class_init(HosSpectrumProfileClass *klass)
{
  GObjectClass       *gobject_class     = G_OBJECT_CLASS (klass);
  HosCanvasItemClass *canvas_item_class = (HosCanvasItemClass*)klass;
  HosLineClass       *line_class        = (HosLineClass*)klass;

  gobject_class->set_property = spectrum_profile_set_property;
  gobject_class->get_property = spectrum_profile_get_property;

  canvas_item_class->configure = spectrum_profile_configure;

  g_object_class_install_property (gobject_class,
                                   PROP_SPECTRUM,
                                   g_param_spec_object ("spectrum",
							"spectrum",
							"1D spectrum that is drawn by this line plot",
							HOS_TYPE_SPECTRUM,
							G_PARAM_READABLE | G_PARAM_WRITABLE));

  g_object_class_install_property (gobject_class,
				   PROP_ORIENTATION,
				   g_param_spec_enum ("orientation",
						      "orientation",
						      "orientation of the chemical shift axis",
						      HOS_TYPE_ORIENTATION_TYPE,
						      HOS_HORIZONTAL,
						      G_PARAM_READWRITE));

}

static void
spectrum_profile_set_property (GObject      *object,
			       guint         prop_id,
			       const GValue *value,
			       GParamSpec   *pspec)
{
  switch (prop_id)
    {
    case PROP_SPECTRUM:
      spectrum_profile_set_spectrum(HOS_SPECTRUM_PROFILE(object),
				    HOS_SPECTRUM(g_value_get_object(value)));
      break;
    case PROP_ORIENTATION:
      spectrum_profile_set_orientation(HOS_SPECTRUM_PROFILE(object),
				       g_value_get_enum(value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
spectrum_profile_get_property (GObject      *object,
			       guint         prop_id,
			       GValue       *value,
			       GParamSpec   *pspec)
{
  switch (prop_id)
    {
    case PROP_SPECTRUM:
      g_value_set_object(value,
			 G_OBJECT(HOS_SPECTRUM_PROFILE(object)->spectrum));
      break;
    case PROP_ORIENTATION:
      g_value_set_enum(value,
		       HOS_SPECTRUM_PROFILE(object)->orientation);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
spectrum_profile_configure(HosCanvasItem *self)
{
}

static void
sync_points(HosSpectrumProfile *self)
{
  HosSpectrum        *spectrum         = self->spectrum;

  if (HOS_IS_SPECTRUM(spectrum))
    {
      /* FIXME: here is the place to implement different vertical sync modes */
      int np = spectrum_np(spectrum, 0);
      gdouble *x = g_new0(gdouble, np);
      gdouble *y = g_new0(gdouble, np);

      spectrum_traverse_blocking(spectrum);
      gdouble *data = spectrum->buf;
      
      x[0] = spectrum_pt2ppm(spectrum, 0, 0);
      x[1] = spectrum_pt2ppm(spectrum, 0, 1);
      gdouble dx = x[1] - x[0];
      
      int i;
      for (i = 0; i < np; ++i)
	{
	  x[i] = x[0] + (i * dx);
	  y[i] = data[i];
	}

      switch (self->orientation)
	{
	case HOS_HORIZONTAL:
	  line_set_points(HOS_LINE(self), x, y, np);
	  break;
	case HOS_VERTICAL:
	  line_set_points(HOS_LINE(self), y, x, np);
	  break;
	default:
	  g_error("invalid orientation");
	}
      g_free(x);
      g_free(y);
    }
}

/**
 * @brief  set the orientation of the chemical shift axis of #self
 */
void
spectrum_profile_set_orientation(HosSpectrumProfile *self,
				 HosOrientationType orientation)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  if (self->orientation != orientation)
    {
      self->orientation = orientation;
      sync_points(self);
    }
}

/**
 * @brief Set the spectrum associated with #self
 */
void
spectrum_profile_set_spectrum(HosSpectrumProfile *self, HosSpectrum *spectrum)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  g_return_if_fail(HOS_IS_SPECTRUM(spectrum));

  if (self->spectrum != spectrum)
    {
      if (self->spectrum != NULL)
	g_object_unref(self->spectrum);


      if (spectrum != NULL)
	{
	  /*
	   * Be sure to retain only the first dimension;
	   * prevents accidentally traversing a huge spectrum by
	   * associating a multidimensional spectrum with #self.
	   */
	  spectrum = spectrum_cap_ndim(spectrum, 1);
	  
	  self->spectrum = spectrum;
	  g_object_ref(self->spectrum);
	  sync_points(self);
	}
      else
	canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

/** @} */
