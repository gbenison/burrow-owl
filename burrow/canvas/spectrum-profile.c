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
  PROP_ORIENTATION,  /**< orientation of chemical shift axis */
  PROP_VPOLICY,      /**< How #voffset, #vrange, and #vzoom
                          determine vertical size */
  PROP_VOFFSET,
  PROP_VRANGE,
  PROP_VZOOM
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

static void spectrum_profile_canvas_world_configure (HosCanvasItem *self,
						     HosCanvas     *canvas);
static void spectrum_profile_configure      (HosCanvasItem *self);
static void sync_points                     (HosSpectrumProfile *self);

G_DEFINE_TYPE (HosSpectrumProfile, hos_spectrum_profile, HOS_TYPE_LINE)

static void
hos_spectrum_profile_init(HosSpectrumProfile *self)
{
  self->vzoom = 1;
  self->vrange = 1;
  self->voffset = 0;
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
  canvas_item_class->canvas_world_configure =
    spectrum_profile_canvas_world_configure;

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

  g_object_class_install_property (gobject_class,
				   PROP_VPOLICY,
				   g_param_spec_enum ("vpolicy",
						      "vpolicy",
						      "policy for intensity scaling",
						      HOS_TYPE_VSCALING_POLICY,
						      HOS_STRETCH,
						      G_PARAM_READWRITE));

#define INSTALL_DOUBLE_PROPERTY(_prop_, name, blurb, _default)   {g_object_class_install_property(gobject_class, _prop_, g_param_spec_double (name, name, blurb, -G_MAXDOUBLE, G_MAXDOUBLE, _default, G_PARAM_READABLE | G_PARAM_WRITABLE));}

  INSTALL_DOUBLE_PROPERTY(PROP_VOFFSET, "voffset", "intensity scaling 'offset' parameter", 0);
  INSTALL_DOUBLE_PROPERTY(PROP_VRANGE, "vrange", "intensity scaling 'range' parameter", 1);
  INSTALL_DOUBLE_PROPERTY(PROP_VZOOM, "vzoom", "intensity scaling 'zoom' parameter", 1);

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
    case PROP_VPOLICY:
      spectrum_profile_set_vpolicy(HOS_SPECTRUM_PROFILE(object),
				   g_value_get_enum(value));
      break;
    case PROP_VOFFSET:
      spectrum_profile_set_voffset(HOS_SPECTRUM_PROFILE(object),
				   g_value_get_double(value));
      break;
    case PROP_VRANGE:
      spectrum_profile_set_vrange(HOS_SPECTRUM_PROFILE(object),
				  g_value_get_double(value));
      break;
    case PROP_VZOOM:
      spectrum_profile_set_vzoom(HOS_SPECTRUM_PROFILE(object),
				 g_value_get_double(value));
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
    case PROP_VPOLICY:
      g_value_set_enum(value,
		       HOS_SPECTRUM_PROFILE(object)->vpolicy);
      break;
    case PROP_VOFFSET:
      g_value_set_double(value,
			 HOS_SPECTRUM_PROFILE(object)->voffset);
      break;
    case PROP_VRANGE:
      g_value_set_double(value,
			 HOS_SPECTRUM_PROFILE(object)->vrange);
      break;
    case PROP_VZOOM:
      g_value_set_double(value,
			 HOS_SPECTRUM_PROFILE(object)->vzoom);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
spectrum_profile_canvas_world_configure(HosCanvasItem *self, HosCanvas *canvas)
{
  sync_points(HOS_SPECTRUM_PROFILE(self));

  HosCanvasItemClass *parent_class =
    HOS_CANVAS_ITEM_CLASS(hos_spectrum_profile_parent_class);
  
  if (parent_class->canvas_world_configure)
    (parent_class->canvas_world_configure)(self, canvas);
}

static void
spectrum_profile_configure(HosCanvasItem *self)
{
}

static void
sync_points(HosSpectrumProfile *self)
{
  HosSpectrum  *spectrum = self->spectrum;
  HosCanvas    *canvas;

  gdouble actual_offset = 0;
  gdouble actual_range = 1;
  gdouble actual_zoom = 1;

  /* Y = I * Z / R + O */

  switch (self->vpolicy)
    {
    case HOS_STRETCH:
      canvas = HOS_CANVAS_ITEM(self)->canvas;
      if (HOS_IS_CANVAS(canvas))
	{
	  gdouble span;

	  switch (self->orientation)
	    {
	    case HOS_HORIZONTAL:
	      span = (canvas->yn - canvas->y1) / canvas->zoom;
	      actual_offset = self->voffset * canvas_view_height(canvas);
	      canvas_view2world(canvas, NULL, &actual_offset);
	      break;
	    case HOS_VERTICAL:
	      span = (canvas->xn - canvas->x1) / canvas->zoom;
	      actual_offset = self->voffset * canvas_view_width(canvas);
	      canvas_view2world(canvas, &actual_offset, NULL);
	      break;
	    default:
	      g_warn("orientation value is hinky");
	    }
	  actual_zoom   = self->vzoom;
	  actual_range  = self->vrange / span;
	}
      break;
    case HOS_STICKY:
      canvas = HOS_CANVAS_ITEM(self)->canvas;
      if (HOS_IS_CANVAS(canvas))
	{
	  gdouble span =
	    ((self->orientation == HOS_VERTICAL) ? 
	     canvas->xn - canvas->x1 :
	     canvas->yn - canvas->y1)
	    / canvas->zoom;
	  
	  actual_zoom   = self->vzoom;
	  actual_offset = self->voffset;
	  actual_range  = self->vrange / span;
	}
      break;
    case HOS_LITERAL:
      /* nothing to do */
      break;
    case HOS_FIXED:
      actual_offset = self->voffset;
      actual_range  = self->vrange;
      actual_zoom   = self->vzoom;
      break;
    default:
      g_warn("invalid vpolicy");
    }

  if (HOS_IS_SPECTRUM(spectrum))
    {
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
	  y[i] = data[i] * actual_zoom / actual_range + actual_offset;
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

/**
 *
 * @brief set vertical scaling behavior
 *
 * @HosSpectrumProfile #self can adjust its intensity scale by
 * translating the raw spectrum intensity into a world coordinate value.
 *
 * @HOS_LITERAL: intensity values are interpreted as literal world coordinates.
 *
 * @HOS_FIXED:   calculate world coordinate according to:
 *               world = intensity / vrange * vzoom + voffset
 *               There is no automatic scaling with canvas world configuration.
 *
 * @HOS_STRETCH:(default)
 *               useful for 'sidebar'-type 1D spectrum panels.
 *               The intensity interval 'vrange' maps to the world coordinate
 *               interval equal to the canvas span * vzoom.
 *               voffset is interpreted as the view coordinate in (0, 1)
 *               mapping to intensity = 0.
 *
 * @HOS_STICKY:  The world coordinate 'voffset' maps to intensity = 0.
 *               The intensity interval 'vrange' maps to the world coordinate
 *               interval equal to the canvas span * vzoom.
 *               The canvas world coordinate 'voffset' maps to intensity = 0.
 *               Useful for a 'spectrum stuck to a cursor'-type display.
 */
void
spectrum_profile_set_vpolicy (HosSpectrumProfile *self,
			      HosVScalingPolicy   vpolicy)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  if (self->vpolicy != vpolicy)
    {
      self->vpolicy = vpolicy;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

void
spectrum_profile_set_voffset (HosSpectrumProfile *self,
			      gdouble             voffset)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  if (self->voffset != voffset)
    {
      self->voffset = voffset;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

void
spectrum_profile_set_vrange (HosSpectrumProfile *self,
			     gdouble             vrange)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  if (self->vrange != vrange)
    {
      self->vrange = vrange;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}

void
spectrum_profile_set_vzoom (HosSpectrumProfile *self,
			    gdouble             vzoom)
{
  g_return_if_fail(HOS_IS_SPECTRUM_PROFILE(self));
  if (self->vzoom != vzoom)
    {
      self->vzoom = vzoom;
      canvas_item_configure(HOS_CANVAS_ITEM(self));
    }
}



/** @} */
