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

#ifndef _HAVE_SPECTRUM_PROFILE_H
#define _HAVE_SPECTRUM_PROFILE_H

#include <glib-object.h>
#include "burrow/spectrum.h"
#include "canvas-enums.h"
#include "line.h"

typedef struct _HosSpectrumProfile       HosSpectrumProfile;
typedef struct _HosSpectrumProfileClass  HosSpectrumProfileClass;

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_PROFILE              (hos_spectrum_profile_get_type())
#define HOS_SPECTRUM_PROFILE(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_PROFILE, HosSpectrumProfile))
#define HOS_SPECTRUM_PROFILE_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_PROFILE, HosSpectrumProfileClass))
#define HOS_IS_SPECTRUM_PROFILE(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_PROFILE))
#define HOS_IS_SPECTRUM_PROFILE_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_PROFILE))
#define HOS_SPECTRUM_PROFILE_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_PROFILE, HosSpectrumProfileClass))


struct _HosSpectrumProfileClass
{
  HosLineClass parent_class;
};

struct _HosSpectrumProfile
{
  HosLine parent_instance;

  HosOrientationType  orientation;
  HosSpectrum        *spectrum;

  HosVScalingPolicy   vpolicy;
  gdouble             voffset;
  gdouble             vrange;
  gdouble             vzoom;
};

void spectrum_profile_set_orientation (HosSpectrumProfile *self,
				       HosOrientationType  orientation);
void spectrum_profile_set_spectrum    (HosSpectrumProfile *self,
				       HosSpectrum        *spectrum);

void spectrum_profile_set_vpolicy     (HosSpectrumProfile *self,
				       HosVScalingPolicy   vpolicy);
void spectrum_profile_set_voffset     (HosSpectrumProfile *self,
				       gdouble             voffset);
void spectrum_profile_set_vrange      (HosSpectrumProfile *self,
				       gdouble             vrange);
void spectrum_profile_set_vzoom       (HosSpectrumProfile *self,
				       gdouble             vzoom);


GType hos_spectrum_profile_get_type (void);


G_END_DECLS


#endif  /* HAVE_SPECTRUM_PROFILE_H */




