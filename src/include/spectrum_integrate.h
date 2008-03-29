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

#ifndef HAVE_SPECTRUM_INTEGRATE_H
#define HAVE_SPECTRUM_INTEGRATE_H

#include "include/spectrum.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_INTEGRATED              (hos_spectrum_integrated_get_type())
#define HOS_SPECTRUM_INTEGRATED(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegrated))
#define HOS_SPECTRUM_INTEGRATED_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedClass))
#define HOS_IS_SPECTRUM_INTEGRATED(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_INTEGRATED))
#define HOS_IS_SPECTRUM_INTEGRATED_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_INTEGRATED))
#define HOS_SPECTRUM_INTEGRATED_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_INTEGRATED, HosSpectrumIntegratedClass))

typedef struct _HosSpectrumIntegrated       HosSpectrumIntegrated;
typedef struct _HosSpectrumIntegratedClass  HosSpectrumIntegratedClass;

struct _HosSpectrumIntegrated
{
  HosSpectrum parent_instance;
};

struct _HosSpectrumIntegratedClass
{
  HosSpectrumClass parent_class;
};

/* 
 * The 'CONSTRUCTOR' tag is inserted into function prototypes
 * so that h2def.py will mark them as constructors and allow
 * the caller to own the reference to the return value.
 * The CONSTRUCTOR tag has no influence on C compilation.
 */
#define CONSTRUCTOR  /* empty */

HosSpectrum* CONSTRUCTOR spectrum_integrate (HosSpectrum* self);

GType hos_spectrum_integrated_get_type (void);

G_END_DECLS

#endif /* not HAVE_SPECTRUM_INTEGRATE_H */



