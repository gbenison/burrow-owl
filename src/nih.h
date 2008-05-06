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

#ifndef _HAVE_NIH_H
#define _HAVE_NIH_H

#include "spectrum_segmented.h"



G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_NIH              (hos_spectrum_nih_get_type())
#define HOS_SPECTRUM_NIH(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_NIH, HosSpectrumNih))
#define HOS_SPECTRUM_NIH_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_NIH, HosSpectrumNihClass))
#define HOS_IS_SPECTRUM_NIH(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_NIH))
#define HOS_IS_SPECTRUM_NIH_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_NIH))
#define HOS_SPECTRUM_NIH_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_NIH, HosSpectrumNihClass))

typedef struct _HosSpectrumNih       HosSpectrumNih;
typedef struct _HosSpectrumNihClass  HosSpectrumNihClass;

struct _HosSpectrumNih
{
  HosSpectrumSegmented parent_instance;

  gchar *fname;
};

struct _HosSpectrumNihClass
{
  HosSpectrumSegmentedClass parent_class;
};

GType hos_spectrum_nih_get_type (void);

G_END_DECLS


#endif /* not _HAVE_NIH_H */
