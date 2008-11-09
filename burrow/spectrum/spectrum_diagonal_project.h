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

#ifndef HAVE_SPECTRUM_DIAGONAL_H
#define HAVE_SPECTRUM_DIAGONAL_H

#include "spectrum.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_DIAGONAL              (hos_spectrum_diagonal_get_type())
#define HOS_SPECTRUM_DIAGONAL(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_DIAGONAL, HosSpectrumDiagonal))
#define HOS_SPECTRUM_DIAGONAL_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_DIAGONAL, HosSpectrumDiagonalClass))
#define HOS_IS_SPECTRUM_DIAGONAL(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_DIAGONAL))
#define HOS_IS_SPECTRUM_DIAGONAL_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_DIAGONAL))
#define HOS_SPECTRUM_DIAGONAL_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_DIAGONAL, HosSpectrumDiagonalClass))

typedef struct _HosSpectrumDiagonal       HosSpectrumDiagonal;
typedef struct _HosSpectrumDiagonalClass  HosSpectrumDiagonalClass;

struct _HosSpectrumDiagonal
{
  HosSpectrum parent_instance;
};

struct _HosSpectrumDiagonalClass
{
  HosSpectrumClass parent_class;
};

GType hos_spectrum_diagonal_get_type (void);

HosSpectrum* CONSTRUCTOR spectrum_diagonal_project (HosSpectrum* spec);

G_END_DECLS

#endif /* not HAVE_SPECTRUM_DIAGONAL_H */







