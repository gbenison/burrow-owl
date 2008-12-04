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

/**
 * @mainpage
 *
 * This manual documents the public C API of burrow-owl; for an overall description, see
 * <a href="http://burrow-owl.sourceforge.net">burrow-owl.sourceforge.net</a>
 *
 * The core of the API is the @ref HosSpectrum object, which represents an NMR spectrum
 * (data and metadata).
 *
 * The interactive graphical features of burrow-owl revolve around the
 * @ref HosCanvas widget, a custom GTK+ widget for displaying spectra and annotations.
 */

/*
 * FIXME
 *
 * Document other bindings:
 * The C API maps fairly directly to the Scheme api, for example:
 * spectrum_traverse(my_spec) --> (spectrum-traverse my-spec)
 */

/* The public C api for the non-graphical part of burrow-owl. */

#include <burrow/spectrum/spectrum.h>
#include <burrow/spectrum/contour.h>
#include <burrow/spectrum/nih.h>
#include <burrow/spectrum/painter.h>
#include <burrow/spectrum/painter_bwps.h>
#include <burrow/spectrum/spectrum.h>
#include <burrow/spectrum/spectrum_convoluted.h>
#include <burrow/spectrum/spectrum_diagonal_project.h>
#include <burrow/spectrum/spectrum_extract.h>
#include <burrow/spectrum/spectrum_integrated.h>
#include <burrow/spectrum/spectrum_project.h>
#include <burrow/spectrum/spectrum_transpose.h>
#include <burrow/spectrum/spectrum_unfold.h>
#include <burrow/spectrum/version.h>

