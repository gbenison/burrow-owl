/*
 *  Copyright (C) 2012 Greg Benison
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

#ifndef _HOS_HAVE_POINT_CACHE_H
#define _HOS_HAVE_POINT_CACHE_H

#include "spectrum_priv.h"
#include <glib.h>

void    point_cache_store (gpointer data, gsize idx, gdouble value);

/*
 * Fetch value previously stored by point_cache_store, process with DATUM_ENSURE_KNOWN(x), return x
 * or, if no cached value is available, return DATUM_UNKNOWN_VALUE
 */
gdouble point_cache_fetch (gpointer data, gsize idx);

#endif
