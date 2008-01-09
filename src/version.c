/*
 *  Copyright (C) 2007 Greg Benison
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

#include <glib.h>

/*
 * returns:
 * false: major > binary major version
 * true:  major < binary major version
 * false: major == binary major version && minor > binary minor version
 * true:  major == binary major version && minor <= binary minor version
 */
gboolean
burrow_check_version(gint major, gint minor)
{
  gchar** elems = g_strsplit(PACKAGE_VERSION, ".", 2);
  gint binary_major = 0;
  gint binary_minor = 0;
  if (elems[0])
    binary_major = g_ascii_strtoll(elems[0], NULL, 0);
  if (elems[1])
    binary_minor = g_ascii_strtoll(elems[1], NULL, 0);
  if (major < binary_major) return TRUE;
  if (major > binary_major) return FALSE;

  if (minor > binary_minor) return FALSE;

  return TRUE;

}

char* burrow_version() { return PACKAGE_VERSION; }
