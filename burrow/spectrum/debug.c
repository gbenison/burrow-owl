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

#include <glib.h>
#include <glib/gprintf.h>

gint burrow_debug_lvl = -1;

void
burrow_debug_init()
{
  if (burrow_debug_lvl < 0)
    {
      const gchar* debug_env = g_getenv("DEBUG");
      if (debug_env == NULL)
	burrow_debug_lvl = 0;
      else
	burrow_debug_lvl = g_ascii_strtoll(debug_env, NULL, 0);
    }
}

void
burrow_debug_print(gchar *str)
{
  /* FIXME no... push to queue! */
  g_print(str);
}
