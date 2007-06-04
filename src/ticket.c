/*
 *  Copyright (C) 2006 Greg Benison
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

static gulong ticket_counter = 1;
static GMutex *ticket_lock = NULL;

gulong
ticket_grab()
{

  gulong result;

  if (!ticket_lock)
    ticket_lock = g_mutex_new();

  g_mutex_lock(ticket_lock);
  result = ticket_counter;

  ++ticket_counter;
  g_mutex_unlock(ticket_lock);

  return result;
}

