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


#ifndef HAVE_COMMON_H
#define HAVE_COMMON_H

/* 
 * The 'CONSTRUCTOR' tag is inserted into function prototypes
 * so that h2def.py will mark them as constructors and allow
 * the caller to own the reference to the return value.
 * The CONSTRUCTOR tag has no influence on C compilation.
 */
#define CONSTRUCTOR

#endif /* not HAVE_COMMON_H */
