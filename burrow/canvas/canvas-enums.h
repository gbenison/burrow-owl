/*
 *  Copyright (C) 2009 Greg Benison
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

#ifndef _HOS_HAVE_CANVAS_ENUMS_H
#define _HOS_HAVE_CANVAS_ENUMS_H

/**
 * @defgroup canvas_enums
 * @brief standard enumerations for the graphical component of burrow-owl
 *
 * @{
 */

#include <glib-object.h>

typedef enum {
  HOS_HORIZONTAL,
  HOS_VERTICAL
} HosOrientationType;

GType hos_orientation_type_get_type (void) G_GNUC_CONST;
#define HOS_TYPE_ORIENTATION_TYPE (hos_orientation_type_get_type())

typedef enum {
  HOS_STRETCH,
  HOS_FIXED,
  HOS_LITERAL
} HosVScalingPolicy;

GType hos_vscaling_policy_get_type (void) G_GNUC_CONST;
#define HOS_TYPE_VSCALING_POLICY (hos_vscaling_policy_get_type())

/** @} */

#endif /* not  _HOS_HAVE_CANVAS_ENUMS_H */
