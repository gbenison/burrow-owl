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

#include <cairo.h>

void
cairo_shape_x(cairo_t* cr, int cross_size)
{
  cairo_rel_move_to (cr, -cross_size, -cross_size);
  cairo_rel_line_to (cr, cross_size * 2, cross_size * 2);
  cairo_rel_move_to (cr, -cross_size, -cross_size);
  cairo_rel_move_to (cr, cross_size, -cross_size);
  cairo_rel_line_to (cr, -cross_size * 2, cross_size * 2);
  cairo_stroke(cr);
}

void
cairo_shape_hourglass(cairo_t* cr, int width, int height)
{
  int curve_height = height * 0.5;

  cairo_move_to  (cr, -width, -height);
  cairo_curve_to (cr,
		  -width, curve_height,
		   width, -curve_height,
		   width, height);
  cairo_line_to  (cr, -width, height);
  cairo_curve_to (cr,
		  -width, -curve_height,
		  width, curve_height,
		  width, -height);
  cairo_close_path (cr);

  cairo_pattern_t* pat = cairo_pattern_create_linear(0, -height, 0, height);
  cairo_pattern_add_color_stop_rgb (pat, 0, 0, 0, 0);
  cairo_pattern_add_color_stop_rgb (pat, 1, 1, 1, 1);
  cairo_set_source                 (cr, pat);
  cairo_fill_preserve              (cr);
  cairo_set_source_rgb             (cr, 0.2, 0.2, 0.2);
  cairo_set_line_width             (cr, 4);
  cairo_stroke_preserve            (cr);
  cairo_set_source_rgb             (cr, 0.8, 0.8, 0.8);
  cairo_set_line_width             (cr, 2);
  cairo_stroke                     (cr);

  //  cairo_set_source_rgba(cr, 0.9, 0.2, 0.2, 1.0);
  cairo_set_line_cap   (cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_line_width (cr, height * 0.2);

  int bar_width = width * 1.15;

  cairo_move_to (cr, -bar_width, height);
  cairo_line_to (cr, bar_width, height);
  cairo_move_to (cr, -bar_width, -height);
  cairo_line_to (cr, bar_width, -height);
  cairo_stroke     (cr);
		      
}
