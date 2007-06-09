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

/*
 * returns:
 * 0: machine has big-endian byte order
 * 1: machine has little-endian byte order
 */
int
machine_little_endian()
{
   static long int x = 0x1;
   static char* px = (char*)&x;
   
   return (px[0] == 0) ? 0 : 1;
}

/*
 * perform 4-byte endian swap on 'data',
 * an array of 'n_elem' 4-byte elements;
 * destructive in-place
 */
void
endian_swap4(void* data, int n_elem)
{
  int i;
  char* ptr = data;
  char tmp;
#define SWAP(a, b) {tmp=a; a=b; b=tmp;}
  for (i = 0; i < n_elem; ++i)
    {
      SWAP(ptr[3], ptr[0]);
      SWAP(ptr[2], ptr[1]);
      ptr += 4;
    }
}

