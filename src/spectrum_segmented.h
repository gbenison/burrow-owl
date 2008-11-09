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

#ifndef HAVE_SPECTRUM_SEGMENTED
#define HAVE_SPECTRUM_SEGMENTED

#include "spectrum.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_SEGMENTED              (hos_spectrum_segmented_get_type())
#define HOS_SPECTRUM_SEGMENTED(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmented))
#define HOS_SPECTRUM_SEGMENTED_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmentedClass))
#define HOS_IS_SPECTRUM_SEGMENTED(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM_SEGMENTED))
#define HOS_IS_SPECTRUM_SEGMENTED_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM_SEGMENTED))
#define HOS_SPECTRUM_SEGMENTED_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM_SEGMENTED, HosSpectrumSegmentedClass))

typedef struct _HosSpectrumSegmented       HosSpectrumSegmented;
typedef struct _HosSpectrumSegmentedClass  HosSpectrumSegmentedClass;

struct _HosSpectrumSegmented
{
  HosSpectrum parent_instance;
  
  gpointer    traversal_env;
};

struct _HosSpectrumSegmentedClass
{
  HosSpectrumClass parent_class;

  void     (*idx2segment)  (gpointer env, guint *idx, gint *segid, gint *pt);
  void     (*read_segment) (gpointer env, guint segid, gdouble *buf);
};

void spectrum_segmented_set_segment_size  (HosSpectrumSegmented *self, guint size);
void spectrum_segmented_set_cache_size    (HosSpectrumSegmented *self, guint size);

GType hos_spectrum_segmented_get_type (void);

G_END_DECLS


#endif /* not HAVE_SPECTRUM_SEGMENTED */


