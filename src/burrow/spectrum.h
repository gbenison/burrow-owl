/*
 *  Copyright (C) 2005, 2006 Greg Benison
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

#ifndef _HOS_HAVE_SPECTRUM_H
#define _HOS_HAVE_SPECTRUM_H

#include <glib.h>
#include <glib-object.h>

/* peace in our time */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define HOS_TYPE_SPECTRUM              (hos_spectrum_get_type())
#define HOS_SPECTRUM(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM, HosSpectrum))
#define HOS_SPECTRUM_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_SPECTRUM, HosSpectrumClass))
#define HOS_IS_SPECTRUM(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_SPECTRUM))
#define HOS_IS_SPECTRUM_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_SPECTRUM))
#define HOS_SPECTRUM_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_SPECTRUM, HosSpectrumClass))
					
typedef struct _HosSpectrum       HosSpectrum;
typedef struct _HosSpectrumClass  HosSpectrumClass;

/* spectrum status */
enum
{
  NO_STATUS = 0,
  LATENT,
  TRAVERSING,
  COMPLETE
};

struct _HosSpectrum
{

  GObject parent_instance;

  gdouble *buf;

  gboolean negated;
  guint status;
  gboolean *alive;

  GList *projections;
  GList *dimensions;

  GMutex *traverse_lock;
  GMutex *status_lock;

};

struct _HosSpectrumClass
{
  GObjectClass parent_class;
  void(*ready)(HosSpectrum *spec);
};

/* 
 * The 'CONSTRUCTOR' tag is inserted into function prototypes
 * so that h2def.py will mark them as constructors and allow
 * the caller to own the reference to the return value.
 * The CONSTRUCTOR tag has no influence on C compilation.
 */
#define CONSTRUCTOR  /* empty */

gsize spectrum_np(HosSpectrum* spec, guint dim);
gsize spectrum_ndim(HosSpectrum* spec);
gdouble spectrum_sw(HosSpectrum* spec, guint dim);
gdouble spectrum_sw_ppm(HosSpectrum* spec, guint dim);
gdouble spectrum_sf(HosSpectrum* spec, guint dim);
gdouble spectrum_orig(HosSpectrum* spec, guint dim);
gdouble spectrum_giro(HosSpectrum* spec, guint dim);
gdouble spectrum_orig_ppm(HosSpectrum* spec, guint dim);
gdouble spectrum_giro_ppm(HosSpectrum* spec, guint dim);
gdouble spectrum_ppm2pt(HosSpectrum* spec, guint dim, gdouble ppm);
gdouble spectrum_pt2ppm(HosSpectrum* spec, guint dim, gdouble pt);

HosSpectrum* CONSTRUCTOR spectrum_project(HosSpectrum* self);
HosSpectrum* CONSTRUCTOR spectrum_project_pt(HosSpectrum* self, guint pt);
HosSpectrum* CONSTRUCTOR spectrum_project_ppm(HosSpectrum* self, gdouble ppm);
HosSpectrum* CONSTRUCTOR spectrum_transpose(HosSpectrum* self, guint dim);
HosSpectrum* CONSTRUCTOR spectrum_extract(HosSpectrum* spec, gdouble A, gdouble B);
HosSpectrum* CONSTRUCTOR spectrum_extract_ppm(HosSpectrum* spec, gdouble A, gdouble B);
HosSpectrum* CONSTRUCTOR spectrum_diagonal_project(HosSpectrum* spec);
HosSpectrum* CONSTRUCTOR spectrum_convolute(HosSpectrum *A, HosSpectrum *B);
HosSpectrum* CONSTRUCTOR spectrum_integrate(HosSpectrum* self);
HosSpectrum* CONSTRUCTOR spectrum_cache(HosSpectrum* self);

gdouble spectrum_get_ranked(HosSpectrum *spec, guint n);
gdouble spectrum_get_max(HosSpectrum *spec);
gdouble spectrum_get_min(HosSpectrum *spec);
gdouble spectrum_get_percentile(HosSpectrum *spec, gdouble percentile);
gdouble spectrum_mean(HosSpectrum *spec);
gdouble spectrum_stddev(HosSpectrum *spec);
gdouble spectrum_peek(HosSpectrum *spec, guint idx);

gdouble* spectrum_traverse(HosSpectrum *spec);
gdouble* spectrum_traverse_blocking(HosSpectrum *spec);

GType hos_spectrum_get_type (void);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* not  _HOS_HAVE_SPECTRUM_H  */





