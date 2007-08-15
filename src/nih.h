
#ifndef _HAVE_NIH_H
#define _HAVE_NIH_H

#include "hosspectrum.h"
#include "burrow.h"

/*
 * The NIH spectrum type--
 * Constructors for one type of block spectrum
 */
HosSpectrum* CONSTRUCTOR spectrum_nih_from_file(gchar* fname);
HosSpectrum* CONSTRUCTOR spectrum_nih_2d_from_file(gchar* fname);

void spectrum_nih_unfold(HosSpectrum *self,
			 guint dim,
			 guint downfield,
			 guint upfield,
			 gboolean negate_on_fold);

#endif /* not _HAVE_NIH_H */
