
#ifndef _HAVE_TEST_UTILS_H
#define _HAVE_TEST_UTILS_H
#include <math.h>
#include "burrow/spectrum.h"

#define IS_ABOUT_EQUAL(a, b) (fabs((a)-(b)) < 0.000001)

extern guint monitor_interval;
GThread* spectrum_monitor(HosSpectrum *self);

#endif /* not _HAVE_TEST_UTILS_H */

