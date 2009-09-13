
#ifndef _HAVE_TEST_UTILS_H
#define _HAVE_TEST_UTILS_H
#include "burrow/spectrum.h"

extern guint monitor_interval;
GThread* spectrum_monitor(HosSpectrum *self);

#endif /* not _HAVE_TEST_UTILS_H */

