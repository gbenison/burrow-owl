
#ifndef HAVE_BURROW_VERSION_H
#define HAVE_BURROW_VERSION_H

#include <glib.h>

gboolean burrow_check_version (gint major, gint minor);
char*    burrow_version       (void);

#endif /* not HAVE_BURROW_VERSION_H */
