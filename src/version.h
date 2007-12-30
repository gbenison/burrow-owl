
#ifndef HAVE_BURROW_VERSION_H
#define HAVE_BURROW_VERSION_H

gboolean burrow_check_version(gint major, gint minor);
char* burrow_version() { return PACKAGE_VERSION; }

#endif /* not HAVE_BURROW_VERSION_H */
