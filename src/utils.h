
#ifndef HAVE_UTILS_H
#define HAVE_UTILS_H

#define G_OBJECT_UNREF_AND_CLEAR(_o_) { if ((_o_) != NULL) { g_object_unref(_o_); _o_ = NULL; } }

#endif /* not HAVE_UTILS_H */

