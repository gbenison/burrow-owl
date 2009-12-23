
#ifndef HAVE_UTILS_H
#define HAVE_UTILS_H

#define GREATER_OF(a, b) ((a) > (b) ? (a) : (b))
#define LESSER_OF(a, b)  ((a) > (b) ? (b) : (a))
#define G_OBJECT_UNREF_AND_CLEAR(_o_) { if (G_IS_OBJECT(_o_)) { g_object_unref(_o_); _o_ = NULL; } }

#endif /* not HAVE_UTILS_H */

