
#ifndef HAVE_CONTOUR_FSM
#define HAVE_CONTOUR_FSM

#include <glib.h>

struct hos_point {gdouble x, y;};

void test_method(void*, struct hos_point*, const int, const int, gboolean);


void contour_fsm(double *buf_base,
		 const int x_offset,
		 const int y_offset,
		 const int stride,
		 const int nx,
		 const int ny,
		 double *levels,
		 const int n_levels,
		 void (*trace_method)(void* data, struct hos_point*, const gint n_point, gint level, gboolean closed),
		 void *trace_data,
		 gulong *cancellation);

#endif /* HAVE_CONTOUR_FSM */

