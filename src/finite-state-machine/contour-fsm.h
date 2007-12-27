
#ifndef HAVE_CONTOUR_FSM
#define HAVE_CONTOUR_FSM

#include <glib.h>

struct hos_point {gdouble x, y;};

void test_method(void*, struct hos_point*, const int, const int, gboolean);

typedef void (*trace_func_t)(void* data, struct hos_point* points,
			   const gint n_point,
			   gint level,
			   gboolean closed);

typedef struct _fsm_state fsm_state_t;

gboolean contour_fsm(fsm_state_t* state);
void fsm_state_free(fsm_state_t* state);
fsm_state_t*
fsm_state_init(double* buffer,
	       int nx,
	       int ny,
	       int x_first,
	       int x_last,
	       int y_first,
	       int y_last,
	       double* levels,
	       int n_levels,
	       trace_func_t trace_method,
	       gpointer trace_data);

#endif /* HAVE_CONTOUR_FSM */

