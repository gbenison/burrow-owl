
#include "test-utils.h"

guint monitor_interval = 500000;

static void
monitor_func(HosSpectrum *self)
{
  /* FIXME perhaps add timer? */
  while (1)
    {
      if (spectrum_is_ready(self))
	break;
      g_usleep(monitor_interval);
      g_print(".");
    }
}

GThread*
spectrum_monitor(HosSpectrum *self)
{
  /* start report thread */
  GError *error = NULL;
  GThread *result =
    g_thread_create((GThreadFunc)monitor_func,
		    self,
		    TRUE,
		    &error);
  g_assert(error == NULL);

  return result;
}

