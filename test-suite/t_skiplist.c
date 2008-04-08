
#include <glib.h>
#include <glib/gprintf.h>
#include "skiplist.h"

skip_node_t* list;

/*
 * At random, increment one key and decrement another
 */
static void
skip_list_jitter(gpointer data)
{
  while (1)
    {
      g_usleep(g_random_int_range(10000, 100000));
      skip_list_insert(list, g_random_int_range(0, 100));
    }
}

int
main()
{
  list = skip_list_new(8, 0.65);

  g_thread_init(NULL);

  g_printf("========== Skip list test ===========\n");

  int i;
  for (i = 0; i < 50; ++i)
    skip_list_insert(list, g_random_int_range(0, 100));

  skip_list_print(list);

  for (i = 0; i < 50; ++i)
    {
      gint next = g_random_int_range(0, 100);
      skip_list_insert(list, next);
      gint popped = skip_list_pop(list);
      g_printf("Inserted %d, popped %d\n", next, popped);
      skip_list_print(list);
    }

  g_printf("\n\n\n");

  g_printf("======== start multithread test =============\n");
  g_printf("DISABLED\n");

  /*  g_thread_create((GThreadFunc)skip_list_jitter, NULL, FALSE, NULL); */

  return 0;
}

