
#include <glib.h>
#include <glib/gprintf.h>
#include "skiplist.h"

skip_node_t* list;

static gint
skip_list_insert_random(skip_node_t* list)
{
  gint result = g_random_int_range(0, 100);
  skip_list_insert(list, result, GINT_TO_POINTER(result));
  return result;
}

/*
 * At random, increment one key and decrement another
 */
static void
skip_list_jitter(gpointer data)
{
  while (1)
    {
      g_usleep(g_random_int_range(10000, 100000));
      skip_list_insert_random(list);
    }
}

void
print_node(gpointer data, gchar* fmt)
{
  g_printf(fmt, GPOINTER_TO_INT(data));
}

int
main()
{
  list = skip_list_new(8, 0.65);

  g_thread_init(NULL);

  g_printf("========== Skip list test ===========\n");

  int i;
  for (i = 0; i < 50; ++i)
    skip_list_insert_random(list);

  skip_list_print(list);

  for (i = 0; i < 50; ++i)
    {
      gint next = skip_list_insert_random(list);
      gint popped = skip_list_pop_first(list);
      g_printf("Inserted %d, popped %d\n", next, popped);
      skip_list_print(list);
    }

  g_printf("==== for-each test =====\n");
  skip_list_foreach (list, (GFunc)print_node, "--> %d");

  g_printf("\n====== pop test =======\n");
  for (i = 0; i < 500; ++i)
    {
      gint next   = skip_list_insert_random(list);
      gint popped = GPOINTER_TO_INT(skip_list_pop(list, next));
      g_assert(popped == next);
    }

  g_printf("\n====== lookup test =======\n");
  for (i = 0; i < 500; ++i)
    {
      gint next   = skip_list_insert_random(list);
      gint looked = GPOINTER_TO_INT(skip_list_lookup(list, next));
      g_assert(looked == next);
    }

  g_printf("======== start multithread test =============\n");
  g_printf("DISABLED\n");

  /*  g_thread_create((GThreadFunc)skip_list_jitter, NULL, FALSE, NULL); */

  return 0;
}

