
#include <glib.h>
#include <glib/gprintf.h>
#include "skiplist.h"

static const jitter_length_min = 20;

static gint
skip_list_insert_random(skip_list_t* list)
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
  static gint last = -1;
  skip_list_t* list = (skip_list_t*)data;
  while (1)
    {
      if ((skip_list_length(list) > jitter_length_min) && (last >= 0))
	skip_list_pop(list, last);
      last = skip_list_insert_random(list);
    }
}

void
print_node(gpointer data, gchar* fmt)
{
  g_printf(fmt, GPOINTER_TO_INT(data));
}

void
assert_present(gpointer data, skip_list_t *target)
{
  g_assert(skip_list_has_key(target, GPOINTER_TO_INT(data)));
}

int
main()
{
  g_type_init();
  if (!g_thread_supported ()) g_thread_init (NULL);

  g_print("Testing skip lists");

  skip_list_t *list  = skip_list_new(8, 0.65);
  skip_list_t *list2 = skip_list_new(6, 0.3);

  /* for-each test */
  int i;
  for (i = 0; i < 50; ++i)
    {
      gint key = skip_list_insert_random(list);
      skip_list_insert(list2, key, GINT_TO_POINTER(key));
    }
  skip_list_foreach (list, (GFunc)assert_present, list2);
  g_print("...");

  for (i = 0; i < 50; ++i)
    {
      gint next = skip_list_insert_random(list);
      gint popped = skip_list_pop_first(list);
    }
  g_assert(skip_list_self_consistent(list));

  /* pop test */
  for (i = 0; i < 500; ++i)
    {
      gint next   = skip_list_insert_random(list);
      gint popped = GPOINTER_TO_INT(skip_list_pop(list, next));
      g_assert(popped == next);
    }
  g_assert(skip_list_self_consistent(list));
  g_print("...");

  /* lookup test */
  for (i = 0; i < 500; ++i)
    {
      gint next   = skip_list_insert_random(list);
      gint looked = GPOINTER_TO_INT(skip_list_lookup(list, next));
      g_assert(looked == next);
    }
  g_assert(skip_list_self_consistent(list));
  g_print("...");

  g_print("OK\n");

  g_print("Testing skiplist thread safety");

  GError *error = NULL;
  g_thread_create((GThreadFunc)skip_list_jitter, list, FALSE, &error);
  g_assert(error == NULL);

  for (i = 0; i < 500; ++i)
    {
      g_usleep(10000);
      g_assert(skip_list_self_consistent(list));
      g_assert(skip_list_length(list) > (jitter_length_min - 5));
      g_assert(skip_list_length(list) < (jitter_length_min + 5));
      if ((i % 25) == 0) g_print(".");
    }

  g_print("OK\n");

  return 0;
}

