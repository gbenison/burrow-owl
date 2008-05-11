/*
 *  Copyright (C) 2007, 2008 Greg Benison
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*
 * skip_node_t: an implementation of a "skip list",
 * an O(log(n)) lookup and insertion dictionary
 * data structure.
 * The skiplist structure can be safely accessed
 * by one 'writer' thread and any number of 'reader'
 * threads without locking.
 * To the reader threads, the skiplist will always
 * be perceived in a consistent state.
 */

#ifdef DEBUG
#define ASSERT_CONSISTENT(x) {if (!skip_list_self_consistent(x)) skip_list_trap(x);}
#else
#define ASSERT_CONSISTENT(x) {}
#endif

#include <glib.h>
#include <glib/gprintf.h>
#include <assert.h>
#include "skiplist.h"

static void skip_list_trap(skip_list_t* self) {}

typedef struct _skip_node skip_node_t;
struct _skip_node
{
  skip_node_t *next;
  skip_node_t *down;

  gint         key;
  gpointer     data;
};

struct _skip_list
{
  gdouble      prob;
  GPtrArray   *free_list;
  skip_node_t *nodes;
};

static skip_node_t* insert_inner  (skip_list_t* list, skip_node_t* node, gint key, gpointer data);
static skip_node_t* link_new_node (skip_list_t* list, skip_node_t* base, gint key, skip_node_t* down);
static void         print_inner   (skip_node_t* node);
static gpointer     pop_inner     (skip_list_t* list, skip_node_t* node, gint key);
static gpointer     lookup_inner  (skip_node_t* node, gint key);

static skip_node_t*
link_new_node(skip_list_t* list, skip_node_t* base, gint key, skip_node_t* down)
{

#define g_ptr_array_pop(r) (g_ptr_array_remove_index(r, r->len - 1))
  skip_node_t* result = 
    (list->free_list->len == 0) ?
    g_slice_alloc(sizeof(skip_node_t)) :
    g_ptr_array_pop(list->free_list);

  result->next = base->next;
  result->key  = key;
  result->down = down;

  g_atomic_pointer_set(&base->next, result);

  return result;
}

/*
 * An example of the insertion operation:
 * 'increment key 9 with level 3'.
 *
 * pre-state:
 *
 *   1 ----------------------- 15
 *   1 ------- 4 ------------- 15 -------------- 25
 *   1 -- 3 -- 4 -- 8 -- 11 -- 15 -- 21 -- 22 -- 25 -- 29
 *
 * post-state:
 *
 *   1 ----------------- 9 -------- 15
 *   1 ------- 4 ------- 9 -------- 15 -------------- 25
 *   1 -- 3 -- 4 -- 8 -- 9 -- 11 -- 15 -- 21 -- 22 -- 25 -- 29
 *
 */

/*
 * Make 'key' a member of 'self', if it is not already, and
 * set the value corresponding to 'key' to 'data'
 */
void
skip_list_insert(skip_list_t* list, gint key, gpointer data)
{
  insert_inner(list, list->nodes, key, data);
  ASSERT_CONSISTENT(list);
}

/*
 * Returns:
 * a new node that should become the 'down' link for a new
 * node inserted into the level of 'node', or NULL if no such
 * link should be created.
 */
static skip_node_t*
insert_inner(skip_list_t* list, skip_node_t* node, gint key, gpointer data)
{
  if (node == NULL) return NULL;

  g_assert(node->key < key);
  while ((node->next != NULL) && (node->next->key < key))
    node = node->next;

  gboolean is_bottom_row = (node->down == NULL);

  skip_node_t* down = insert_inner(list, node->down, key, data);

  /* 'key' already present? finished. */
  if ((node->next != NULL) && (node->next->key == key))
    {
      node->next->data = data;
      return NULL;
    }

  if (is_bottom_row || down)
    {
      skip_node_t* new_node = link_new_node(list, node, key, down);
      new_node->data = data;
      return (g_random_double_range(0, 1.0) < list->prob) ? new_node : NULL;
    }
  else
    return NULL;

  g_assert_not_reached();
}

skip_list_t*
skip_list_new(gint n_level, gdouble prob)
{
  if (n_level == 0)
    return NULL;

  skip_list_t* result = g_new0(skip_list_t, 1);
  result->prob = prob;
  result->free_list = g_ptr_array_new();

  int i;
  for (i = 0; i < n_level; ++i)
    {

      skip_node_t* node = g_slice_alloc(sizeof(skip_node_t));

      node->key  = -1;
      node->next = NULL;
      node->down = result->nodes;
      node->data = NULL;
      result->nodes = node;

    }
  ASSERT_CONSISTENT(result);
  return result;
}

/*
 * Returns: the key of the second column in 'list', or -1 if
 * 'list' has only one column (e.g. is empty).
 * The second column of 'list' is deleted.
 */
gint
skip_list_pop_first(skip_list_t* list)
{
  skip_node_t* node = list->nodes;
  g_assert(node->key == -1);

  /* Find bottom row */
  while (node->down != NULL)
    node = node->down;

  if (node->next == NULL)
    return -1;

  gint result = node->next->key;
  skip_list_pop(list, result);

  ASSERT_CONSISTENT(list);

  return result;
}

void
skip_list_print_last_row(skip_list_t* list)
{
  if (list == NULL)
    return;

  skip_node_t* node = list->nodes;

  while (node->down != NULL)
    node = node->down;
  
  for (; node != NULL; node = node->next)
    g_printf("%d-", node->key);
  
  g_printf("\n");
}

void
skip_list_print(skip_list_t* list)
{
  if (list == NULL)
    return;

  print_inner(list->nodes);
}

static void
print_inner(skip_node_t* node)
{
  if (node == NULL)
    return;

  skip_node_t* cur;
  for (cur = node; cur != NULL; cur = cur->next)
    {
      g_printf("%2d", cur->key);

      skip_node_t* next = cur->next;
      if (next != NULL)
	{
	  g_printf("-");

	  /* track out to bottom node */
	  skip_node_t* track = cur;
	  while (1)
	    {
	      skip_node_t* down = track->down;
	      if (down == NULL)
		break;
	      track = down;
	    }

	  while (1)
	    {
	      if (track == NULL)
		break;
	      track = track->next;
	      if (track == NULL)
		break;
	      if (track->key == next->key)
		break;
	      g_printf("---");
	    }
	}
    }

  g_printf("\n");
  print_inner(node->down);
}

/*
 * Find the data corresponding to 'key' in 'list' 
 */
gpointer
skip_list_lookup(skip_list_t* list, gint key)
{
  if (list == NULL)
    return NULL;
  //  return lookup_inner(list->nodes, key);

  skip_node_t* node = list->nodes;
  skip_node_t *next, *down;

  while (1)
    {
      next = node->next;
      
      if ((next != NULL) && (next->key <= key))
	node = next;
      else
	{
	  down = node->down;
	  if (down != NULL)
	    node = down;
	  else
	    return (node->key == key) ? node->data : NULL;
	}
    }
}

static gpointer
lookup_inner(skip_node_t* node, gint key)
{
  skip_node_t* down = node->down;
  skip_node_t* next = node->next;

  if ((next != NULL) && (next->key <= key))
    return lookup_inner(next, key);
  else if (down != NULL)
    return lookup_inner(down, key);
  else return (node->key == key) ? node->data : NULL;
}

gboolean
skip_list_self_consistent(skip_list_t* list)
{
  skip_node_t *row;
  gboolean result = TRUE;
  for (row = list->nodes; row != NULL; row = row->down)
    {
      skip_node_t* column;
      for (column = row; column->next != NULL; column = column->next)
	{
	  if (column->down != NULL)
	    {
	      if (column->down->key != column->key)
		result = FALSE;
	      if (column->down->data != column->data)
		result = FALSE;
	    }
	  if (column->next->key <= column->key)
	    result = FALSE;
	}
    }

  return result;
}

/*
 * like skip_list_lookup(), but delete 'key' from 'list'
 * after finding it.
 */
gpointer
skip_list_pop(skip_list_t* list, gint key)
{
  if (list == NULL)
    return NULL;

  gpointer result = pop_inner(list, list->nodes, key);

  ASSERT_CONSISTENT(list);

  return result;

}

static gpointer
pop_inner(skip_list_t* list, skip_node_t* node, gint key)
{
  while ((node->next != NULL) && (node->next->key < key))
    node = node->next;

  skip_node_t* down = g_atomic_pointer_get(&node->down);
  gpointer result = NULL;

  if ((node->next != NULL) && (node->next->key == key))
    {
      result = node->next->data;
      skip_node_t* dead = node->next;
      g_atomic_pointer_set(&node->next,
			   g_atomic_pointer_get(&node->next->next));
      g_atomic_pointer_set(&dead->next, NULL);
      g_atomic_pointer_set(&dead->down, NULL);
      g_atomic_pointer_set(&dead->data, NULL);
      g_atomic_int_set(&dead->key, -1);
      g_ptr_array_add(list->free_list, dead);
    }

  return (down == NULL) ? result : pop_inner(list, down, key);

}

void
skip_list_foreach   (skip_list_t* list,
		     GFunc func,
		     gpointer user_data)
{
  if (list == NULL)
    return;

  skip_node_t* node = list->nodes;

  /* Find bottom row */
  while (node->down != NULL)
    node = node->down;

  /* traverse */
  for (; node = node->next; node != NULL)
    func(node->data, user_data);
}

/*
 * FIXME
 * since this is performance-costly, it would be nice to cache the
 * result somehow
 */
gint
skip_list_length(skip_list_t* list)
{
  if (list == NULL)
    return 0;

  skip_node_t* node = list->nodes;

  gint result = 0;
  /* find bottom row */
  while (node->down != NULL)
    node = node->down;

  while (node->next != NULL)
    {
      ++result;
      node = node->next;
    }
  return result;
}

gboolean
skip_list_is_empty  (skip_list_t* list)
{
  if (list == NULL)
    return TRUE;

  skip_node_t* node = list->nodes;

  /* find bottom row */
  while (node->down != NULL)
    node = node->down;

  return node->next == NULL ? TRUE : FALSE;
}
