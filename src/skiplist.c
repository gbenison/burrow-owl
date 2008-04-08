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

#include <glib.h>
#include <glib/gprintf.h>
#include <assert.h>
#include "skiplist.h"

struct _skip_node
{
  skip_node_t *next;
  skip_node_t *down;

  gdouble      prob;
  gint         key;
};

static skip_node_t* insert_inner  (skip_node_t* node, gint key);
static skip_node_t* link_new_node (skip_node_t* base, gint key, skip_node_t* down);


static skip_node_t*
link_new_node(skip_node_t* base, gint key, skip_node_t* down)
{
  skip_node_t* result = g_slice_alloc(sizeof(skip_node_t));

  result->next = base->next;
  result->key = key;
  result->down = down;
  result->prob = base->prob;

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
 * Make 'key' a member of 'self', if it is not already
 */
void
skip_list_insert(skip_node_t* self, gint key)
{
  insert_inner(self, key);
}

/*
 * Returns:
 * a new node that should become the 'down' link for a new
 * node inserted into the level of 'node', or NULL if no such
 * link should be created.
 */
static skip_node_t*
insert_inner(skip_node_t* node, gint key)
{
  if (node == NULL)
    return NULL;

  g_assert(node->key < key);
  while ((node->next != NULL) && (node->next->key < key))
    node = node->next;

  /* 'key' already present? finished. */
  if ((node->next != NULL) && (node->next->key == key))
    return NULL;

  gboolean is_bottom_row = (node->down == NULL);
  
  skip_node_t* down = insert_inner(node->down, key);

  if (is_bottom_row || down)
    {
      skip_node_t* new_node = link_new_node(node, key, down);
      return (g_random_double_range(0, 1.0) < node->prob) ? new_node : NULL;
    }
  else
    return NULL;

  g_assert_not_reached();
}

skip_node_t*
skip_list_new(gint n_level, gdouble prob)
{
  if (n_level == 0)
    return NULL;

  skip_node_t* result = g_slice_alloc(sizeof(skip_node_t));

  result->key  = -1;
  result->next = NULL;
  result->prob = prob;

  g_atomic_pointer_set(&result->down, skip_list_new(n_level - 1, prob));

  return result;
}

/*
 * Returns: the key of the second column in 'list', or -1 if
 * 'list' has only one column (e.g. is empty).
 * The second column of 'list' is deleted.
 */
gint
skip_list_pop(skip_node_t* list)
{
  g_assert(list->key == -1);

  /* Find bottom row */
  skip_node_t* node = list;
  while (node->down != NULL)
    node = node->down;

  if (node->next == NULL)
    return -1;

  gint result = node->next->key;

  /* Unlink the nodes */
  for (node = list; node != NULL; node = node->down)
    if (node->next->key == result)
      {
	skip_node_t* dead = node->next;
	g_atomic_pointer_set(&node->next,
			     g_atomic_pointer_get(&node->next->next));
	g_slice_free(skip_node_t, dead);
      }

  return result;
}

void
skip_list_print(skip_node_t* node)
{
  if (node == NULL)
    return;

  skip_node_t* cur;
  for (cur = node; cur != NULL; cur = cur->next)
    {
      g_printf("%2d", cur->key);

      if (cur->next != NULL)
	{
	  g_printf("-");

	  /* track out to bottom node */
	  skip_node_t* track = cur;
	  while (track->down != NULL)
	    track = track->down;

	  while (track->next->key != cur->next->key)
	    {
	      g_printf("---");
	      track = track->next;
	    }

	}
    }

  g_printf("\n");
  skip_list_print(node->down);
}










