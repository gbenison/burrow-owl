
#ifndef HAVE_SKIPLIST_H
#define HAVE_SKIPLIST_H

typedef struct _skip_node skip_node_t;

skip_node_t* skip_list_new       (gint n_level, gdouble prob);
void         skip_list_insert    (skip_node_t* self, gint key, gpointer data);
gint         skip_list_pop_first (skip_node_t* list);
gpointer     skip_list_lookup    (skip_node_t* list, gint key);
gpointer     skip_list_pop       (skip_node_t* list, gint key);
void         skip_list_print     (skip_node_t* list);

#endif /* not HAVE_SKIPLIST_H */
