
#ifndef HAVE_SKIPLIST_H
#define HAVE_SKIPLIST_H

typedef struct _skip_node skip_node_t;

skip_node_t* skip_list_new    (gint n_level, gdouble prob);
void         skip_list_insert (skip_node_t* self, gint key);
gint         skip_list_pop    (skip_node_t* list);
void         skip_list_print  (skip_node_t* list);

#endif /* not HAVE_SKIPLIST_H */
