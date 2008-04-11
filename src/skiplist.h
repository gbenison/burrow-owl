
#ifndef HAVE_SKIPLIST_H
#define HAVE_SKIPLIST_H

typedef struct _skip_list skip_list_t;

skip_list_t* skip_list_new       (gint n_level, gdouble prob);
void         skip_list_insert    (skip_list_t* list, gint key, gpointer data);
gint         skip_list_pop_first (skip_list_t* list);
gpointer     skip_list_lookup    (skip_list_t* list, gint key);
gpointer     skip_list_pop       (skip_list_t* list, gint key);
void         skip_list_foreach   (skip_list_t* list,
				  GFunc func,
				  gpointer user_data);
gint         skip_list_length    (skip_list_t* list);
gboolean     skip_list_is_empty  (skip_list_t* list);

void         skip_list_print          (skip_list_t* list);
void         skip_list_print_last_row (skip_list_t* list);


#endif /* not HAVE_SKIPLIST_H */
