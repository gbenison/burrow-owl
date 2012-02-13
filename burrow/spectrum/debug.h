
extern gint burrow_debug_lvl;

void burrow_debug_print(gchar *str);

#define CONFESS_FULL(lvl, fmt, ...) { \
    burrow_debug_init(); \
    if (burrow_debug_lvl >= lvl) { \
      gchar *prefix = g_strdup_printf("%35s:%4d (thread 0x%p) ", __FILE__, __LINE__, g_thread_self()); \
      gchar *suffix = g_strdup_printf(fmt, __VA_ARGS__); \
      gchar *msg = g_strdup_printf("%s | %s\n", prefix, suffix); \
      g_free(prefix); \
      g_free(suffix); \
      burrow_debug_print(msg); }}

#define CONFESS(fmt, ...) { CONFESS_FULL(1, fmt, __VA_ARGS__) }

