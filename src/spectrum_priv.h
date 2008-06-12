
/* private API of 'spectrum' object */

#ifndef _HOS_HAVE_SPECTRUM_PRIV_H
#define _HOS_HAVE_SPECTRUM_PRIV_H

#include "burrow/spectrum.h"

/*
 * 'spectrum->buf' contains the spectral data.
 * A special value of 'spectrum->buf[i]' indicates that
 * value 'i' is not yet known.
 */
#define DATUM_UNKNOWN_VALUE             0
#define DATUM_UNKNOWN_VALUE_SUBSTITUTE  G_MINDOUBLE
#define DATUM_ENSURE_KNOWN(x)           {if ((x) == DATUM_UNKNOWN_VALUE) (x) = DATUM_UNKNOWN_VALUE_SUBSTITUTE;}
#define DATUM_IS_KNOWN(x)               ((x) != DATUM_UNKNOWN_VALUE)

typedef struct _dimension dimension_t;
struct _dimension
{
  guint   np;
  gdouble orig;
  gdouble sf;
  gdouble sw;
};

GList*   spectrum_copy_dimensions (HosSpectrum *self);
void     spectrum_set_dimensions  (HosSpectrum *self, GList *dimensions);

/*
 *  --- Iterators ---
 *
 *  Operations supported by spectrum iterators:
 *
 *  increment -- change the position of the pointer
 *  tickle    -- peek at the position of the pointer, returns TRUE if value is available
 *  mark      -- save the position of the pointer (see 'wait' operation)
 *  wait      -- block until the value at the position saved by 'mark' is available
 *
 */
struct spectrum_iterator* spectrum_construct_iterator(HosSpectrum *self);
void     iterator_free        (struct spectrum_iterator *self);
void     iterator_increment   (struct spectrum_iterator *self, guint dim, gint delta);
gboolean iterator_tickle      (struct spectrum_iterator *self, gdouble *dest);
void     iterator_mark        (struct spectrum_iterator *self);
void     iterator_restore     (struct spectrum_iterator *self);
gdouble  iterator_wait        (struct spectrum_iterator *self);
gboolean iterator_probe       (struct spectrum_iterator *self);

struct spectrum_iterator
{
  HosSpectrum *root;
  GType        root_type;
  gint         ndim;
  guint       *idx;
  guint       *save_idx;
  gsize       *stride;
  gsize       *np;
  gsize        idx_linear;
  gsize        save_idx_linear;
  gboolean     can_cache;

  gboolean (*tickle)      (struct spectrum_iterator *self, gdouble *dest);
  void     (*mark)        (struct spectrum_iterator *self);
  void     (*restore)     (struct spectrum_iterator *self);
  gdouble  (*wait)        (struct spectrum_iterator *self);
  gboolean (*probe)       (struct spectrum_iterator *self);
  void     (*increment)   (struct spectrum_iterator *self, guint dim, gint delta);
};


#endif /* not  _HOS_HAVE_SPECTRUM_PRIV_H */

