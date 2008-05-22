
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

gdouble  spectrum_accumulate (HosSpectrum* self, HosSpectrum* root, guint* idx);
gboolean spectrum_tickle     (HosSpectrum* self, HosSpectrum* root, guint* idx, gdouble* dest);

GList*   spectrum_copy_dimensions (HosSpectrum *self);
void     spectrum_set_dimensions  (HosSpectrum *self, GList *dimensions);

/* Iterators */
struct spectrum_iterator* spectrum_construct_iterator(HosSpectrum *self);
void     iterator_free        (struct spectrum_iterator *self);
void     iterator_increment   (struct spectrum_iterator *self, guint dim, gint delta);
void     iterator_save        (struct spectrum_iterator *self);
void     iterator_restore     (struct spectrum_iterator *self);
gboolean iterator_tickle      (struct spectrum_iterator *self, gdouble *dest);
gdouble  iterator_accumulate  (struct spectrum_iterator *self);

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
  gboolean     blocked;

  gboolean (*tickle)      (struct spectrum_iterator* self, gdouble *dest);
  gdouble  (*accumulate)  (struct spectrum_iterator* self);
  void     (*increment)   (struct spectrum_iterator* self, guint dim, gint delta);
  void     (*save)        (struct spectrum_iterator* self);
  void     (*restore)     (struct spectrum_iterator* self);
};


#endif /* not  _HOS_HAVE_SPECTRUM_PRIV_H */

