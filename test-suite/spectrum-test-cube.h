
#ifndef HAVE_TEST_CUBE_H
#define HAVE_TEST_CUBE_H

#include "burrow.h"

G_BEGIN_DECLS

#define HOS_TYPE_SPECTRUM_TEST_CUBE             (hos_spectrum_test_cube_get_type())
#define HOS_SPECTRUM_TEST_CUBE(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_SPECTRUM_TEST_CUBE, HosSpectrumTestCube))

typedef struct _HosSpectrumTestCubeClass HosSpectrumTestCubeClass;
typedef struct _HosSpectrumTestCube      HosSpectrumTestCube;

struct _HosSpectrumTestCubeClass
{
  HosSpectrumClass parent_class;
};

struct _HosSpectrumTestCube
{
  HosSpectrum parent_instance;
};

HosSpectrumTestCube* spectrum_test_cube_new(void);

gdouble              test_cube_predict    (guint idx);
gdouble              test_cube_I_predict  (guint idx);
gdouble              test_cube_II_predict (guint idx);

GType hos_spectrum_test_cube_get_type (void);


G_END_DECLS


#endif  /* not HAVE_TEST_CUBE_H */

