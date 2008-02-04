
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "pipehdr.h"

void
pipe_set(int fd, int idx, float value)
{
  lseek(fd, idx * sizeof(float), SEEK_SET);
  write(fd, &value, sizeof(float));
}

float
pipe_get(int fd, int idx)
{
  float result;
  lseek(fd, idx * sizeof(float), SEEK_SET);
  read(fd, &result, sizeof(float));

  return result;
}



int
main(int argc, char* argv[])
{
  assert(argc >= 1);
  int fd = open(argv[1], O_RDWR);

#define ARG_DOUBLE_DEFAULT(_idx_, _default_) (argc > (_idx_) ? strtod(argv[_idx_], NULL) : (_default_))

  double dx = ARG_DOUBLE_DEFAULT(2, 0);
  double dy = ARG_DOUBLE_DEFAULT(3, 0);
  double dz = ARG_DOUBLE_DEFAULT(4, 0);

  float sf_x = pipe_get(fd, AWOL_SF_X);
  float sf_y = pipe_get(fd, AWOL_SF_Y);
  float sf_z = pipe_get(fd, AWOL_SF_Z);

  pipe_set(fd, AWOL_ORIG_X, (pipe_get(fd, AWOL_ORIG_X) + (dx * sf_x)));
  pipe_set(fd, AWOL_ORIG_Y, (pipe_get(fd, AWOL_ORIG_Y) + (dy * sf_y)));
  pipe_set(fd, AWOL_ORIG_Z, (pipe_get(fd, AWOL_ORIG_Z) + (dz * sf_z)));

  close(fd);

  return 0;
}


