
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

static void
usage()
{
#define L(m) {fprintf(stderr, "%s\n", m);}
  L("                                                                ");
  L("Usage: interleave <ser-file> <block-size> <n-blocks>            ");
  L("                                                                ");
  L("De-interleave <ser-file> into <n-blocks> segments (named        ");
  L("<ser-file>.1, <ser-file>.2, etc.); where <block-size> is        ");
  L("the number of contiguous points in a segment.                   ");
  L("							             ");
  L("e.g. if you had an interleaved HETNOE experiment where each     ");
  L("fid contained 2048 points, you would run		             ");
  L("							             ");
  L("interleave ser 2048 2			        	     ");
  L("                                                                ");

  exit(1);
}

typedef float specdata_t;

int
main(int argc, char* argv[])
{
  if (argc != 4) usage();

  char* infile_name = argv[1];

  FILE* infile = fopen(infile_name, "r");
  if (infile == NULL)
    {
      perror(infile_name);
      usage();
    }

  char* endptr;
  long block_size = strtol(argv[2], &endptr, 0);
  if (*endptr != 0) usage();

  long block_n = strtol(argv[3], &endptr, 0);
  if (*endptr != 0) usage();

  assert(block_size > 0);
  assert(block_n > 0);

  FILE* outfiles[block_n];
  int i;
  for (i = 0; i < block_n; ++i)
    {
      char outname[strlen(infile_name) + 6];
      sprintf(outname, "%s.%d", infile_name, i + 1);
      outfiles[i] = fopen(outname, "w");
    }

  specdata_t buf[block_size];

  while(1)
    {
      int i;

      for (i = 0; i < block_n; ++i)
	{
	  size_t n_read = fread(buf, sizeof(specdata_t), block_size, infile);
	  if (n_read  < block_size)
	    return 0;
	  fwrite(buf, sizeof(specdata_t), block_size, outfiles[i]);
	}
    }

  return 0;
}

