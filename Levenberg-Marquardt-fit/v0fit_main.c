#ifdef SHINE_CLIENT_IS_FUNCTION
#include <stdio.h>
#include <string.h>

/*#include "v0fit.h"*/


/*void main_v0fit_(int*, char*, int*);*/
void main_v0fit_(void);


int
main_c_v0fit(int argc, char* argv[])
{
#if 0
  int i, sumlen = 0;

  /* Set length of argv. */
  for (i = 0; i < argc; ++i)
    sumlen += strlen(argv[i]);

  /* Create the new argv. */
  char newargv[sumlen + argc - 1];
  int pos = 0;

  for (i = 0; i < argc; ++i) {
    int len = strlen(argv[i]);
    int j;
    for (j = 0; j < len; ++j) {
      newargv[pos] = argv[i][j];
      ++pos;
    }
    newargv[pos] = ' ';
    ++pos;
  }
  newargv[pos] = '\0';
  sumlen = strlen(newargv);

  /* Inform the Framework. */
  fprintf(stderr, "    C wrapper-> The value of newarg is: %s\n", newargv);
  fprintf(stderr, "    C wrapper-> Calling Fortran subroutine...\n");
  fprintf(stderr, "FORTRAN V0FIT:\n");

  /* Call the Fortran subroutine with the correct argv (simple char*). */
  main_v0fit_(&argc, newargv, &sumlen);
#endif
  main_v0fit_();
  return 0;
}
#endif
