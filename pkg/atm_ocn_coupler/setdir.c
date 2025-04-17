/*
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

void FC_NAMEMANGLE(setdir) (int *myPEListId )
{
   char RUN_DIR[1024];
   char *rundirsetting;
   char *SROOT;
   char *SPREF;
   char *scycle;
   char *endPtr;
   char SROOT_BUFFER[1024];
   int  rc;
   int  sDirNum;
   int sDirCycle;

   sprintf(RUN_DIR,"rank_%d",*myPEListId);
   rc = chdir(RUN_DIR);
   /* perror(strerror(rc));
      exit(-1); */
}
