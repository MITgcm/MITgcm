#include <stdlib.h>
#include <unistd.h>
                                                                                
void setdir_(int *myPEListId )
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
