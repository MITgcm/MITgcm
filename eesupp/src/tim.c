#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <sys/time.h>
void cloc_( double *curtim )
{
 struct timespec tv1;
 getclock(TIMEOFDAY, &tv1);
 *curtim =  (double)((tv1.tv_nsec)+(tv1.tv_sec)*1.E9);
 *curtim = *curtim/1E9;
}

