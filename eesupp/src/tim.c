/*
//BOP
// !ROUTINE: cloc
// !INTERFACE:
   cloc(  double *curtim )
*/

/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

#define TIM_USES_GETTIMEOFDAY

/*
// !DESCRIPTION:
// *======================================================*
// | cloc( double* timeinsec)                                                
// *======================================================*
// | Call system time routines and return elapsed time 
// | in seconds as a 64-bit float.
// *======================================================*
//EOP
*/
#ifdef TIM_USES_OSF_GETCLOCK
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <sys/time.h>
void procedure_cloc ( double *curtim )
{
 struct timespec tv1;
 getclock(TIMEOFDAY, &tv1);
 *curtim =  (double)((tv1.tv_nsec)+(tv1.tv_sec)*1.E9);
 *curtim = *curtim/1E9;
}
#endif
#ifdef TIM_USES_GETTIMEOFDAY
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <sys/time.h>
void FC_NAMEMANGLE(cloc) ( double *curtim )
{
 struct timeval tv1;
 gettimeofday(&tv1 , (void *)NULL );
 *curtim =  (double)((tv1.tv_usec)+(tv1.tv_sec)*1.E6);
 *curtim = *curtim/1.E6;
 
}
#endif

