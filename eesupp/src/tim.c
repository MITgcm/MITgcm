/*
//BOP
// !ROUTINE: cloc
// !INTERFACE:
   cloc(  double *curtim )
*/
#ifdef TARGET_LINUX
#undef  FORTRAN_MANGLE_TRAILING_UNDERSCORE
#else
#define FORTRAN_MANGLE_TRAILING_UNDERSCORE
#endif
#define TIM_USES_GETTIMEOFDAY

#ifdef FORTRAN_MANGLE_TRAILING_UNDERSCORE
#define procedure_cloc cloc_
#else
#define procedure_cloc cloc
#endif
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
void procedure_cloc ( double *curtim )
{
 struct timeval tv1;
 gettimeofday(&tv1 , (void *)NULL );
 *curtim =  (double)((tv1.tv_usec)+(tv1.tv_sec)*1.E6);
 *curtim = *curtim/1.E6;
 
}
#endif

