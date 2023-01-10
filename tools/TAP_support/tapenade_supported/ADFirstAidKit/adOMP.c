#include <stdio.h>
#include <stdlib.h>
#include "omp.h"
#include "adStack.h"
#include "adOMP.h"

/*
 Support functions for AD of OpenMP-parallel programs.  This is mostly to
ensure that the work is distributed among threads in the same way during the
forward and reverse sweep.  This is needed to ensure correct data flow, and to
enable the threads to use only their own threadprivate push/pop stack.
*/

/************ ensure OpenMP-enabled stack is used ***********/

void assertSafeStack() {
  if(!stackIsThreadSafe()) {
    printf("ERROR: OpenMP-parallel derivative program uses non-OpenMP ADFirstAidKit.\n") ;
    printf("Update to the latest ADFirstAidKit and compile it with openmp flags enabled.\n") ;
    exit(1) ;
  }
}

/******************** static OpenMP schedule *****************/
/* This is the "easy" option: the static schedule distributes work among threads
   in a deterministic way. If the same schedule is used in the fwd and rev code,
   everything is fine. There is no overhead, no recording, no storage needed to
   do this, but it only works for static schedules. */

void getStaticSchedule(int lo, int hi, int stride, int* threadlo, int* threadhi) {
/* Static OpenMP scheduler, identical to what LLVM would use. Each thread gets
   one chunk of consecutive iterations. The number of iterations per chunk is
   aproximately trip_count/num_threads. If the trip count can not be evenly
   divided among threads, the first few threads get one extra iteration.
   As long as the number of threads stays constant, and when called by the
   same thread, this subroutine will always return the same threadstart and
   threadend when given the same imin,imax,istride as input. */

/* formula to compute the number of iterations, F90 standard sec 8.1.4.4.1 */
  int trip_count = (int)((hi-lo+stride)/stride) ;
  if(trip_count < 0) trip_count = 0 ;

  int nth = omp_get_num_threads() ;
  int tid = omp_get_thread_num() ;

  if(trip_count < nth) {
    /* fewer iterations than threads. some threads will get one iteration,
       the other threads will get nothing. */
    if(tid < trip_count) {
      /* do one iteration */
      *threadlo = lo + tid * stride ;
      *threadhi = *threadlo ;
    }
    else {
      /* do nothing */
      *threadhi = 0 ;
      *threadlo = *threadhi + stride ;
    }
  }
  /* at least one iteration per thread. since the total number of iterations may
     not be evenly dividable by the number of threads, there will be a few extra
     iterations. the first few threads will each get one of those, which results
     in some offsetts that are applied to the start and end of the chunks. */
  else {
    int chunksize = trip_count / nth ;
    int extras = trip_count % nth ;
    int tidextras, incr ;
    if(tid < extras) {
      tidextras = tid ;
      incr = 0 ;
    }
    else {
      tidextras = extras ;
      incr = stride ;
    }
    *threadlo = lo + (tid*chunksize + tidextras) * stride ;
    *threadhi = *threadlo + chunksize * stride - incr ;
  }
  assertSafeStack() ;
}

/****************** dynamic OpenMP schedule *****************/
/* This is the more challenging situation: The user wants a dynamic/auto/guided
   schedule, which is not deterministic. We need to record how the workload was
   distributed among threads, so that the reverse sweep can replay this.
   Doing this requires a few integer operations per loop iteration, and a push
   of two integers (chunk start and end) for each new chunk of work that is
   given to a thread. If the memory consumption of this recording is a problem,
   consider using larger chunks or a static schedule. */
static int numChunks, previousIter, thisChunkStart ;
static char isFirstIter ;
#pragma omp threadprivate(numChunks, previousIter, isFirstIter, thisChunkStart)

void initDynamicSchedule() {
  isFirstIter = 1 ;
  numChunks = 0 ;
  assertSafeStack() ;
}

void recordDynamicSchedule(int counter, int stride) {
  if(isFirstIter) {
    thisChunkStart = counter ;
    isFirstIter = 0 ;
    numChunks ++ ;
  }
  else {
    if(previousIter + stride != counter) {
      pushInteger4(thisChunkStart) ;
      pushInteger4(previousIter) ;
      thisChunkStart = counter ;
      numChunks ++ ;
    }
  }
  previousIter = counter ;
}

void finalizeDynamicSchedule() {
  pushInteger4(thisChunkStart) ;
  pushInteger4(previousIter) ;
  pushInteger4(numChunks) ;
}

/****************** INTERFACE CALLED FROM FORTRAN *******************/

void assertsafestack_() {
  assertSafeStack() ;
}

void getstaticschedule_(int* lo, int* hi, int* stride, int* threadlo, int* threadhi) {
  getStaticSchedule(*lo, *hi, *stride, threadlo, threadhi) ;
}

void initdynamicschedule_() {
  initDynamicSchedule() ;
}

void recorddynamicschedule_(int* counter, int* stride) {
  recordDynamicSchedule(*counter, *stride) ;
}

void finalizedynamicschedule_() {
  finalizeDynamicSchedule() ;
}
