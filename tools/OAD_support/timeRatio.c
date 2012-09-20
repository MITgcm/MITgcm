#include <sys/time.h>
#include <stdio.h>

void timeratio_() { 
  /* 
     three invocations produce
     the ratio of the timedifference of 
     the 3rd vs. the 2nd invocation over
     the difference of the 2nd vs. the 1st invocation
  */
  static short stage=0;
#define tvCount 3
  static struct timeval tv_a[tvCount];
  double deltaThis, deltaPrevious;
  if (stage<tvCount) 
    gettimeofday(&(tv_a[stage]),0);
  printf("OAD: TIMING: stamp %u: %lu.%06lu\n",
	 stage,
	 tv_a[stage].tv_sec,
	 tv_a[stage].tv_usec);
  if (stage>0) { 
    printf("OAD: TIMING: delta stamps %u-%u: %lu.%06lu\n",
           stage,
           stage-1,
           (tv_a[stage].tv_usec<tv_a[stage-1].tv_usec)?tv_a[stage].tv_sec-tv_a[stage-1].tv_sec-1:tv_a[stage].tv_sec-tv_a[stage-1].tv_sec,
           (tv_a[stage].tv_usec<tv_a[stage-1].tv_usec)?1000000-(tv_a[stage-1].tv_usec-tv_a[stage].tv_usec):tv_a[stage].tv_usec-tv_a[stage-1].tv_usec);
  }
  if (stage>1) { 
    printf("OAD: TIMING: delta stamps %u-%u: %lu.%06lu\n",
           stage,
           0,
           (tv_a[stage].tv_usec<tv_a[0].tv_usec)?tv_a[stage].tv_sec-tv_a[0].tv_sec-1:tv_a[stage].tv_sec-tv_a[0].tv_sec,
           (tv_a[stage].tv_usec<tv_a[0].tv_usec)?1000000-(tv_a[0].tv_usec-tv_a[stage].tv_usec):tv_a[stage].tv_usec-tv_a[0].tv_usec);
  }
  if (stage==tvCount-1 && stage>1) { 
    deltaThis=(tv_a[stage].tv_usec<tv_a[stage-1].tv_usec)?tv_a[stage].tv_sec-tv_a[stage-1].tv_sec-1:tv_a[stage].tv_sec-tv_a[stage-1].tv_sec;
    deltaThis*=1.0e6;
    deltaThis+=(tv_a[stage].tv_usec<tv_a[stage-1].tv_usec)?1000000-(tv_a[stage-1].tv_usec-tv_a[stage].tv_usec):tv_a[stage].tv_usec-tv_a[stage-1].tv_usec;
    deltaPrevious=(tv_a[stage-1].tv_usec<tv_a[stage-2].tv_usec)?tv_a[stage-1].tv_sec-tv_a[stage-2].tv_sec-1:tv_a[stage-1].tv_sec-tv_a[stage-2].tv_sec;
    deltaPrevious*=1.0e6;
    deltaPrevious+=(tv_a[stage-1].tv_usec<tv_a[stage-2].tv_usec)?1000000-(tv_a[stage-2].tv_usec-tv_a[stage-1].tv_usec):tv_a[stage-1].tv_usec-tv_a[stage-2].tv_usec;
    printf("OAD: TIMING: ratio stamps (%u-%u)/(%u-%u): %e/%e=%e\n",
	   stage,
	   stage-1,
	   stage-1,
	   stage-2,
	   deltaThis,
	   deltaPrevious,
	   deltaThis/deltaPrevious);
    stage=0;  
  }
  else
    stage++;
}
