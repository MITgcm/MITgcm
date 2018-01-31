/*
 */
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <unistd.h>
/*  Here, we get the definition of the FC_NAMEMANGLE() macro. */
#include "FC_NAMEMANGLE.h"

   static long clktck = 0;
   static double invclktck = 0.0;

   double FC_NAMEMANGLE(cusertime) (void);
   double FC_NAMEMANGLE(csystemtime) (void);
   double FC_NAMEMANGLE(timenow) ();
   void init_timer();

  void init_timer()
   {
     clktck = sysconf(_SC_CLK_TCK);
     invclktck = 1.0/clktck;
     return;
   }

  double FC_NAMEMANGLE(cusertime) (void)
   {
     clock_t blabla;
     struct tms timest;
/* This is useless overhead but we'd need to call init_timer() elsewhere */ 
     if (clktck == 0) init_timer();
 
     blabla = times(&timest);
     return invclktck*timest.tms_utime;
  
   }

  double FC_NAMEMANGLE(csystemtime) (void)
   {
     clock_t blabla;
     struct tms timest;
/* This is useless overhead but we'd need to call init_timer() elsewhere */
     if (clktck == 0) init_timer();
     
     blabla = times(&timest);
     return invclktck*timest.tms_stime;
  
   }



 double FC_NAMEMANGLE(timenow) (void)
  {
struct timeval timestr;
   void *Tzp=0;
 gettimeofday(&timestr, Tzp);

  return (double)timestr.tv_sec+1.0E-06*(double)timestr.tv_usec;
 }

