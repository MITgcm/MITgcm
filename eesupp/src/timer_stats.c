#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>

   double user_time(void);
   double system_time(void);
   double timenow();

  double user_time(void)
   {
     clock_t blabla;
     struct tms timest;
     
     blabla = times(&timest);
     return timest.tms_utime;
  
   }

  double system_time(void)
   {
     clock_t blabla;
     struct tms timest;
     
     blabla = times(&timest);
     return timest.tms_stime;
  
   }



 double timenow(void)
  {
struct timeval timestr;
   void *Tzp=0;
 gettimeofday(&timestr, Tzp);

  return (double)timestr.tv_sec+1.0E-06*(double)timestr.tv_usec;
 }

