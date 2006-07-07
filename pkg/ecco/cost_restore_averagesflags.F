
#include "COST_CPPOPTIONS.h"

      subroutine cost_restore_averagesflags(
     &           lcostrestart, first, last, 
     &           sum1day, dayrec, restartdaydate,
     &           sum1mon, monrec, restartmondate,
     &           sum1year,yearrec, restartyeardate,
     &           mythid )

c     ==================================================================
c     SUBROUTINE cost_restore_averagesflags
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

c     == routine arguments ==

      logical lcostrestart
      logical first
      logical last
      integer sum1day
      integer dayrec
      integer sum1mon
      integer monrec
      integer sum1year
      integer yearrec
      _RL     restartdaydate(4)
      _RL     restartmondate(4)
      _RL     restartyeardate(4)
      integer mythid


      IF (myProcId .eq. 0) THEN
         _BEGIN_MASTER( mythid )
         open(unit=76,file='costaveragesflags')
         read(76,*) 
     &        sum1day, dayrec, sum1mon, monrec, sum1year,yearrec,
     &        restartdaydate, restartmondate, restartyeardate
         close(76)
         _END_MASTER( mythid )
      ENDIF

      end
