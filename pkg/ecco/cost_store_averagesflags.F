
#include "COST_CPPOPTIONS.h"

      subroutine cost_store_averagesflags(
     &           lflag, first, last, 
     &           sum1day, dayrec, sum1mon, monrec,
     &           restartdaydate, restartmondate,
     &           mythid )

c     ==================================================================
c     SUBROUTINE cost_store_averagesflags
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

c     == routine arguments ==

      logical lflag
      logical first
      logical last
      integer sum1day
      integer dayrec
      integer sum1mon
      integer monrec
      _RL     restartdaydate(4)
      _RL     restartmondate(4)
      integer mythid


      if ( lflag ) then
         IF (myProcId .eq. 0) THEN
            _BEGIN_MASTER( mythid )
            open(unit=76,file='costaveragesflags')
            write(76,*) sum1day, dayrec, sum1mon, monrec, 
     &           restartdaydate, restartmondate
            close(76)
            _END_MASTER( mythid )
         ENDIF
      endif

      end
