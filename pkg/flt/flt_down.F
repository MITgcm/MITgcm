C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_down.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine flt_down (
     I                      myCurrentIter, 
     I                      myCurrentTime, 
     I                      myThid
     &                     )   

c     ==================================================================
c     SUBROUTINE flt_down
c     ==================================================================
c
c     o This routine moves particles vertical from the surface to 
c       the target depth
c
c     ==================================================================
c     SUBROUTINE flt_down
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT.h"

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj

c     == local variables ==

      integer ip

c     == end of interface ==


      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

      do ip=1,npart_tile(bi,bj)

         if(
     &  (                             myCurrentTime.ge.tstart(ip,bi,bj))
     &   .and.
     &  (  tend(ip,bi,bj).eq.-1. .or. myCurrentTime.le.  tend(ip,bi,bj))
     &   .and.
     &  (kpart(ip,bi,bj) .le. flt_surf)
     &   .and.
     &  (iup(ip,bi,bj)   .gt. 0.)
     &      ) then

c         if(tstart(ip,bi,bj) .ne. -1.              .and.
c     &      myCurrentTime    .ge. tstart(ip,bi,bj) .and.
c     &      myCurrentTime    .le.   tend(ip,bi,bj) .and.
c     &      kpart(ip,bi,bj) .eq. flt_surf .and.
c     &      iup(ip,bi,bj)   .gt. 0.) then

c     move float to the target depth
c
            if(mod(myCurrentTime,iup(ip,bi,bj)).eq.itop(ip,bi,bj))
     &      kpart(ip,bi,bj) = kfloat(ip,bi,bj)

         endif
      enddo

      ENDDO
      ENDDO

      return
      end

