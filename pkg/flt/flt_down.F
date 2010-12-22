C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_down.F,v 1.5 2010/12/22 21:25:18 jahn Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_DOWN (
     I                      myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_DOWN
C     ==================================================================
C     o This routine moves particles vertical from the surface to
C       the target depth
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==
      INTEGER bi, bj
      INTEGER ip

C     == end of interface ==

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO ip=1,npart_tile(bi,bj)

         IF (
     &  (                             myTime.GE.tstart(ip,bi,bj))
     &   .AND.
     &  (  tend(ip,bi,bj).EQ.-1. .OR. myTime.LE.  tend(ip,bi,bj))
     &   .AND.
     &  (kpart(ip,bi,bj) .LE. flt_surf)
     &   .AND.
     &  (iup(ip,bi,bj)   .GT. 0.)
     &      ) THEN

c         IF (tstart(ip,bi,bj) .NE. -1.              .AND.
c     &      myTime    .GE. tstart(ip,bi,bj) .AND.
c     &      myTime    .LE.   tend(ip,bi,bj) .AND.
c     &      kpart(ip,bi,bj) .EQ. flt_surf .AND.
c     &      iup(ip,bi,bj)   .GT. 0.) THEN

C     move float to the target depth

           IF (mod(myTime,iup(ip,bi,bj)).EQ.itop(ip,bi,bj))
     &      kpart(ip,bi,bj) = kfloat(ip,bi,bj)

         ENDIF
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END

