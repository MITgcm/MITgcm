C $Header: /u/gcmpack/MITgcm/model/src/cycle_tracer.F,v 1.3 2001/02/04 14:38:46 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R CYCLE_TRACER                                         |
C     |==========================================================|
C     | o Cycles the time-stepping arrays for a tracer field     |
C     \==========================================================/
      SUBROUTINE CYCLE_TRACER(
     I                   bi, bj, iMin, iMax, jMin, jMax, K,
     U                   tracer, gTracer, gTrNm1,
     I                   myTime, myThid )
      IMPLICIT NONE
C Common
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == Routine Arguments ==
C     bi,bj,iMin,iMax,jMin,jMax, K - Loop counters
C     tracer, gTracer, gTrNm1      - Tracer field, with tendencies
C     myThid                       - Instance number for 
C                                    this call to S/R CORRECTION_STEP
C     myTime                       - Current simulation time for this instance.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
      _RL myTime

C     == Local variables ==
      INTEGER i,j

C     Rotate tracer/gTracer/gTrNm1
      DO j=jMin,jMax
       DO i=iMin,iMax
        tracer(i,j,k,bi,bj)=gTrNm1(i,j,k,bi,bj)
        gTrNm1(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
