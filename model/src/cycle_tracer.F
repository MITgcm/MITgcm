C $Header: /u/gcmpack/MITgcm/model/src/cycle_tracer.F,v 1.5 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_TRACER(
     I                   bi, bj, iMin, iMax, jMin, jMax, K,
     U                   tracer, gTracer, gTrNm1,
     I                   myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CYCLE_TRACER                                          
C     *==========================================================*
C     | o Cycles the time-stepping arrays for a tracer field      
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C Common
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj,iMin,iMax,jMin,jMax, K :: Loop counters
C     tracer, gTracer, gTrNm1      :: Tracer field, with tendencies
C     myThid                       :: Instance number for 
C                                     this call to S/R CORRECTION_STEP
C     myTime                       :: Current simulation time for this instance.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
      _RL myTime

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C     Rotate tracer/gTracer/gTrNm1
      DO j=jMin,jMax
       DO i=iMin,iMax
        tracer(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
