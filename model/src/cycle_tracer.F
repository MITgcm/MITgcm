C $Header: /u/gcmpack/MITgcm/model/src/cycle_tracer.F,v 1.7 2005/04/15 14:06:14 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_TRACER(
     I                   bi, bj, k,
     U                   tracer, gTracer,
     I                   myTime, myIter, myThid )
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
C     bi,bj,k,iMin,iMax,jMin,jMax  :: Loop counters
C     tracer, gTracer, gTrNm1      :: Tracer field, with tendencies
C     myTime                       :: Current simulation time for this instance.
C     myIter                       :: Current time-step number
C     myThid                       :: Instance number for 
C                                     this call to S/R CORRECTION_STEP
      INTEGER bi,bj,k
c     INTEGER iMin,iMax,jMin,jMax
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C     Rotate tracer/gTracer/gTrNm1
c     DO j=jMin,jMax
c      DO i=iMin,iMax
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        tracer(i,j,k,bi,bj)=gTracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
