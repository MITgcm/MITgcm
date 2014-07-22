C $Header: /u/gcmpack/MITgcm/model/src/cycle_ab_tracer.F,v 1.1 2014/07/22 11:59:09 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_AB_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_AB_TRACER(
     I                   bi, bj,
     U                   tracer, gTracer, gTrNm1,
     I                   myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CYCLE_AB_TRACER
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
C     bi, bj                       :: current tile indices
C     tracer, gTracer, gTrNm1      :: Tracer field, with tendencies
C     myTime                       :: Current simulation time
C     myIter                       :: Current time-step number
C     myThid                       :: my Thread Id number
      INTEGER bi,bj
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
CEOP

C     Rotate tracer/gTracer/gTrNm1
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         gTrNm1(i,j,k,bi,bj) =  tracer(i,j,k,bi,bj)
         tracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
