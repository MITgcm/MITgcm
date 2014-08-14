C $Header: /u/gcmpack/MITgcm/model/src/cycle_tracer.F,v 1.9 2014/08/14 16:49:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_TRACER(
     I                   bi, bj,
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
C     bi, bj                  :: current tile indices
C     tracer, gTracer, gTrNm1 :: Tracer field, with tendencies
C     myTime                  :: Current simulation time for this instance.
C     myIter                  :: Current time-step number
C     myThid                  :: my Thread Id number
      INTEGER bi,bj
c     INTEGER iMin,iMax,jMin,jMax
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
c     _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
CEOP

C     Rotate tracer/gTracer
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         tracer(i,j,k) = gTracer(i,j,k)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
