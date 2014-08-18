C $Header: /u/gcmpack/MITgcm/model/src/cycle_ab_tracer.F,v 1.3 2014/08/18 12:24:16 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_AB_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_AB_TRACER(
     I                   bi, bj,
     I                   tracNew,
     U                   tracer,
     O                   tracNm1,
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
C     bi, bj    :: current tile indices
C     tracNew   :: next     time-step tracer field
C     tracer    :: current  time-step tracer field
C     tracNm1   :: previous time-step tracer field
C     myTime    :: Current simulation time
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      INTEGER bi,bj
      _RL tracNew(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
      INTEGER i,j,k
CEOP

C     Rotate tracNew/tracer/tracNm1
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         tracNm1(i,j,k) = tracer(i,j,k)
         tracer(i,j,k) = tracNew(i,j,k)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
