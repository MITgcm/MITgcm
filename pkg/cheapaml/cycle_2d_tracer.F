C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/Attic/cycle_2d_tracer.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CYCLE_2d_TRACER
C     !INTERFACE:
      SUBROUTINE CYCLE_2d_TRACER(
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
C     bi,bj                        :: Loop counters
C     tracer, gTracer, gTrNm1      :: Tracer field, with tendencies
C     myTime                       :: Current simulation time for this instance.
C     myIter                       :: Current time-step number
C     myThid                       :: Instance number for 
C                                     this call to S/R CORRECTION_STEP
      INTEGER bi,bj
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL gTrNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      INTEGER jmin, jmax, imin, imax
CEOP

C     Rotate tracer/gTracer/gTrNm1
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        tracer(i,j,bi,bj)=gTracer(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
