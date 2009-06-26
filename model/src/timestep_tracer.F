C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.14 2009/06/26 23:10:09 jahn Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_TRACER
C     !INTERFACE:
      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, iMin, iMax, jMin, jMax, K,
     I                     advection_scheme, deltaTloc,
     I                     tracer, gTracer,
     I                     myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TIMESTEP_TRACER                                       
C     | o Step model tracer field forward in time                 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      INTEGER advection_scheme
      _RL     deltaTloc
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Extrapolate tendencies to mid-time level (n+1/2)
      DO j=jMin,jMax
       DO i=iMin,iMax
         gTracer(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &            +deltaTloc*gTracer(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
