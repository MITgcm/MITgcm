C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.16 2011/05/23 00:33:38 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_TRACER
C     !INTERFACE:
      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, k, deltaTloc,
     I                     tracer,
     U                     gTracer,
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
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi, bj, k
      _RL     deltaTloc
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Step tracer forward in time and store provisional value in gTracer array
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
         gTracer(i,j,k,bi,bj)=tracer(i,j,k,bi,bj)
     &            +deltaTloc*gTracer(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                      *maskInC(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
