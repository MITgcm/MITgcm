C $Header: /u/gcmpack/MITgcm/model/src/timestep_tracer.F,v 1.18 2014/08/08 19:55:52 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_TRACER
C     !INTERFACE:
      SUBROUTINE TIMESTEP_TRACER(
     I                     bi, bj, deltaTLev,
     I                     tracer,
     U                     gTracer,
     I                     myTime, myIter, myThid )
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
C     bi, bj     :: current tile indices
C     deltaTLev  :: time-step [s] (vertical dimension vector)
C     tracer     :: tracer field
C     gTracer    :: input: tracer tendency ; output: updated tracer
C     myTime     :: current time in simulation
C     myIter     :: current iteration number
C     myThid     :: my Thread Id number
      INTEGER bi, bj
      _RL     deltaTLev(Nr)
      _RL     tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j, k
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Step tracer forward in time and store provisional value in gTracer array
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          gTracer(i,j,k) = tracer(i,j,k)
     &                   + deltaTLev(k)*gTracer(i,j,k)
#ifdef ALLOW_OBCS
CML   For the Stevens open boundary conditions, we need to step forward
CML   the tracer even on the boundary in order to include surface
CML   forcing and (vertical) diffusion contributions. For other cases
CML   this mask was meant to avoid unrealistic values on the open boundaries.
CML   Lets hope that we can live without this safety net.
C    &                      *maskInC(i,j,bi,bj)
#endif
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
