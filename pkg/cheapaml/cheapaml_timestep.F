C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_timestep.F,v 1.1 2013/02/18 21:17:11 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C     !ROUTINE: CHEAPAML_TIMESTEP
C     !INTERFACE:
      SUBROUTINE CHEAPAML_TIMESTEP(
     I                     bi, bj, iMin, iMax, jMin, jMax,
     I                     deltaTloc,
     I                     gTracer,
     U                     tracer,
     I                     nSubIter, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CHEAPAML_TIMESTEP
C     | o Step cheapaml tracer field forward in time
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
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  deltaTloc
      INTEGER nSubIter, myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Step tracer forward in time
      DO j=jMin,jMax
        DO i=iMin,iMax
          tracer(i,j,bi,bj) = tracer(i,j,bi,bj)
     &            + deltaTloc*gTracer(i,j,bi,bj)
        ENDDO
      ENDDO

      RETURN
      END
