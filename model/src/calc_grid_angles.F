C $Header: /u/gcmpack/MITgcm/model/src/calc_grid_angles.F,v 1.1 2011/12/25 22:24:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CALC_GRID_ANGLES
C     !INTERFACE:
      SUBROUTINE CALC_GRID_ANGLES( myThid )

C     !DESCRIPTION: \bv
C     *===================================================================*
C     | SUBROUTINE CALC_GRID_ANGLES
C     | o calculate the angle between geographical north and model grid
C     |   north, assuming that yG holds the geographical coordinates
C     *===================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id Number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i, j   :: Loop counters
      INTEGER bi, bj
      INTEGER  i,  j
C     pseudo velocities
      _RL uPseudo(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vPseudo(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uC, vC, uNorm
CEOP

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     compute pseudo velocities from stream function psi = -yG*deg2rad,
C     that is, zonal flow
        DO j = 1-OLy,sNy+OLy-1
         DO i = 1-OLx,sNx+OLx
          IF ( _dyG(i,j,bi,bj).GT.0. ) THEN
            uPseudo(i,j) =
     &         - ( yG(i,j,bi,bj) - yG(i,j+1,bi,bj) )*deg2rad
     &         / _dyG(i,j,bi,bj)
          ELSE
            uPseudo(i,j) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx-1
          IF ( _dxG(i,j,bi,bj).GT.0. ) THEN
            vPseudo(i,j) =
     &         + ( yG(i,j,bi,bj) - yG(i+1,j,bi,bj) )*deg2rad
     &         / _dxG(i,j,bi,bj)
          ELSE
            vPseudo(i,j) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-OLy,sNy+OLy-1
         DO i = 1-OLx,sNx+OLx-1
          uC = 0.5*(uPseudo(i,j) + uPseudo(i+1,j))
          vC = 0.5*(vPseudo(i,j) + vPseudo(i,j+1))
          uNorm = SQRT(uC*uC+vC*vC)
          IF ( uNorm .NE. 0. _d 0 ) uNorm = 1./uNorm
          angleCosC(i,j,bi,bj) =  uC*uNorm
          angleSinC(i,j,bi,bj) = -vC*uNorm
         ENDDO
        ENDDO
C     bi,bj-loops
       ENDDO
      ENDDO

      RETURN
      END
