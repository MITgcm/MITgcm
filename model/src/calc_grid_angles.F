C $Header: /u/gcmpack/MITgcm/model/src/calc_grid_angles.F,v 1.2 2013/02/17 02:29:52 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CALC_GRID_ANGLES
C     !INTERFACE:
      SUBROUTINE CALC_GRID_ANGLES( skipCalcAngleC, myThid )

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
C     skipCalcAngleC :: skip setting of grid-angle at cell-center location
C     myThid  :: my Thread Id Number
      LOGICAL skipCalcAngleC
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
      _RL uC, vC, uNorm, tmpVal
CEOP

C-    For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-    compute pseudo velocities from stream function psi = -yG*deg2rad,
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
          u2zonDir(i,j,bi,bj) = rSphere*uPseudo(i,j)
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
          v2zonDir(i,j,bi,bj) = rSphere*vPseudo(i,j)
         ENDDO
        ENDDO
        IF ( .NOT.skipCalcAngleC ) THEN
         DO j = 1-OLy,sNy+OLy-1
          DO i = 1-OLx,sNx+OLx-1
           uC = 0.5*(uPseudo(i,j) + uPseudo(i+1,j))
           vC = 0.5*(vPseudo(i,j) + vPseudo(i,j+1))
           uNorm = SQRT(uC*uC+vC*vC)
           IF ( uNorm .NE. 0. _d 0 ) uNorm = 1. _d 0/uNorm
           angleCosC(i,j,bi,bj) =  uC*uNorm
           angleSinC(i,j,bi,bj) = -vC*uNorm
          ENDDO
         ENDDO
        ENDIF

C-   To check angular momentum conservation, use an alternative definition
C    of grid-angles cosine (@ U pt) & sine (@ V pt) (consistent with
C    stream-function of solid-body velocity field).
        DO j = 1-OLy,sNy+OLy-1
         DO i = 1-OLx,sNx+OLx
C- Note: most natural way would be to divide by dyG (as below); but scaling by
C        dxC/rAw ensures that u2zonDir is exactly =1 with current Lat-Lon grid
c         tmpVal = _dyG(i,j,bi,bj) * COS( deg2rad*
          tmpVal = _rAw(i,j,bi,bj) * COS( deg2rad*
     &             ( yG(i,j,bi,bj) + yG(i,j+1,bi,bj) )*halfRL )
          IF ( tmpVal.GT.0. ) THEN
            u2zonDir(i,j,bi,bj) =  rSphere
     &          *( SIN( yG(i,j+1,bi,bj)*deg2rad )
     &           - SIN( yG(i, j, bi,bj)*deg2rad )
     &           )* _dxC(i,j,bi,bj)/tmpVal
c    &           )/tmpVal
          ELSE
            u2zonDir(i,j,bi,bj) = 1.
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx-1
C- Note: most natural way would be to divide by dxG (as below); for symetry
C        reason with u2zonDir expression, we use instead dyC/rAs scaling factor
c         tmpVal = _dxG(i,j,bi,bj) * COS( deg2rad*
          tmpVal = _rAs(i,j,bi,bj) * COS( deg2rad*
     &             ( yG(i,j,bi,bj) + yG(i+1,j,bi,bj) )*halfRL )
          IF ( tmpVal.GT.0. ) THEN
            v2zonDir(i,j,bi,bj) = -rSphere
     &          *( SIN( yG(i+1,j,bi,bj)*deg2rad )
     &           - SIN( yG(i,j,bi,bj)*deg2rad )
     &           )* _dyC(i,j,bi,bj)/tmpVal
c    &           )/tmpVal
          ELSE
            v2zonDir(i,j,bi,bj) = 0.
          ENDIF
         ENDDO
        ENDDO

C-    bi,bj-loops
       ENDDO
      ENDDO

      RETURN
      END
