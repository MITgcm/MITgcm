C $Header: /u/gcmpack/MITgcm/model/src/rotate_spherical_polar_grid.F,v 1.3 2011/02/21 17:57:36 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C--  File rotate_spherical_polar_grid.F:
C--   Contents
C--   o ROTATE_SPHERICAL_POLAR_GRID
C--   o CALC_ANGLES

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ROTATE_SPHERICAL_POLAR_GRID
C     !INTERFACE:
      SUBROUTINE ROTATE_SPHERICAL_POLAR_GRID(
     U                  X, Y,
     I                  myThid )

C     !DESCRIPTION: \bv
C     *===================================================================*
C     | SUBROUTINE ROTATE_SPHERICAL_POLAR_GRID
C     | o rotate the model coordinates on the input arrays to
C     |   geographical coordinates
C     | o this is useful when a rotated spherical grid is used,
C     |   e.g., in order to avoid the pole singularity.
C     | The three Euler angles PhiEuler, ThetaEuler, and PsiEuler
C     | define the rotation about the original z-axis (of an sphere
C     | centered cartesian grid), the new x-axis, and the new z-axis,
C     | respectively.
C     | The input coordinates X, Y are assumed to be the model coordinates
C     | on a rotated grid defined by the Euler angles. In this S/R they
C     | are rotated *BACK* to the geographical coordinate; that is why
C     | all rotation matrices are the inverses of the original matrices.
C     | On exit X and Y are the geographical coordinates, that are
C     | used to compute the Coriolis parameter and also to interpolate
C     | forcing fields to as in pkg/exf/exf_interf.F
C     | Naturally, this feature does not work with all packages, so the
C     | some combinations are prohibited in config_summary (flt,
C     | flt_zonal, ecco, profiles), because there the coordinates are
C     | assumed to be regular spherical grid coordinates.
C     *===================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     X, Y   :: on entry: model coordinate location
C            :: on exit: geographical coordinate location
C     myThid :: my Thread Id Number
      _RS X(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS Y(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i, j   :: Loop counters
      INTEGER bi, bj
      INTEGER  I,  J, iA, jA, kA
C     Euler angles in radians
      _RL phiRad, thetaRad, psiRad
C     inverted rotation matrix
      _RL Ainv(3,3), Binv(3,3), Cinv(3,3), Dinv(3,3), CB(3,3)
C     cartesian coordinates
      _RL XYZgeo(3), XYZrot(3)
C     some auxilliary variables
      _RL hypotxy
CEOP

C     convert to radians
      phiRad    = phiEuler  *deg2rad
      thetaRad  = thetaEuler*deg2rad
      psiRad    = psiEuler  *deg2rad

C     create inverse of full rotation matrix
      Dinv(1,1) =  COS(phiRad)
      Dinv(1,2) = -SIN(phiRad)
      Dinv(1,3) =  0. _d 0
C
      Dinv(2,1) =  SIN(phiRad)
      Dinv(2,2) =  COS(phiRad)
      Dinv(2,3) =  0. _d 0
C
      Dinv(3,1) =  0. _d 0
      Dinv(3,2) =  0. _d 0
      Dinv(3,3) =  1. _d 0
C
      Cinv(1,1) =  1. _d 0
      Cinv(1,2) =  0. _d 0
      Cinv(1,3) =  0. _d 0
C
      Cinv(2,1) =  0. _d 0
      Cinv(2,2) =  COS(thetaRad)
      Cinv(2,3) = -SIN(thetaRad)
C
      Cinv(3,1) =  0. _d 0
      Cinv(3,2) =  SIN(thetaRad)
      Cinv(3,3) =  COS(thetaRad)
C
      Binv(1,1) =  COS(psiRad)
      Binv(1,2) = -SIN(psiRad)
      Binv(1,3) =  0. _d 0
C
      Binv(2,1) =  SIN(psiRad)
      Binv(2,2) =  COS(psiRad)
      Binv(2,3) =  0. _d 0
C
      Binv(3,1) =  0. _d 0
      Binv(3,2) =  0. _d 0
      Binv(3,3) =  1. _d 0
C     Ainv = Binv*Cinv*Dinv (matrix multiplications)
      DO jA=1,3
       DO iA=1,3
        Ainv(iA,jA) = 0. _d 0
        CB  (iA,jA) = 0. _d 0
       ENDDO
      ENDDO
      DO jA=1,3
       DO iA=1,3
        DO kA=1,3
         CB  (iA,jA) = CB  (iA,jA) + Cinv(iA,kA)*Binv(kA,jA)
        ENDDO
       ENDDO
      ENDDO
      DO jA=1,3
       DO iA=1,3
        DO kA=1,3
         Ainv(iA,jA) = Ainv(iA,jA) + Dinv(iA,kA)*CB(kA,jA)
        ENDDO
       ENDDO
      ENDDO
C

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     loop over grid points
        DO J = 1-Oly,sNy+Oly
         DO I = 1-Olx,sNx+Olx
C     transform spherical coordinates with unit radius
C     to sphere centered cartesian coordinates
          XYZrot(1) =
     &         COS( Y(I,J,bi,bj)*deg2rad )*COS( X(I,J,bi,bj)*deg2rad )
          XYZrot(2) =
     &         COS( Y(I,J,bi,bj)*deg2rad )*SIN( X(I,J,bi,bj)*deg2rad )
          XYZrot(3) = SIN( Y(I,J,bi,bj)*deg2rad )
C     rotate cartesian coordinate (matrix multiplication)
          DO iA=1,3
           XYZgeo(iA) = 0. _d 0
          ENDDO
          DO iA=1,3
           DO jA=1,3
            XYZgeo(iA) = XYZgeo(iA) + Ainv(iA,jA)*XYZrot(jA)
           ENDDO
          ENDDO
C     tranform cartesian coordinates back to spherical coordinates
          hypotxy = SQRT( ABS(XYZgeo(1))*ABS(XYZgeo(1))
     &                  + ABS(XYZgeo(2))*ABS(XYZgeo(2)) )
          IF(XYZgeo(1) .EQ. 0. _d 0 .AND. XYZgeo(2) .EQ. 0. _d 0)THEN
C     happens exactly at the poles
           X(I,J,bi,bj) = 0. _d 0
          ELSE
           X(I,J,bi,bj) = ATAN2(XYZgeo(2),XYZgeo(1))/deg2rad
           IF ( X(I,J,bi,bj) .LT. 0. _d 0 )
     &          X(I,J,bi,bj) = X(I,J,bi,bj) + 360. _d 0
          ENDIF
          IF(hypotxy .EQ. 0. _d 0 .AND. XYZgeo(3) .EQ. 0. _d 0)THEN
C     this can not happen for a sphere with unit radius
           Y(I,J,bi,bj) = 0. _d 0
          ELSE
           Y(I,J,bi,bj) = ATAN2(XYZgeo(3),hypotxy)/deg2rad
          ENDIF
         ENDDO
        ENDDO
C     bi,bj-loops
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CALC_ANGLES
C     !INTERFACE:
      SUBROUTINE CALC_ANGLES(
     I                        myThid )

C     !DESCRIPTION: \bv
C     *===================================================================*
C     | SUBROUTINE CALC_ANGLES
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
      _RS uPseudo(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS vPseudo(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS uC, vC, uNorm
CEOP

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     compute pseudo velocities from stream function psi = -yG*deg2rad,
C     that is, zonal flow
        DO j = 1-Oly,sNy+Oly-1
         DO i = 1-Olx,sNx+Olx
          IF ( _dyG(i,j,bi,bj).GT.0. ) THEN
            uPseudo(i,j) =
     &         - ( yG(i,j,bi,bj) - yG(i,j+1,bi,bj) )*deg2rad
     &         / _dyG(i,j,bi,bj)
          ELSE
            uPseudo(i,j) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-Oly,sNy+Oly
         DO i = 1-Olx,sNx+Olx-1
          IF ( _dxG(i,j,bi,bj).GT.0. ) THEN
            vPseudo(i,j) =
     &         + ( yG(i,j,bi,bj) - yG(i+1,j,bi,bj) )*deg2rad
     &         / _dxG(i,j,bi,bj)
          ELSE
            vPseudo(i,j) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j = 1-Oly,sNy+Oly-1
         DO i = 1-Olx,sNx+Olx-1
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
