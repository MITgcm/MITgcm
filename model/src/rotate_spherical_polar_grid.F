C $Header: /u/gcmpack/MITgcm/model/src/rotate_spherical_polar_grid.F,v 1.1 2008/02/05 13:32:49 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ROTATE_SPHERICAL_POLAR_GRID
C     !INTERFACE:
      SUBROUTINE ROTATE_SPHERICAL_POLAR_GRID( 
     U     X, Y, 
     I     myThid )
C     !DESCRIPTION: \bv
C     /===================================================================\
C     | SUBROUTINE ROTATE_SPHERICAL_POLAR_GRID                            |
C     | o rotate the model coordinates on the input arrays to             |
C     |   geographical coordinates                                        |
C     | o this is useful when a rotated spherical grid is used,           |
C     |   e.g., in order to avoid the pole singularity.                   |
C     | The three Euler angles PhiEuler, ThetaEuler, and PsiEuler         |
C     | define the rotation about the original z-axis (of an sphere       |
C     | centered cartesian grid), the new x-axis, and the new z-axis,     |
C     | respectively.                                                     |
C     | The input coordinates X, Y are assumed to be the model coordinates|
C     | on a rotated grid defined by the Euler angles. In this S/R they   | 
C     | are rotated *BACK* to the geographical coordinate; that is why    |
C     | all rotation matrices are the inverses of the original matrices.  | 
C     | On exit X and Y are the geographical coordinates, that are        |
C     | used to compute the Coriolis parameter and also to interpolate    |
C     | forcing fields to as in pkg/exf/exf_interf.F                      | 
C     | Naturally, this feature does not work with all packages, so the   |
C     | some combinations are prohibited in config_summary (flt,          |
C     | flt_zonal, ecco, profiles), because there the coordinates are     |
C     | assumed to be regular spherical grid coordinates.                 |
C     \===================================================================/
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of ROTATECARTESIAN_GRID
      INTEGER myThid
C     X, Y   - on entry: model coordinate location
C            - on exit: geographical coordinate location
      _RS X(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS Y(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables ==
C     xLoc, yLoc - local copies of X, Y
C     bi,bj  - Loop counters
C     I, J
      INTEGER bi, bj
      INTEGER  I,  J, iA, jA, kA
C     Euler angles in radians
      _RL phiRad, thetaRad, psiRad
C     inverted rotation matrix
      _RL Ainv(3,3), Binv(3,3), Cinv(3,3), Dinv(3,3), CB(3,3)
C     cartesian coordinates
      _RL XYZgeo(3), XYZrot(3)
C     some auxilliary variables
      _RL hypotxy, rcoslat
CEOP

CMLC     for NAOSIMit
CML      phiEuler  = - 30. _d 0
CML      thetaEuler= - 90. _d 0
CML      psiEuler  = - 90. _d 0
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
      DO ja=1,3
       DO ia=1,3
        Ainv(ia,ja) = 0. _d 0
        CB  (ia,ja) = 0. _d 0
       ENDDO
      ENDDO
      DO ja=1,3
       DO ia=1,3
        DO ka=1,3
         CB  (ia,ja) = CB  (ia,ja) + Cinv(ia,ka)*Binv(ka,ja)
        ENDDO
       ENDDO
      ENDDO
      DO ja=1,3
       DO ia=1,3
        DO ka=1,3
         Ainv(ia,ja) = Ainv(ia,ja) + Dinv(ia,ka)*CB(ka,ja)
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
