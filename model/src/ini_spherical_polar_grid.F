C $Header: /u/gcmpack/MITgcm/model/src/ini_spherical_polar_grid.F,v 1.31 2013/02/17 02:27:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

#undef USE_BACKWARD_COMPATIBLE_GRID

CBOP
C     !ROUTINE: INI_SPHERICAL_POLAR_GRID
C     !INTERFACE:
      SUBROUTINE INI_SPHERICAL_POLAR_GRID( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_SPHERICAL_POLAR_GRID
C     | o Initialise model coordinate system arrays
C     *==========================================================*
C     | These arrays are used throughout the code in evaluating
C     | gradients, integrals and spatial avarages. This routine
C     | is called separately by each thread and initialise only
C     | the region of the domain it is "responsible" for.
C     | Under the spherical polar grid mode primitive distances
C     | in X and Y are in degrees. Distance in Z are in m or Pa
C     | depending on the vertical gridding mode.
C     *==========================================================*
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     i, j    :: loop counters
C     lat     :: Temporary variables used to hold latitude values.
C     dlat    :: Temporary variables used to hold latitudes increment.
C     dlon    :: Temporary variables used to hold longitude increment.
C     delXloc :: mesh spacing in X direction
C     delYloc :: mesh spacing in Y direction
C     xGloc   :: mesh corner-point location (local "Long" real array type)
C     yGloc   :: mesh corner-point location (local "Long" real array type)
      LOGICAL skipCalcAngleC
      INTEGER bi, bj
      INTEGER i,  j
      INTEGER gridNx, gridNy
      _RL lat, dlat, dlon
C NOTICE the extended range of indices!!
      _RL delXloc(0-OLx:sNx+OLx)
      _RL delYloc(0-OLy:sNy+OLy)
C NOTICE the extended range of indices!!
      _RL xGloc(1-OLx:sNx+OLx+1,1-OLy:sNy+OLy+1)
      _RL yGloc(1-OLx:sNx+OLx+1,1-OLy:sNy+OLy+1)
CEOP

C--   For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--     set tile local mesh (same units as delX,deY)
C       corresponding to coordinates of cell corners for N+1 grid-lines
        CALL INI_LOCAL_GRID(
     O                       xGloc, yGloc,
     O                       delXloc, delYloc,
     O                       gridNx, gridNy,
     I                       bi, bj, myThid )

C--     Make a permanent copy of [xGloc,yGloc] in [xG,yG]
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xG(i,j,bi,bj) = xGloc(i,j)
          yG(i,j,bi,bj) = yGloc(i,j)
         ENDDO
        ENDDO

C--     Calculate [xC,yC], coordinates of cell centers
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C         by averaging
          xC(i,j,bi,bj) = 0.25 _d 0*(
     &     xGloc(i,j)+xGloc(i+1,j)+xGloc(i,j+1)+xGloc(i+1,j+1) )
          yC(i,j,bi,bj) = 0.25 _d 0*(
     &     yGloc(i,j)+yGloc(i+1,j)+yGloc(i,j+1)+yGloc(i+1,j+1) )
         ENDDO
        ENDDO

C--     Calculate [dxF,dyF], lengths between cell faces (through center)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C         by averaging
c         dxF(i,j,bi,bj) = 0.5*(dxG(i,j,bi,bj)+dxG(i,j+1,bi,bj))
c         dyF(i,j,bi,bj) = 0.5*(dyG(i,j,bi,bj)+dyG(i+1,j,bi,bj))
C         by formula
          lat = yC(i,j,bi,bj)
          dlon = delXloc(i)
          dlat = delYloc(j)
          dxF(i,j,bi,bj) = rSphere*COS(lat*deg2rad)*dlon*deg2rad
          dyF(i,j,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C         by averaging
c         dxG(i,j,bi,bj) = 0.5*(dxF(i,j,bi,bj)+dxF(i,j-1,bi,bj))
c         dyG(i,j,bi,bj) = 0.5*(dyF(i,j,bi,bj)+dyF(i-1,j,bi,bj))
C         by formula
          lat = 0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dlon = delXloc(i)
          dlat = delYloc(j)
          dxG(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
          IF (dxG(i,j,bi,bj).LT.1.) dxG(i,j,bi,bj)=0.
          dyG(i,j,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     The following arrays are not defined in some parts of the halo
C       region. We set them to zero here for safety. If they are ever
C       referred to, especially in the denominator then it is a mistake!
C       Note: this is now done earlier in main S/R INI_GRID
c       DO j=1-OLy,sNy+OLy
c        DO i=1-OLx,sNx+OLx
c         dxC(i,j,bi,bj) = 0.
c         dyC(i,j,bi,bj) = 0.
c         dxV(i,j,bi,bj) = 0.
c         dyU(i,j,bi,bj) = 0.
c         rAw(i,j,bi,bj) = 0.
c         rAs(i,j,bi,bj) = 0.
c        ENDDO
c       ENDDO

C--     Calculate [dxC], zonal length between cell centers
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx ! NOTE range
C         by averaging
          dxC(i,j,bi,bj) = 0.5 _d 0*(dxF(i,j,bi,bj)+dxF(i-1,j,bi,bj))
C         by formula
c         lat = 0.5*(yC(i,j,bi,bj)+yC(i-1,j,bi,bj))
c         dlon = 0.5*( delXloc(i) + delXloc(i-1) )
c         dxC(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
C         by difference
c         lat = 0.5*(yC(i,j,bi,bj)+yC(i-1,j,bi,bj))
c         dlon = (xC(i,j,bi,bj)-xC(i-1,j,bi,bj))
c         dxC(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO j=1-OLy+1,sNy+OLy ! NOTE range
         DO i=1-OLx,sNx+OLx
C         by averaging
          dyC(i,j,bi,bj) = 0.5 _d 0*(dyF(i,j,bi,bj)+dyF(i,j-1,bi,bj))
C         by formula
c         dlat = 0.5*( delYloc(j) + delYloc(j-1) )
c         dyC(i,j,bi,bj) = rSphere*dlat*deg2rad
C         by difference
c         dlat = (yC(i,j,bi,bj)-yC(i,j-1,bi,bj))
c         dyC(i,j,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO j=1-OLy+1,sNy+OLy ! NOTE range
         DO i=1-OLx+1,sNx+OLx ! NOTE range
C         by averaging (method I)
          dxV(i,j,bi,bj) = 0.5 _d 0*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
          dyU(i,j,bi,bj) = 0.5 _d 0*(dyG(i,j,bi,bj)+dyG(i,j-1,bi,bj))
C         by averaging (method II)
c         dxV(i,j,bi,bj) = 0.5*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
c         dyU(i,j,bi,bj) = 0.5*(dyC(i,j,bi,bj)+dyC(i-1,j,bi,bj))
         ENDDO
        ENDDO

C--     Calculate vertical face area (tracer cells)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dlon = delXloc(i)
          dlat = delYloc(j)
          rA(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *ABS( SIN((lat+dlat)*deg2rad)-SIN(lat*deg2rad) )
#ifdef USE_BACKWARD_COMPATIBLE_GRID
          lat = yC(i,j,bi,bj) - delYloc(j)*0.5 _d 0
          dlat= yC(i,j,bi,bj) + delYloc(j)*0.5 _d 0
          rA(i,j,bi,bj) = dyF(i,j,bi,bj)
     &    *rSphere*(SIN(dlat*deg2rad)-SIN(lat*deg2rad))
#endif /* USE_BACKWARD_COMPATIBLE_GRID */
         ENDDO
        ENDDO

C--     Calculate vertical face area (u cells)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx ! NOTE range
C         by averaging
          rAw(i,j,bi,bj) = 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj))
C         by formula
c         lat=yGloc(i,j)
c         dlon = 0.5*( delXloc(i) + delXloc(i-1) )
c         dlat = delYloc(j)
c         rAw(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
c    &        *abs( sin((lat+dlat)*deg2rad)-sin(lat*deg2rad) )
         ENDDO
        ENDDO

C--     Calculate vertical face area (v cells)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C         by formula
          lat=yC(i,j,bi,bj)
          dlon = delXloc(i)
          dlat = 0.5 _d 0*( delYloc(j) + delYloc(j-1) )
#ifdef USE_BACKWARD_COMPATIBLE_GRID
          dlat= delYloc(j)
#endif /* USE_BACKWARD_COMPATIBLE_GRID */
          rAs(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *ABS( SIN(lat*deg2rad)-SIN((lat-dlat)*deg2rad) )
          IF (ABS(lat).GT.90..OR.ABS(lat-dlat).GT.90.) rAs(i,j,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate vertical face area (vorticity points)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C         by formula
          lat  = 0.5 _d 0*(yGloc(i,j)+yGloc(i,j+1))
          dlon = 0.5 _d 0*( delXloc(i) + delXloc(i-1) )
          dlat = 0.5 _d 0*( delYloc(j) + delYloc(j-1) )
          rAz(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &     *ABS( SIN(lat*deg2rad)-SIN((lat-dlat)*deg2rad) )
          IF (ABS(lat).GT.90..OR.ABS(lat-dlat).GT.90.) rAz(i,j,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate trigonometric terms & grid orientation:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i,j+1))
          tanPhiAtU(i,j,bi,bj)=TAN(lat*deg2rad)
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          tanPhiAtV(i,j,bi,bj)=TAN(lat*deg2rad)
C       Note: this is now done earlier in main S/R INI_GRID
c         angleCosC(i,j,bi,bj) = 1.
c         angleSinC(i,j,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Cosine(lat) scaling
        DO j=1-OLy,sNy+OLy
         i = 1
         IF (cosPower.NE.0.) THEN
          lat = 0.5 _d 0*(yGloc(i,j)+yGloc(i,j+1))
          cosFacU(j,bi,bj) = ABS( COS(lat*deg2rad) )**cosPower
          lat = 0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          cosFacV(j,bi,bj) = ABS( COS(lat*deg2rad) )**cosPower
          sqcosFacU(j,bi,bj) = SQRT(cosFacU(j,bi,bj))
          sqcosFacV(j,bi,bj) = SQRT(cosFacV(j,bi,bj))
         ELSE
          cosFacU(j,bi,bj) = 1.
          cosFacV(j,bi,bj) = 1.
          sqcosFacU(j,bi,bj)=1.
          sqcosFacV(j,bi,bj)=1.
         ENDIF
        ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

      IF ( rotateGrid ) THEN
       CALL ROTATE_SPHERICAL_POLAR_GRID( xC, yC, myThid )
       CALL ROTATE_SPHERICAL_POLAR_GRID( xG, yG, myThid )
       skipCalcAngleC = .FALSE.
       CALL CALC_GRID_ANGLES( skipCalcAngleC, myThid )
      ENDIF

      RETURN
      END
