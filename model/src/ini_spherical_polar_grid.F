C $Header: /u/gcmpack/MITgcm/model/src/ini_spherical_polar_grid.F,v 1.17 2001/02/04 16:46:44 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

#undef  USE_BACKWARD_COMPATIBLE_GRID

CStartOfInterface
      SUBROUTINE INI_SPHERICAL_POLAR_GRID( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_SPHERICAL_POLAR_GRID                      |
C     | o Initialise model coordinate system                     |
C     |==========================================================|
C     | These arrays are used throughout the code in evaluating  |
C     | gradients, integrals and spatial avarages. This routine  |
C     | is called separately by each thread and initialise only  |
C     | the region of the domain it is "responsible" for.        |
C     | Notes:                                                   |
C     | Two examples are included. One illustrates the           |
C     | initialisation of a cartesian grid. The other shows the  |
C     | inialisation of a spherical polar grid. Other orthonormal|
C     | grids can be fitted into this design. In this case       |
C     | custom metric terms also need adding to account for the  |
C     | projections of velocity vectors onto these grids.        |
C     | The structure used here also makes it possible to        |
C     | implement less regular grid mappings. In particular      |
C     | o Schemes which leave out blocks of the domain that are  |
C     |   all land could be supported.                           |
C     | o Multi-level schemes such as icosohedral or cubic       |
C     |   grid projections onto a sphere can also be fitted      |
C     |   within the strategy we use.                            |
C     |   Both of the above also require modifying the support   |
C     |   routines that map computational blocks to simulation   |
C     |   domain blocks.                                         |
C     | Under the spherical polar grid mode primitive distances  |
C     | in X and Y are in degrees. Distance in Z are in m or Pa  |
C     | depending on the vertical gridding mode.                 |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_CARTESIAN_GRID
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     xG, yG - Global coordinate location.
C     xBase  - South-west corner location for process.
C     yBase
C     zUpper - Work arrays for upper and lower 
C     zLower   cell-face heights.
C     phi    - Temporary scalar
C     iG, jG - Global coordinate index. Usually used to hold
C              the south-west global coordinate of a tile.
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     xBase  - Lower coordinate for this threads cells
C     yBase
C     lat, latN, - Temporary variables used to hold latitude
C     latS         values.
C     I,J,K
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J
      _RL lat, dlat, dlon, xG0, yG0


C "Long" real for temporary coordinate calculation
C  NOTICE the extended range of indices!!
      _RL xGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)
      _RL yGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)

C These functions return the "global" index with valid values beyond
C halo regions
      INTEGER iGl,jGl
      iGl(I,bi) = 1+mod(myXGlobalLo-1+(bi-1)*sNx+I+Olx*Nx-1,Nx)
      jGl(J,bj) = 1+mod(myYGlobalLo-1+(bj-1)*sNy+J+Oly*Ny-1,Ny)

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--     "Global" index (place holder)
        jG = myYGlobalLo + (bj-1)*sNy
        iG = myXGlobalLo + (bi-1)*sNx

C--   First find coordinate of tile corner (meaning outer corner of halo)
        xG0 = thetaMin
C       Find the X-coordinate of the outer grid-line of the "real" tile
        DO i=1, iG-1
         xG0 = xG0 + delX(i)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO i=1, Olx
         xG0 = xG0 - delX( 1+mod(Olx*Nx-1+iG-i,Nx) )
        ENDDO
C       Find the Y-coordinate of the outer grid-line of the "real" tile
        yG0 = phiMin
        DO j=1, jG-1
         yG0 = yG0 + delY(j)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO j=1, Oly
         yG0 = yG0 - delY( 1+mod(Oly*Ny-1+jG-j,Ny) )
        ENDDO

C--     Calculate coordinates of cell corners for N+1 grid-lines
        DO J=1-Oly,sNy+Oly +1
         xGloc(1-Olx,J) = xG0
         DO I=1-Olx,sNx+Olx
c         xGloc(I+1,J) = xGloc(I,J) + delX(1+mod(Nx-1+iG-1+i,Nx))
          xGloc(I+1,J) = xGloc(I,J) + delX( iGl(I,bi) )
         ENDDO
        ENDDO
        DO I=1-Olx,sNx+Olx +1
         yGloc(I,1-Oly) = yG0
         DO J=1-Oly,sNy+Oly
c         yGloc(I,J+1) = yGloc(I,J) + delY(1+mod(Ny-1+jG-1+j,Ny))
          yGloc(I,J+1) = yGloc(I,J) + delY( jGl(J,bj) )
         ENDDO
        ENDDO

C--     Make a permanent copy of [xGloc,yGloc] in [xG,yG]
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          xG(I,J,bi,bj) = xGloc(I,J)
          yG(I,J,bi,bj) = yGloc(I,J)
         ENDDO
        ENDDO

C--     Calculate [xC,yC], coordinates of cell centers
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by averaging
          xC(I,J,bi,bj) = 0.25*( 
     &     xGloc(I,J)+xGloc(I+1,J)+xGloc(I,J+1)+xGloc(I+1,J+1) )
          yC(I,J,bi,bj) = 0.25*( 
     &     yGloc(I,J)+yGloc(I+1,J)+yGloc(I,J+1)+yGloc(I+1,J+1) )
         ENDDO
        ENDDO

C--     Calculate [dxF,dyF], lengths between cell faces (through center)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by averaging
c         dXF(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I,J+1,bi,bj))
c         dYF(I,J,bi,bj) = 0.5*(dYG(I,J,bi,bj)+dYG(I+1,J,bi,bj))
C         by formula
          lat = yC(I,J,bi,bj)
          dlon = delX( iGl(I,bi) )
          dlat = delY( jGl(J,bj) )
          dXF(I,J,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          dXF(I,J,bi,bj) = delX(iGl(I,bi))*deg2rad*rSphere*
     &                     COS(yc(I,J,bi,bj)*deg2rad)
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
          dYF(I,J,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by averaging
c         dXG(I,J,bi,bj) = 0.5*(dXF(I,J,bi,bj)+dXF(I,J-1,bi,bj))
c         dYG(I,J,bi,bj) = 0.5*(dYF(I,J,bi,bj)+dYF(I-1,J,bi,bj))
C         by formula
          lat = 0.5*(yGloc(I,J)+yGloc(I+1,J))
          dlon = delX( iGl(I,bi) )
          dlat = delY( jGl(J,bj) )
          dXG(I,J,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
          dYG(I,J,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     The following arrays are not defined in some parts of the halo
C       region. We set them to zero here for safety. If they are ever
C       referred to, especially in the denominator then it is a mistake!
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          dXC(I,J,bi,bj) = 0.
          dYC(I,J,bi,bj) = 0.
          dXV(I,J,bi,bj) = 0.
          dYU(I,J,bi,bj) = 0.
          rAw(I,J,bi,bj) = 0.
          rAs(I,J,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Calculate [dxC], zonal length between cell centers
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging
          dXC(I,J,bi,bj) = 0.5D0*(dXF(I,J,bi,bj)+dXF(I-1,J,bi,bj))
C         by formula
c         lat = 0.5*(yC(I,J,bi,bj)+yC(I-1,J,bi,bj))
c         dlon = 0.5*(delX( iGl(I,bi) ) + delX( iGl(I-1,bi) ))
c         dXC(I,J,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
C         by difference
c         lat = 0.5*(yC(I,J,bi,bj)+yC(I-1,J,bi,bj))
c         dlon = (xC(I,J,bi,bj)+xC(I-1,J,bi,bj))
c         dXC(I,J,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx,sNx+Olx
C         by averaging
          dYC(I,J,bi,bj) = 0.5*(dYF(I,J,bi,bj)+dYF(I,J-1,bi,bj))
C         by formula
c         dlat = 0.5*(delY( jGl(J,bj) ) + delY( jGl(J-1,bj) ))
c         dYC(I,J,bi,bj) = rSphere*dlat*deg2rad
C         by difference
c         dlat = (yC(I,J,bi,bj)+yC(I,J-1,bi,bj))
c         dYC(I,J,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging (method I)
          dXV(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I-1,J,bi,bj))
          dYU(I,J,bi,bj) = 0.5*(dYG(I,J,bi,bj)+dYG(I,J-1,bi,bj))
C         by averaging (method II)
c         dXV(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I-1,J,bi,bj))
c         dYU(I,J,bi,bj) = 0.5*(dYC(I,J,bi,bj)+dYC(I-1,J,bi,bj))
         ENDDO
        ENDDO

C--     Calculate vertical face area (tracer cells)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          lat=0.5*(yGloc(I,J)+yGloc(I+1,J))
          dlon=delX( iGl(I,bi) )
          dlat=delY( jGl(J,bj) )
          rA(I,J,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *abs( sin((lat+dlat)*deg2rad)-sin(lat*deg2rad) )
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          lat=yC(I,J,bi,bj)-delY( jGl(J,bj) )*0.5 _d 0
          lon=yC(I,J,bi,bj)+delY( jGl(J,bj) )*0.5 _d 0
          rA(I,J,bi,bj) = dyF(I,J,bi,bj)
     &    *rSphere*(SIN(lon*deg2rad)-SIN(lat*deg2rad))
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
         ENDDO
        ENDDO

C--     Calculate vertical face area (u cells)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging
          rAw(I,J,bi,bj) = 0.5*(rA(I,J,bi,bj)+rA(I-1,J,bi,bj))
C         by formula
c         lat=yGloc(I,J)
c         dlon=0.5*( delX( iGl(I,bi) ) + delX( iGl(I-1,bi) ) )
c         dlat=delY( jGl(J,bj) )
c         rAw(I,J,bi,bj) = rSphere*rSphere*dlon*deg2rad
c    &        *abs( sin((lat+dlat)*deg2rad)-sin(lat*deg2rad) )
         ENDDO
        ENDDO

C--     Calculate vertical face area (v cells)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by formula
          lat=yC(I,J,bi,bj)
          dlon=delX( iGl(I,bi) )
          dlat=0.5*( delY( jGl(J,bj) ) + delY( jGl(J-1,bj) ) )
          rAs(I,J,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *abs( sin(lat*deg2rad)-sin((lat-dlat)*deg2rad) )
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          lon=yC(I,J,bi,bj)-delY( jGl(J,bj) )
          lat=yC(I,J,bi,bj)
          rAs(I,J,bi,bj) = rSphere*delX(iGl(I,bi))*deg2rad
     &    *rSphere*(SIN(lat*deg2rad)-SIN(lon*deg2rad))
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
          IF (abs(lat).GT.90..OR.abs(lat-dlat).GT.90.) rAs(I,J,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate vertical face area (vorticity points)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         by formula
          lat=yC(I,J,bi,bj)
          dlon=delX( iGl(I,bi) )
          dlat=0.5*( delY( jGl(J,bj) ) + delY( jGl(J-1,bj) ) )
          rAz(I,J,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &     *abs( sin(lat*deg2rad)-sin((lat-dlat)*deg2rad) )
          IF (abs(lat).GT.90..OR.abs(lat-dlat).GT.90.) rAz(I,J,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate trigonometric terms
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          lat=0.5*(yGloc(I,J)+yGloc(I,J+1))
          tanPhiAtU(i,j,bi,bj)=tan(lat*deg2rad)
          lat=0.5*(yGloc(I,J)+yGloc(I+1,J))
          tanPhiAtV(i,j,bi,bj)=tan(lat*deg2rad)
         ENDDO
        ENDDO

       ENDDO ! bi
      ENDDO ! bj

      RETURN
      END
