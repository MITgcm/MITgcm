C $Header: /u/gcmpack/MITgcm/model/src/ini_spherical_polar_grid.F,v 1.3 1998/04/24 02:10:20 cnh Exp $

#include "CPP_EEOPTIONS.h"

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
C     zG
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
      _RL    xG, yG, zG
      _RL    phi
      _RL    zUpper(Nz), zLower(Nz)
      _RL    xBase, yBase
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL lat, latS, latN

C--   Example of inialisation for spherical polar grid
C--   First set coordinates of cell centers
C     This operation is only performed at start up so for more
C     complex configurations it is usually OK to pass iG, jG to a custom 
C     function and have it return xG and yG.
C     Set up my local grid first
C     Note: In the spherical polar case delX and delY are given in
C           degrees and are relative to some starting latitude and
C           longitude - phiMin and thetaMin.
      DO bj = myByLo(myThid), myByHi(myThid)
       jG = myYGlobalLo + (bj-1)*sNy
       DO bi = myBxLo(myThid), myBxHi(myThid)
        iG = myXGlobalLo + (bi-1)*sNx
        yBase = phiMin
        xBase = thetaMin
        DO i=1,iG-1
         xBase = xBase + delX(i)
        ENDDO
        DO j=1,jG-1
         yBase = yBase + delY(j)
        ENDDO
        yG = yBase
        DO J=1,sNy
         xG = xBase
         DO I=1,sNx
          xc(I,J,bi,bj)  = xG + delX(iG+i-1)*0.5 _d 0
          yc(I,J,bi,bj)  = yG + delY(jG+j-1)*0.5 _d 0
          xG = xG + delX(iG+I-1)
          dxF(I,J,bi,bj) = delX(iG+i-1)*deg2rad*rSphere*COS(yc(I,J,bi,bj)*deg2rad)
          dyF(I,J,bi,bj) = delY(jG+j-1)*deg2rad*rSphere
         ENDDO
         yG = yG + delY(jG+J-1)
        ENDDO
       ENDDO
      ENDDO
C     Now sync. and get edge regions from other threads and/or processes.
C     Note: We could just set the overlap regions ourselves here but
C           exchanging edges is safer and is good practice!
      _EXCH_XY_R4( xc, myThid )
      _EXCH_XY_R4( yc, myThid )
      _EXCH_XY_R4(dxF, myThid )
      _EXCH_XY_R4(dyF, myThid )

C--   Calculate separation between other points
C     dxG, dyG are separations between cell corners along cell faces.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          jG = myYGlobalLo + (bj-1)*sNy + J-1
          iG = myXGlobalLo + (bi-1)*sNx + I-1
          lat = yc(I,J,bi,bj)-delY(jG) * 0.5 _d 0
          dxG(I,J,bi,bj) = rSphere*COS(lat*deg2rad)*delX(iG)*deg2rad
          dyG(I,J,bi,bj) = (dyF(I,J,bi,bj)+dyF(I-1,J,bi,bj))*0.5 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(dxG, myThid )
      _EXCH_XY_R4(dyG, myThid )
C     dxV, dyU are separations between velocity points along cell faces.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          dxV(I,J,bi,bj) = (dxG(I,J,bi,bj)+dxG(I-1,J,bi,bj))*0.5 _d 0
          dyU(I,J,bi,bj) = (dyG(I,J,bi,bj)+dyG(I,J-1,bi,bj))*0.5 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(dxV, myThid )
      _EXCH_XY_R4(dyU, myThid )
C     dxC, dyC is separation between cell centers
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          dxC(I,J,bi,bj)    = (dxF(I,J,bi,bj)+dxF(I-1,J,bi,bj))*0.5 _d 0
          dyC(I,J,bi,bj)    = (dyF(I,J,bi,bj)+dyF(I,J-1,bi,bj))*0.5 _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(dxC, myThid )
      _EXCH_XY_R4(dyC, myThid )
C     Calculate recipricols
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          rDxG(I,J,bi,bj)=1.d0/dxG(I,J,bi,bj)
          rDyG(I,J,bi,bj)=1.d0/dyG(I,J,bi,bj)
          rDxC(I,J,bi,bj)=1.d0/dxC(I,J,bi,bj)
          rDyC(I,J,bi,bj)=1.d0/dyC(I,J,bi,bj)
          rDxF(I,J,bi,bj)=1.d0/dxF(I,J,bi,bj)
          rDyF(I,J,bi,bj)=1.d0/dyF(I,J,bi,bj)
          rDxV(I,J,bi,bj)=1.d0/dxV(I,J,bi,bj)
          rDyU(I,J,bi,bj)=1.d0/dyU(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4(rDxG, myThid )
      _EXCH_XY_R4(rDyG, myThid )
      _EXCH_XY_R4(rDxC, myThid )
      _EXCH_XY_R4(rDyC, myThid )
      _EXCH_XY_R4(rDxF, myThid )
      _EXCH_XY_R4(rDyF, myThid )
      _EXCH_XY_R4(rDxV, myThid )
      _EXCH_XY_R4(rDyU, myThid )
C     Calculate vertical face area 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          jG = myYGlobalLo + (bj-1)*sNy + J-1
          latS = yc(i,j,bi,bj)-delY(jG)*0.5 _d 0
          latN = yc(i,j,bi,bj)+delY(jG)*0.5 _d 0
          zA(I,J,bi,bj) = dyF(I,J,bi,bj)
     &    *rSphere*(SIN(latN*deg2rad)-SIN(latS*deg2rad))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      RETURN
      END
