C $Header: /u/gcmpack/MITgcm/model/src/ini_cartesian_grid.F,v 1.15 2001/02/04 14:38:47 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_CARTESIAN_GRID( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_CARTESIAN_GRID                            |
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
C     | Under the cartesian grid mode primitive distances in X   |
C     | and Y are in metres. Disktance in Z are in m or Pa       |
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
C     xBase  - Temporaries for lower corner coordinate
C     yBase
C     iG, jG - Global coordinate index. Usually used to hold
C              the south-west global coordinate of a tile.
C     bi,bj  - Loop counters
C     zUpper - Temporary arrays holding z coordinates of
C     zLower   upper and lower faces.
C     I,J,K
      _RL    xGloc, yGloc
      _RL    xBase, yBase
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J

C--   Simple example of inialisation on cartesian grid
C--   First set coordinates of cell centers
C     This operation is only performed at start up so for more
C     complex configurations it is usually OK to pass iG, jG to a custom 
C     function and have it return xG and yG.
C     Set up my local grid first
      xC0 = 0. _d 0
      yC0 = 0. _d 0
      DO bj = myByLo(myThid), myByHi(myThid)
       jG = myYGlobalLo + (bj-1)*sNy
       DO bi = myBxLo(myThid), myBxHi(myThid)
        iG = myXGlobalLo + (bi-1)*sNx
        yBase = 0. _d 0
        xBase = 0. _d 0
        DO i=1,iG-1
         xBase = xBase + delX(i)
        ENDDO
        DO j=1,jG-1
         yBase = yBase + delY(j)
        ENDDO
        yGloc = yBase
        DO J=1,sNy
         xGloc = xBase
         DO I=1,sNx
          xG(I,J,bi,bj)  = xGloc
          yG(I,J,bi,bj)  = yGloc
          xc(I,J,bi,bj)  = xGloc + delX(iG+i-1)*0.5 _d 0
          yc(I,J,bi,bj)  = yGloc + delY(jG+j-1)*0.5 _d 0
          xGloc = xGloc + delX(iG+I-1)
          dxF(I,J,bi,bj) = delX(iG+i-1)
          dyF(I,J,bi,bj) = delY(jG+j-1)
         ENDDO
         yGloc = yGloc + delY(jG+J-1)
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
          dxG(I,J,bi,bj) = (dxF(I,J,bi,bj)+dxF(I,J-1,bi,bj))*0.5 _d 0
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
C     Calculate vertical face area 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          rA (I,J,bi,bj) = dxF(I,J,bi,bj)*dyF(I,J,bi,bj)
          rAw(I,J,bi,bj) = dxC(I,J,bi,bj)*dyG(I,J,bi,bj)
          rAs(I,J,bi,bj) = dxG(I,J,bi,bj)*dyC(I,J,bi,bj)
          rAz(I,J,bi,bj) = dxV(I,J,bi,bj)*dyU(I,J,bi,bj)
          tanPhiAtU(I,J,bi,bj) = 0. _d 0
          tanPhiAtV(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _EXCH_XY_R4 (rA       , myThid )
      _EXCH_XY_R4 (rAw      , myThid )
      _EXCH_XY_R4 (rAs      , myThid )
      _EXCH_XY_R4 (tanPhiAtU , myThid )
      _EXCH_XY_R4 (tanPhiAtV , myThid )

C
      RETURN
      END
