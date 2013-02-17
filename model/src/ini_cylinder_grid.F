C $Header: /u/gcmpack/MITgcm/model/src/ini_cylinder_grid.F,v 1.8 2013/02/17 02:18:16 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CYLINDER_GRID
C     !INTERFACE:
      SUBROUTINE INI_CYLINDER_GRID( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_CYLINDER_GRID
C     | o Initialise model coordinate system arrays
C     *==========================================================*
C     | These arrays are used throughout the code in evaluating
C     | gradients, integrals and spatial avarages. This routine
C     | is called separately by each thread and initialise only
C     | the region of the domain it is "responsible" for.
C     | Under the cylindrical grid mode primitive distance
C     | in X is in degrees and distance in Y is in meters.
C     | Distance in Z are in m or Pa depending on the vertical
C     | gridding mode.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C    !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id number
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
      INTEGER bi, bj
      INTEGER i,  j
      INTEGER gridNx, gridNy
c     _RL lat, dlat, dlon
c     _RL xG0, yG0
      _RL dtheta, thisRad
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
          thisRad = yC(i,j,bi,bj)
          dtheta = delXloc(i)
          dxF(i,j,bi,bj) = thisRad*dtheta*deg2rad
          dyF(i,j,bi,bj) = delYloc(j)
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          thisRad = 0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dtheta = delXloc(i)
          dxG(i,j,bi,bj) = thisRad*dtheta*deg2rad
          dyG(i,j,bi,bj) = delYloc(j)
         ENDDO
        ENDDO

C--     The following arrays are not defined in some parts of the halo
C       region. We set them to zero here for safety.
C       Note: this is now done earlier in main S/R INI_GRID

C--     Calculate [dxC], zonal length between cell centers
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx ! NOTE range
C         by averaging
          dxC(i,j,bi,bj) = 0.5 _d 0*(dxF(i,j,bi,bj)+dxF(i-1,j,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO j=1-OLy+1,sNy+OLy ! NOTE range
         DO i=1-OLx,sNx+OLx
C         by averaging
          dyC(i,j,bi,bj) = 0.5 _d 0*(dyF(i,j,bi,bj)+dyF(i,j-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO j=1-OLy+1,sNy+OLy ! NOTE range
         DO i=1-OLx+1,sNx+OLx ! NOTE range
C         by averaging (method I)
          dxV(i,j,bi,bj) = 0.5 _d 0*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
          dyU(i,j,bi,bj) = 0.5 _d 0*(dyG(i,j,bi,bj)+dyG(i,j-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate vertical face area
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C-      All r(dr)(dtheta)
          rA (i,j,bi,bj) = dxF(i,j,bi,bj)*dyF(i,j,bi,bj)
          rAw(i,j,bi,bj) = dxC(i,j,bi,bj)*dyG(i,j,bi,bj)
          rAs(i,j,bi,bj) = dxG(i,j,bi,bj)*dyC(i,j,bi,bj)
          rAz(i,j,bi,bj) = dxV(i,j,bi,bj)*dyU(i,j,bi,bj)
C--     Set trigonometric terms & grid orientation:
C       Note: this is now done earlier in main S/R INI_GRID
c         tanPhiAtU(i,j,bi,bj) = 0.
c         tanPhiAtV(i,j,bi,bj) = 0.
c         angleCosC(i,j,bi,bj) = 1.
c         angleSinC(i,j,bi,bj) = 0.
         ENDDO
        ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
