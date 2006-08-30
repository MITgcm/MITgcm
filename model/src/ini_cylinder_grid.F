C $Header: /u/gcmpack/MITgcm/model/src/ini_cylinder_grid.F,v 1.4 2006/08/30 17:12:51 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CYLINDER_GRID
C     !INTERFACE:
      SUBROUTINE INI_CYLINDER_GRID( myThid )
C     !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE INI_CYLINDER_GRID
C     | o Initialise model coordinate system arrays              |
C     |==========================================================|
C     | These arrays are used throughout the code in evaluating  |
C     | gradients, integrals and spatial avarages. This routine  |
C     | is called separately by each thread and initialise only  |
C     | the region of the domain it is "responsible" for.        |
C     | Under the spherical polar grid mode primitive distances  |
C     | in X is in degrees and Y in meters.                      |
C     | Distance in Z are in m or Pa                             |
C     | depending on the vertical gridding mode.                 |
C     \==========================================================/
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
C     myThid -  Number of this instance of INI_CYLINDER
      INTEGER myThid
CEndOfInterface

C     !LOCAL VARIABLES:
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
      _RL dtheta, thisRad, xG0, yG0

C     "Long" real for temporary coordinate calculation
C      NOTICE the extended range of indices!!
      _RL xGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)
      _RL yGloc(1-Olx:sNx+Olx+1,1-Oly:sNy+Oly+1)

C     The functions iGl, jGl return the "global" index with valid values beyond
C     halo regions
C     cnh wrote:
C     >    I dont understand why we would ever have to multiply the
C     >    overlap by the total domain size e.g
C     >    OLx*Nx, OLy*Ny.
C     >    Can anybody explain? Lines are in ini_spherical_polar_grid.F.
C     >    Surprised the code works if its wrong, so I am puzzled.        
C     jmc replied:
C     Yes, I can explain this since I put this modification to work
C     with small domain (where Oly > Ny, as for instance, zonal-average
C     case):
C     This has no effect on the acuracy of the evaluation of iGl(I,bi)
C     and jGl(J,bj) since we take mod(a+OLx*Nx,Nx) and mod(b+OLy*Ny,Ny).
C     But in case a or b is negative, then the FORTRAN function "mod"
C     does not return the matematical value of the "modulus" function,
C     and this is not good for your purpose.
C     This is why I add +OLx*Nx and +OLy*Ny to be sure that the 1rst 
C     argument of the mod function is positive.
      INTEGER iGl,jGl
      iGl(I,bi) = 1+mod(myXGlobalLo-1+(bi-1)*sNx+I+Olx*Nx-1,Nx)
      jGl(J,bj) = 1+mod(myYGlobalLo-1+(bj-1)*sNy+J+Oly*Ny-1,Ny)
CEOP


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
          xGloc(I+1,J) = xGloc(I,J) + delX( iGl(I,bi) )
         ENDDO
        ENDDO
        DO I=1-Olx,sNx+Olx +1
         yGloc(I,1-Oly) = yG0
         DO J=1-Oly,sNy+Oly
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
          thisRad = yC(I,J,bi,bj)
          dtheta = delX( iGl(I,bi) )
          dXF(I,J,bi,bj) = thisRad*dtheta*deg2rad
          dYF(I,J,bi,bj) = delY( jGl(J,bj) )
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          thisRad = 0.5*(yGloc(I,J)+yGloc(I+1,J))
          dtheta = delX( iGl(I,bi) )
          dXG(I,J,bi,bj) = thisRad*dtheta*deg2rad
          dYG(I,J,bi,bj) = delY( jGl(J,bj) )
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
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx,sNx+Olx
C         by averaging
          dYC(I,J,bi,bj) = 0.5*(dYF(I,J,bi,bj)+dYF(I,J-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO J=1-Oly+1,sNy+Oly ! NOTE range
         DO I=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging (method I)
          dXV(I,J,bi,bj) = 0.5*(dXG(I,J,bi,bj)+dXG(I-1,J,bi,bj))
          dYU(I,J,bi,bj) = 0.5*(dYG(I,J,bi,bj)+dYG(I,J-1,bi,bj))
         ENDDO
        ENDDO



C--     Calculate vertical face area 
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C-      All r(dr)(dtheta)
          rA (I,J,bi,bj) = dxF(I,J,bi,bj)*dyF(I,J,bi,bj)
          rAw(I,J,bi,bj) = dxC(I,J,bi,bj)*dyG(I,J,bi,bj)
          rAs(I,J,bi,bj) = dxG(I,J,bi,bj)*dyC(I,J,bi,bj)
          rAz(I,J,bi,bj) = dxV(I,J,bi,bj)*dyU(I,J,bi,bj)
C--     Set trigonometric terms & grid orientation:
          tanPhiAtU(I,J,bi,bj) = 0.
          tanPhiAtV(I,J,bi,bj) = 0.
          angleCosC(I,J,bi,bj) = 1.
          angleSinC(I,J,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Cosine(lat) scaling
        DO J=1-OLy,sNy+OLy
         cosFacU(J,bi,bj)=1.
         cosFacV(J,bi,bj)=1.
         sqcosFacU(J,bi,bj)=1.
         sqcosFacV(J,bi,bj)=1.
        ENDDO

       ENDDO ! bi
      ENDDO ! bj

C--   Set default (=whole domain) for where relaxation to climatology applies
      IF ( latBandClimRelax.EQ.UNSET_RL ) THEN
        _BEGIN_MASTER(myThid)
        latBandClimRelax = 0.
        DO j=1,Ny
          latBandClimRelax = latBandClimRelax + delY(j)
        ENDDO
        latBandClimRelax = latBandClimRelax*3. _d 0
        _END_MASTER(myThid)
      ENDIF

      RETURN
      END
