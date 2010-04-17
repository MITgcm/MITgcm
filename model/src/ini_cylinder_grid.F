C $Header: /u/gcmpack/MITgcm/model/src/ini_cylinder_grid.F,v 1.6 2010/04/17 18:25:12 jmc Exp $
C $Name:  $

c#include "PACKAGES_CONFIG.h"
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
C     | Under the spherical polar grid mode primitive distances
C     | in X is in degrees and Y in meters.
C     | Distance in Z are in m or Pa
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
c#ifdef ALLOW_EXCH2
c#include "W2_EXCH2_SIZE.h"
c#include "W2_EXCH2_TOPOLOGY.h"
c#include "W2_EXCH2_PARAMS.h"
c#endif /* ALLOW_EXCH2 */

C    !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     xG0,yG0 :: coordinate of South-West tile-corner
C     iG, jG  :: Global coordinate index. Usually used to hold
C             :: the south-west global coordinate of a tile.
C     bi,bj   :: tile indices
C     i, j    :: loop counters
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER i, j
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
C     and jGl(j,bj) since we take mod(a+OLx*Nx,Nx) and mod(b+OLy*Ny,Ny).
C     But in case a or b is negative, then the FORTRAN function "mod"
C     does not return the matematical value of the "modulus" function,
C     and this is not good for your purpose.
C     This is why I add +OLx*Nx and +OLy*Ny to be sure that the 1rst
C     argument of the mod function is positive.
      INTEGER iGl,jGl
      iGl(i,bi) = 1+MOD(myXGlobalLo-1+(bi-1)*sNx+i+Olx*Nx-1,Nx)
      jGl(j,bj) = 1+MOD(myYGlobalLo-1+(bj-1)*sNy+j+Oly*Ny-1,Ny)
c#ifdef ALLOW_EXCH2
c      INTEGER tN
c#endif /* ALLOW_EXCH2 */
CEOP

C     For each tile ...
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--     "Global" index (place holder)
        jG = myYGlobalLo + (bj-1)*sNy
        iG = myXGlobalLo + (bi-1)*sNx
c#ifdef ALLOW_EXCH2
c        IF ( W2_useE2ioLayOut ) THEN
cC- note: does not work for non-uniform delX or delY
c          tN = W2_myTileList(bi,bj)
c          iG = exch2_txGlobalo(tN)
c          jG = exch2_tyGlobalo(tN)
c        ENDIF
c#endif /* ALLOW_EXCH2 */

C--   First find coordinate of tile corner (meaning outer corner of halo)
        xG0 = xgOrigin
C       Find the X-coordinate of the outer grid-line of the "real" tile
        DO i=1, iG-1
         xG0 = xG0 + delX(i)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO i=1, Olx
         xG0 = xG0 - delX( 1+mod(Olx*Nx-1+iG-i,Nx) )
        ENDDO
C       Find the Y-coordinate of the outer grid-line of the "real" tile
        yG0 = ygOrigin
        DO j=1, jG-1
         yG0 = yG0 + delY(j)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO j=1, Oly
         yG0 = yG0 - delY( 1+mod(Oly*Ny-1+jG-j,Ny) )
        ENDDO

C--     Calculate coordinates of cell corners for N+1 grid-lines
        DO j=1-Oly,sNy+Oly +1
         xGloc(1-Olx,j) = xG0
         DO i=1-Olx,sNx+Olx
          xGloc(i+1,j) = xGloc(i,j) + delX( iGl(i,bi) )
         ENDDO
        ENDDO
        DO i=1-Olx,sNx+Olx +1
         yGloc(i,1-Oly) = yG0
         DO j=1-Oly,sNy+Oly
          yGloc(i,j+1) = yGloc(i,j) + delY( jGl(j,bj) )
         ENDDO
        ENDDO

C--     Make a permanent copy of [xGloc,yGloc] in [xG,yG]
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xG(i,j,bi,bj) = xGloc(i,j)
          yG(i,j,bi,bj) = yGloc(i,j)
         ENDDO
        ENDDO

C--     Calculate [xC,yC], coordinates of cell centers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         by averaging
          xC(i,j,bi,bj) = 0.25 _d 0*(
     &     xGloc(i,j)+xGloc(i+1,j)+xGloc(i,j+1)+xGloc(i+1,j+1) )
          yC(i,j,bi,bj) = 0.25 _d 0*(
     &     yGloc(i,j)+yGloc(i+1,j)+yGloc(i,j+1)+yGloc(i+1,j+1) )
         ENDDO
        ENDDO

C--     Calculate [dxF,dyF], lengths between cell faces (through center)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          thisRad = yC(i,j,bi,bj)
          dtheta = delX( iGl(i,bi) )
          dxF(i,j,bi,bj) = thisRad*dtheta*deg2rad
          dyF(i,j,bi,bj) = delY( jGl(j,bj) )
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          thisRad = 0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dtheta = delX( iGl(i,bi) )
          dxG(i,j,bi,bj) = thisRad*dtheta*deg2rad
          dyG(i,j,bi,bj) = delY( jGl(j,bj) )
         ENDDO
        ENDDO

C--     The following arrays are not defined in some parts of the halo
C       region. We set them to zero here for safety. If they are ever
C       referred to, especially in the denominator then it is a mistake!
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          dxC(i,j,bi,bj) = 0.
          dyC(i,j,bi,bj) = 0.
          dxV(i,j,bi,bj) = 0.
          dyU(i,j,bi,bj) = 0.
          rAw(i,j,bi,bj) = 0.
          rAs(i,j,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Calculate [dxC], zonal length between cell centers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging
          dxC(i,j,bi,bj) = 0.5 _d 0*(dxF(i,j,bi,bj)+dxF(i-1,j,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO j=1-Oly+1,sNy+Oly ! NOTE range
         DO i=1-Olx,sNx+Olx
C         by averaging
          dyC(i,j,bi,bj) = 0.5 _d 0*(dyF(i,j,bi,bj)+dyF(i,j-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO j=1-Oly+1,sNy+Oly ! NOTE range
         DO i=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging (method I)
          dxV(i,j,bi,bj) = 0.5 _d 0*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
          dyU(i,j,bi,bj) = 0.5 _d 0*(dyG(i,j,bi,bj)+dyG(i,j-1,bi,bj))
         ENDDO
        ENDDO

C--     Calculate vertical face area
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C-      All r(dr)(dtheta)
          rA (i,j,bi,bj) = dxF(i,j,bi,bj)*dyF(i,j,bi,bj)
          rAw(i,j,bi,bj) = dxC(i,j,bi,bj)*dyG(i,j,bi,bj)
          rAs(i,j,bi,bj) = dxG(i,j,bi,bj)*dyC(i,j,bi,bj)
          rAz(i,j,bi,bj) = dxV(i,j,bi,bj)*dyU(i,j,bi,bj)
C--     Set trigonometric terms & grid orientation:
          tanPhiAtU(i,j,bi,bj) = 0.
          tanPhiAtV(i,j,bi,bj) = 0.
          angleCosC(i,j,bi,bj) = 1.
          angleSinC(i,j,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Cosine(lat) scaling
        DO j=1-OLy,sNy+OLy
         cosFacU(j,bi,bj)=1.
         cosFacV(j,bi,bj)=1.
         sqcosFacU(j,bi,bj)=1.
         sqcosFacV(j,bi,bj)=1.
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
