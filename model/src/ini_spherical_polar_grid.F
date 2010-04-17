C $Header: /u/gcmpack/MITgcm/model/src/ini_spherical_polar_grid.F,v 1.27 2010/04/17 18:25:12 jmc Exp $
C $Name:  $

c#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#undef  USE_BACKWARD_COMPATIBLE_GRID

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
c#ifdef ALLOW_EXCH2
c#include "W2_EXCH2_SIZE.h"
c#include "W2_EXCH2_TOPOLOGY.h"
c#include "W2_EXCH2_PARAMS.h"
c#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id Number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     xG0,yG0 :: coordinate of South-West tile-corner
C     iG, jG  :: Global coordinate index. Usually used to hold
C             :: the south-west global coordinate of a tile.
C     lat     :: Temporary variables used to hold latitude values.
C     bi,bj   :: tile indices
C     i, j    :: loop counters
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER i,  j
      _RL lat, dlat, dlon, xG0, yG0

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
         xG0 = xG0 - delX( 1+MOD(Olx*Nx-1+iG-i,Nx) )
        ENDDO
C       Find the Y-coordinate of the outer grid-line of the "real" tile
        yG0 = ygOrigin
        DO j=1, jG-1
         yG0 = yG0 + delY(j)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO j=1, Oly
         yG0 = yG0 - delY( 1+MOD(Oly*Ny-1+jG-j,Ny) )
        ENDDO

C--     Calculate coordinates of cell corners for N+1 grid-lines
        DO j=1-Oly,sNy+Oly +1
         xGloc(1-Olx,j) = xG0
         DO i=1-Olx,sNx+Olx
c         xGloc(i+1,j) = xGloc(i,j) + delX(1+mod(Nx-1+iG-1+i,Nx))
          xGloc(i+1,j) = xGloc(i,j) + delX( iGl(i,bi) )
         ENDDO
        ENDDO
        DO i=1-Olx,sNx+Olx +1
         yGloc(i,1-Oly) = yG0
         DO j=1-Oly,sNy+Oly
c         yGloc(i,j+1) = yGloc(i,j) + delY(1+mod(Ny-1+jG-1+j,Ny))
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
C         by averaging
c         dxF(i,j,bi,bj) = 0.5*(dxG(i,j,bi,bj)+dxG(i,j+1,bi,bj))
c         dyF(i,j,bi,bj) = 0.5*(dyG(i,j,bi,bj)+dyG(i+1,j,bi,bj))
C         by formula
          lat = yC(i,j,bi,bj)
          dlon = delX( iGl(i,bi) )
          dlat = delY( jGl(j,bj) )
          dxF(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          dxF(i,j,bi,bj) = delX(iGl(i,bi))*deg2rad*rSphere*
     &                     COS(yC(i,j,bi,bj)*deg2rad)
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
          dyF(i,j,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxG,dyG], lengths along cell boundaries
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         by averaging
c         dxG(i,j,bi,bj) = 0.5*(dxF(i,j,bi,bj)+dxF(i,j-1,bi,bj))
c         dyG(i,j,bi,bj) = 0.5*(dyF(i,j,bi,bj)+dyF(i-1,j,bi,bj))
C         by formula
          lat = 0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dlon = delX( iGl(i,bi) )
          dlat = delY( jGl(j,bj) )
          dxG(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
          if (dxG(i,j,bi,bj).LT.1.) dxG(i,j,bi,bj)=0.
          dyG(i,j,bi,bj) = rSphere*dlat*deg2rad
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
C         by formula
c         lat = 0.5*(yC(i,j,bi,bj)+yC(i-1,j,bi,bj))
c         dlon = 0.5*(delX( iGl(i,bi) ) + delX( iGl(i-1,bi) ))
c         dxC(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
C         by difference
c         lat = 0.5*(yC(i,j,bi,bj)+yC(i-1,j,bi,bj))
c         dlon = (xC(i,j,bi,bj)+xC(i-1,j,bi,bj))
c         dxC(i,j,bi,bj) = rSphere*COS(deg2rad*lat)*dlon*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dyC], meridional length between cell centers
        DO j=1-Oly+1,sNy+Oly ! NOTE range
         DO i=1-Olx,sNx+Olx
C         by averaging
          dyC(i,j,bi,bj) = 0.5 _d 0*(dyF(i,j,bi,bj)+dyF(i,j-1,bi,bj))
C         by formula
c         dlat = 0.5*(delY( jGl(j,bj) ) + delY( jGl(j-1,bj) ))
c         dyC(i,j,bi,bj) = rSphere*dlat*deg2rad
C         by difference
c         dlat = (yC(i,j,bi,bj)+yC(i,j-1,bi,bj))
c         dyC(i,j,bi,bj) = rSphere*dlat*deg2rad
         ENDDO
        ENDDO

C--     Calculate [dxV,dyU], length between velocity points (through corners)
        DO j=1-Oly+1,sNy+Oly ! NOTE range
         DO i=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging (method I)
          dxV(i,j,bi,bj) = 0.5 _d 0*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
          dyU(i,j,bi,bj) = 0.5 _d 0*(dyG(i,j,bi,bj)+dyG(i,j-1,bi,bj))
C         by averaging (method II)
c         dxV(i,j,bi,bj) = 0.5*(dxG(i,j,bi,bj)+dxG(i-1,j,bi,bj))
c         dyU(i,j,bi,bj) = 0.5*(dyC(i,j,bi,bj)+dyC(i-1,j,bi,bj))
         ENDDO
        ENDDO

C--     Calculate vertical face area (tracer cells)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          dlon=delX( iGl(i,bi) )
          dlat=delY( jGl(j,bj) )
          rA(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *ABS( SIN((lat+dlat)*deg2rad)-SIN(lat*deg2rad) )
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          lat=yC(i,j,bi,bj)-delY( jGl(j,bj) )*0.5 _d 0
          lon=yC(i,j,bi,bj)+delY( jGl(j,bj) )*0.5 _d 0
          rA(i,j,bi,bj) = dyF(i,j,bi,bj)
     &    *rSphere*(SIN(lon*deg2rad)-SIN(lat*deg2rad))
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
         ENDDO
        ENDDO

C--     Calculate vertical face area (u cells)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx+1,sNx+Olx ! NOTE range
C         by averaging
          rAw(i,j,bi,bj) = 0.5 _d 0*(rA(i,j,bi,bj)+rA(i-1,j,bi,bj))
C         by formula
c         lat=yGloc(i,j)
c         dlon=0.5*( delX( iGl(i,bi) ) + delX( iGl(i-1,bi) ) )
c         dlat=delY( jGl(j,bj) )
c         rAw(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
c    &        *abs( sin((lat+dlat)*deg2rad)-sin(lat*deg2rad) )
         ENDDO
        ENDDO

C--     Calculate vertical face area (v cells)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         by formula
          lat=yC(i,j,bi,bj)
          dlon=delX( iGl(i,bi) )
          dlat=0.5 _d 0*( delY( jGl(j,bj) ) + delY( jGl(j-1,bj) ) )
          rAs(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &        *ABS( SIN(lat*deg2rad)-SIN((lat-dlat)*deg2rad) )
#ifdef    USE_BACKWARD_COMPATIBLE_GRID
          lon=yC(i,j,bi,bj)-delY( jGl(j,bj) )
          lat=yC(i,j,bi,bj)
          rAs(i,j,bi,bj) = rSphere*delX(iGl(i,bi))*deg2rad
     &    *rSphere*(SIN(lat*deg2rad)-SIN(lon*deg2rad))
#endif    /* USE_BACKWARD_COMPATIBLE_GRID */
          IF (ABS(lat).GT.90..OR.ABS(lat-dlat).GT.90.) rAs(i,j,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate vertical face area (vorticity points)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         by formula
          lat =0.5 _d 0*(yGloc(i,j)+yGloc(i,j+1))
          dlon=0.5 _d 0*( delX( iGl(i,bi) ) + delX( iGl(i-1,bi) ) )
          dlat=0.5 _d 0*( delY( jGl(j,bj) ) + delY( jGl(j-1,bj) ) )
          rAz(i,j,bi,bj) = rSphere*rSphere*dlon*deg2rad
     &     *ABS( SIN(lat*deg2rad)-SIN((lat-dlat)*deg2rad) )
          IF (ABS(lat).GT.90..OR.ABS(lat-dlat).GT.90.) rAz(i,j,bi,bj)=0.
         ENDDO
        ENDDO

C--     Calculate trigonometric terms & grid orientation:
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i,j+1))
          tanPhiAtU(i,j,bi,bj)=TAN(lat*deg2rad)
          lat=0.5 _d 0*(yGloc(i,j)+yGloc(i+1,j))
          tanPhiAtV(i,j,bi,bj)=TAN(lat*deg2rad)
          angleCosC(i,j,bi,bj) = 1.
          angleSinC(i,j,bi,bj) = 0.
         ENDDO
        ENDDO

C--     Cosine(lat) scaling
        DO j=1-OLy,sNy+OLy
         jG = myYGlobalLo + (bj-1)*sNy + j-1
         jG = MIN(MAX(1,jG),Ny)
         IF (cosPower.NE.0.) THEN
          cosFacU(j,bi,bj)=COS(yC(1,j,bi,bj)*deg2rad)
     &                    **cosPower
          cosFacV(j,bi,bj)=COS((yC(1,j,bi,bj)-0.5*delY(jG))*deg2rad)
     &                    **cosPower
          cosFacU(j,bi,bj)=ABS(cosFacU(j,bi,bj))
          cosFacV(j,bi,bj)=ABS(cosFacV(j,bi,bj))
          sqcosFacU(j,bi,bj)=SQRT(cosFacU(j,bi,bj))
          sqcosFacV(j,bi,bj)=SQRT(cosFacV(j,bi,bj))
         ELSE
          cosFacU(j,bi,bj)=1.
          cosFacV(j,bi,bj)=1.
          sqcosFacU(j,bi,bj)=1.
          sqcosFacV(j,bi,bj)=1.
         ENDIF
        ENDDO

       ENDDO ! bi
      ENDDO ! bj

      IF ( rotateGrid ) THEN
       CALL ROTATE_SPHERICAL_POLAR_GRID( xC, yC, myThid )
       CALL ROTATE_SPHERICAL_POLAR_GRID( xG, yG, myThid )
       CALL CALC_ANGLES( myThid )
      ENDIF

      RETURN
      END
