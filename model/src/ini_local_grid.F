C $Header: /u/gcmpack/MITgcm/model/src/ini_local_grid.F,v 1.1 2011/12/12 19:01:01 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_EXCH2
# include "W2_OPTIONS.h"
#endif /* ALLOW_EXCH2 */

CBOP
C     !ROUTINE: INI_LOCAL_GRID
C     !INTERFACE:
      SUBROUTINE INI_LOCAL_GRID(
     O                           xGloc, yGloc,
     O                           delXloc, delYloc,
     O                           gridNx, gridNy,
     I                           bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_LOCAL_GRID
C     | o Initialise model tile-local horizontal grid
C     *==========================================================*
C     | Set local grid-point location (xGloc & yGloc) and
C     |  local grid-point spacing (delXloc,delYloc) keeping the
C     |  same units as grid-spacing input parameter (delX,delY
C     |  and xgOrigin,ygOrigin).
C     | This tile-local mesh setting will be used to build
C     |  the horizontal model grid, according to the selected
C     |  grid option (cartesian, spherical_polar or cylindrical)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
#include "SET_GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     xGloc   :: mesh corner-point location (local "Long" real array type)
C     yGloc   :: mesh corner-point location (local "Long" real array type)
C     delXloc :: mesh spacing in X direction
C     delYloc :: mesh spacing in Y direction
C     gridNx  :: mesh total grid-point number in X direction
C     gridNy  :: mesh total grid-point number in Y direction
C     bi, bj  :: tile indices
C     myThid  :: my Thread Id Number

C NOTICE the extended range of indices!!
      _RL xGloc(1-OLx:sNx+OLx+1,1-OLy:sNy+OLy+1)
      _RL yGloc(1-OLx:sNx+OLx+1,1-OLy:sNy+OLy+1)
C NOTICE the extended range of indices!!
      _RL delXloc(0-OLx:sNx+OLx)
      _RL delYloc(0-OLy:sNy+OLy)
      INTEGER gridNx, gridNy
      INTEGER bi, bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     xG0,yG0 :: coordinate of South-West tile-corner
C     iG0,jG0 :: Tile base X and Y indices within global index space
C     i, j    :: loop counters
      INTEGER iG0, jG0
      INTEGER i, j
      _RL xG0, yG0
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */

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
C     with small domain (where OLy > Ny, as for instance, zonal-average
C     case):
C     This has no effect on the acuracy of the evaluation of iGl(I,bi)
C     and jGl(j,bj) since we take mod(a+OLx*Nx,Nx) and mod(b+OLy*Ny,Ny).
C     But in case a or b is negative, then the FORTRAN function "mod"
C     does not return the matematical value of the "modulus" function,
C     and this is not good for your purpose.
C     This is why I add +OLx*Nx and +OLy*Ny to be sure that the 1rst
C     argument of the mod function is positive.
c     INTEGER iGl,jGl
c     iGl(i,bi) = 1+MOD(iG0+i-1+OLx*gridNx,gridNx)
c     jGl(j,bj) = 1+MOD(jG0+j-1+OLy*gridNy,gridNy)
CEOP

#ifdef ALLOW_EXCH2
      gridNx = exch2_mydNx(1)
      gridNy = exch2_mydNy(1)
#else /* ALLOW_EXCH2 */
      gridNx = Nx
      gridNy = Ny
#endif /* ALLOW_EXCH2 */

c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
C     For this tile ...

C--   Set current tile base X and base Y indices within global index space
C     e.g., local indices of tile south-west corner grid point are (1,1)
C           and global indices are 1+iG0, 1+jG0
#ifdef ALLOW_EXCH2
        tN = W2_myTileList(bi,bj)
        iG0 = exch2_tBasex(tN)
        jG0 = exch2_tBasey(tN)
#else  /* ALLOW_EXCH2 */
        iG0 = myXGlobalLo - 1 + (bi-1)*sNx
        jG0 = myYGlobalLo - 1 + (bj-1)*sNy
#endif /* ALLOW_EXCH2 */

C--   First find coordinate of tile corner (meaning outer corner of halo)
        xG0 = xgOrigin
C       Find the X-coordinate of the outer grid-line of the "real" tile
        DO i=1, iG0
         xG0 = xG0 + delX(i)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO i=1, OLx
         xG0 = xG0 - delX( 1+MOD(iG0-i+OLx*gridNx,gridNx) )
        ENDDO
C       Find the Y-coordinate of the outer grid-line of the "real" tile
        yG0 = ygOrigin
        DO j=1, jG0
         yG0 = yG0 + delY(j)
        ENDDO
C       Back-step to the outer grid-line of the "halo" region
        DO j=1, OLy
         yG0 = yG0 - delY( 1+MOD(jG0-j+OLy*gridNy,gridNy) )
        ENDDO

C--     Make a local copy of current-tile grid-spacing
        DO i=0-OLx,sNx+OLx
          delXloc(i) = delX( 1+MOD(iG0+i-1+OLx*gridNx,gridNx) )
        ENDDO
        DO j=0-OLy,sNy+OLy
          delYloc(j) = delY( 1+MOD(jG0+j-1+OLy*gridNy,gridNy) )
        ENDDO

C--     Calculate coordinates of cell corners for N+1 grid-lines
        DO j=1-OLy,sNy+OLy +1
         xGloc(1-OLx,j) = xG0
         DO i=1-OLx,sNx+OLx
          xGloc(i+1,j) = xGloc(i,j) + delXloc(i)
         ENDDO
        ENDDO
        DO i=1-OLx,sNx+OLx +1
         yGloc(i,1-OLy) = yG0
         DO j=1-OLy,sNy+OLy
          yGloc(i,j+1) = yGloc(i,j) + delYloc(j)
         ENDDO
        ENDDO

C--   end bi,bj loops
c      ENDDO
c     ENDDO

      RETURN
      END
