#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_REGISTER

C !INTERFACE:
      SUBROUTINE CPL_REGISTER

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_REGISTER
C     | o Routine controlling registration with MIT coupler.
C     | - Oceanic version -
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
# include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid    :: Thread number for this instance of the routine
c     INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj    :: Tile indices
C     j         :: Loop counter
C     iG0, jG0  :: Base coordinates of a tile on the global grid.
C     iReg      :: Array for passing attributes to the coupler
C     x,ySize   :: expected size of the coupler 2-D maps
      INTEGER iG0, jG0
      INTEGER bi, bj
      INTEGER j, nTiles
      INTEGER iReg(6,nSx*nSy)
c     INTEGER xSize, ySize
#ifdef ALLOW_EXCH2
      INTEGER tN
      LOGICAL useExch2LayOut
#endif /* ALLOW_EXCH2 */

C--   Set dimensions of coupler 2-D maps:
c     xSize = Nx
c     ySize = Ny

#ifdef ALLOW_EXCH2
C-- Note: current coupler mapping relies only on i0,j0,nx,ny ; this prevent to
C     map tiles to Exch-2 global map when faces do not fit in global-IO-array
C     (see, e.g., gather/scatter_2d routines in eesupp/src, case where
C      iGjLoc <> 0 or jGjLoc <> 1 ).
      useExch2LayOut = .FALSE.
      IF ( W2_useE2ioLayOut ) THEN
        useExch2LayOut = .TRUE.
        DO tN=1,exch2_nTiles
          IF   ( exch2_mydNx(tN) .GT. exch2_global_Nx ) THEN
           useExch2LayOut = .FALSE.
C-         face x-size larger than glob-size : fold it
c              iGjLoc = 0
c              jGjLoc = exch2_mydNx(tN) / xSize
          ELSEIF ( exch2_tNy(tN) .GT. exch2_global_Ny ) THEN
           useExch2LayOut = .FALSE.
C-         tile y-size larger than glob-size : make a long line
c              iGjLoc = exch2_mydNx(tN)
c              jGjLoc = 0
          ELSE
C-          default (face fit into global-IO-array)
c             iGjLoc = 0
c             jGjLoc = 1
          ENDIF
        ENDDO
      ENDIF
c     IF ( useExch2LayOut ) THEN
c       xSize = exch2_global_Nx
c       ySize = exch2_global_Ny
c     ENDIF
C-- Note: should check that xSize,ySize match with coupler Nx_ocn,Ny_ocn
#endif /* ALLOW_EXCH2 */

C--   Register with the MIT coupler
      j = 0
      DO bj=1,nSy
       DO bi=1,nSx
        j = j+1
        iG0 = myXGlobalLo+(bi-1)*sNx
        jG0 = myYGlobalLo+(bj-1)*sNy
#ifdef ALLOW_EXCH2
        IF ( useExch2LayOut ) THEN
          tN = W2_myTileList(bi,bj)
          iG0 = exch2_txGlobalo(tN)
          jG0 = exch2_tyGlobalo(tN)
        ENDIF
#endif /* ALLOW_EXCH2 */
        iReg(1,j) = bi
        iReg(2,j) = bj
        iReg(3,j) = sNx
        iReg(4,j) = sNy
        iReg(5,j) = iG0
        iReg(6,j) = jG0
       ENDDO
      ENDDO
      nTiles = nSx*nSy
      CALL MITCOMPONENT_TILE_REGISTER( nTiles, iReg )
#endif /* COMPONENT_MODULE */

      RETURN
      END
