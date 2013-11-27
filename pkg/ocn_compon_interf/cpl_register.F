C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_register.F,v 1.3 2013/11/27 21:54:45 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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
#include "OCNIDS.h"
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
      INTEGER iG0, jG0
      INTEGER bi, bj
      INTEGER j, nTiles
      INTEGER iReg(6,nSx*nSy)
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */

C--   Register with the MIT coupler
      j = 0
      DO bj=1,nSy
       DO bi=1,nSx
        j = j+1
        iG0 = myXGlobalLo+(bi-1)*sNx
        jG0 = myYGlobalLo+(bj-1)*sNy
#ifdef ALLOW_EXCH2
        IF ( W2_useE2ioLayOut ) THEN
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
