C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/cpl_register.F,v 1.2 2009/12/25 19:43:43 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_REGISTER

C !INTERFACE:
      SUBROUTINE CPL_REGISTER

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_REGISTER
C     | o Routine controlling registration with MIT coupler.
C     | - Atmospheric version -
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMIDS.h"

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
C     iReg,rReg :: Arrays for passing attributes to the coupler
      INTEGER iG0, jG0
      INTEGER bi, bj
c     INTEGER j
      INTEGER iReg(4,nSx,nSy)
      Real*4  rReg(4,nSx,nSy)

C--   Register with the MIT coupler
      DO bj=1,nSy
       DO bi=1,nSx
        jG0 = myYGlobalLo+(bj-1)*sNy
        iG0 = myXGlobalLo+(bi-1)*sNx
        iReg(1,bi,bj) = sNx
        iReg(2,bi,bj) = sNy
        iReg(3,bi,bj) = iG0
        iReg(4,bi,bj) = jG0
       ENDDO
      ENDDO
      CALL MITCOMPONENT_TILE_REGISTER( nSx, nSy, iReg, rReg )
#endif /* COMPONENT_MODULE */

      RETURN
      END
