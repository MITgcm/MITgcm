C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_register.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_REGISTER
C     *==========================================================*
C     | SUBROUTINE CPL_REGISTER                                  |
C     | o Routine controlling registration with MIT coupler.     |
C     | - Oceanic version -                                      |
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OCNIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
#ifdef COMPONENT_MODULE
C     Arrays for passing attributes to the coupler
      INTEGER iReg(4,nSx,nSy)
      Real*4  rReg(4,nSx,nSy)
C     bi, bj, J - Loop counters
      INTEGER bi,bj,J
C     Base coordinates of a tile on the global grid.
      INTEGER iG0, jG0
#endif


C--   Register with the MIT coupler
#ifdef COMPONENT_MODULE
      DO bj=1,nSy
       DO bi=1,nSx
        jG0 = myYGlobalLo+(bj-1)*sNy
        iG0 = myXGlobalLo+(bi-1)*sNx
        IREG(1,bi,bj)=sNx
        IREG(2,bi,bj)=sNy
        IREG(3,bi,bj)=iG0
        IREG(4,bi,bj)=jG0
       ENDDO
      ENDDO
      CALL MITCOMPONENT_TILE_REGISTER( nSx, nSy, ireg, rreg )
#endif


      RETURN
      END
