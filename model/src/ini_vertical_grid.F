C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.7 1998/11/06 22:44:48 cnh Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_VERTICAL_GRID( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_VERTICAL_GRID                             |
C     | o Initialise vertical gridding arrays                    |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     K
      INTEGER K

C     Calculate depths of centers and interfaces
      rF(1) = 0. _d 0
      DO K=1,Nr
       drF(K)     = delR(K)
       rF(K+1) = rF(K)-rkFac*delR(K)
C      drF(K)     = delZ(K)
C      rF(K+1)    = rF(K)-delZ(K)
      ENDDO
      drC(1)      = delR(1) * 0.5 _d 0
      rC(1)       = rf(1)-rkFac*delR(1) * 0.5 _d 0
C     drC(1)      = delZ(1) * 0.5 _d 0
C     rC(1)       = -delZ(1) * 0.5 _d 0
      DO K=2,Nr
       drC(K)     = 0.5 _d 0 *(delR(K-1)+delR(K))
       rC(K)      = rC(K-1) - rkFac*drC(K)
C      drC(K)     = 0.5 _d 0 *(delZ(K-1)+delZ(K))
C      rC(K)      = rC(K-1) - drC(K)
      ENDDO
      DO K=1,Nr
       saFac(K)  = 1. _d 0
       recip_drC(K)   = 1. _d 0/drC(K)
       recip_drF(K)   = 1. _d 0/drF(K)
      ENDDO
C
      RETURN
      END
