C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.4 1998/08/22 17:51:09 cnh Exp $

#include "CPP_EEOPTIONS.h"

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
       drF(K)     = delZ(K)
       rF(K+1) = rF(K)-delZ(K)
      ENDDO
      drC(1)      = delZ(1) * 0.5 _d 0
      rC(1)       = -delZ(1) * 0.5 _d 0
      DO K=2,Nr
       drC(K)     = 0.5 _d 0 *(delZ(K-1)+delZ(K))
       rC(K)      = rC(K-1) - drC(K)
      ENDDO
      DO K=1,Nr
       saFac(K)  = 1. D0
       recip_drC(K)   = 1. _d 0/drC(K)
       recip_drF(K)   = 1. _d 0/drF(K)
      ENDDO
C
      RETURN
      END
