C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.1 1998/07/02 14:17:11 adcroft Exp $

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
      zFace(1) = 0. _d 0
      DO K=1,Nz
       dzF(K)     = delZ(K)
       zFace(K+1) = zFace(K)+delZ(K)
      ENDDO
      dzC(1)      = delZ(1) * 0.5 _d 0
      zC(1)       = delZ(1) * 0.5 _d 0
      DO K=2,Nz
       dzC(K)     = 0.5 _d 0 *(delZ(K-1)+delZ(K))
       zC(K)      = zC(K) + dzC(K)
      ENDDO
      DO K=1,Nz
       saFac(K)  = 1. D0
       rDzC(K)   = 1. _d 0/dzC(K)
       rDzF(K)   = 1. _d 0/dzF(K)
      ENDDO
C
      RETURN
      END
