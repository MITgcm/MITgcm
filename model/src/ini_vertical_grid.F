C $Header: /u/gcmpack/MITgcm/model/src/ini_vertical_grid.F,v 1.12 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_VERTICAL_GRID
C     !INTERFACE:
      SUBROUTINE INI_VERTICAL_GRID( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_VERTICAL_GRID                              
C     | o Initialise vertical gridding arrays                     
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_DEPTHS
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     K
      INTEGER K
CEOP

C     Calculate depths of centers and interfaces
      rF(1) = Ro_SeaLevel
      DO K=1,Nr
       drF(K)     = delR(K)
       rF(K+1) = rF(K)-rkFac*delR(K)
      ENDDO
      drC(1)      = delR(1) * 0.5 _d 0
      rC(1)       = rf(1)-rkFac*delR(1) * 0.5 _d 0
      DO K=2,Nr
       drC(K)     = 0.5 _d 0 *(delR(K-1)+delR(K))
       rC(K)      = rC(K-1) - rkFac*drC(K)
      ENDDO
      DO K=1,Nr
       saFac(K)  = 1. _d 0
       recip_drC(K)   = 1. _d 0/drC(K)
       recip_drF(K)   = 1. _d 0/drF(K)
      ENDDO
C
      RETURN
      END
