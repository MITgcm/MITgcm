C     $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_calc_diags.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C     $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CPL_CALC_DIAGS( bi, bj, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_CALC_DIAGS                                |
C     | o Calculate CPL diagnostics                              |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OCNCPL.h"
#include "CPL_DIAGS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     bi,bj  - Tile index
C     myTime - Current time of simulation ( s )
C     myThid - Number of this instance of the routine
      INTEGER bi, bj
      _RL     myTime
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE

C     == Local variables ==
      INTEGER I,J,K
      _RL DDTT

#ifdef ALLOW_TIMEAVE

      DDTT = deltaTclock

      DO J=1,sNy
       DO I=1,sNx
        HFtave(i,j,bi,bj)   = HFtave(i,j,bi,bj)
     &                      + HeatFlux(i,j,bi,bj)*DDTT
        TXtave(i,j,bi,bj)   = TXtave(i,j,bi,bj)
     &                      + tauX(i,j,bi,bj)*DDTT
        TYtave(i,j,bi,bj)   = TYtave(i,j,bi,bj)
     &                      + tauY(i,j,bi,bj)*DDTT
        FWtave(i,j,bi,bj)   = FWtave(i,j,bi,bj)
     &                      + FWFlux(i,j,bi,bj)*DDTT
        UGtave(i,j,bi,bj)   = UGtave(i,j,bi,bj)
     &                      + uVelGround(i,j,bi,bj)*DDTT
        VGtave(i,j,bi,bj)   = VGtave(i,j,bi,bj)
     &                      + vVelGround(i,j,bi,bj)*DDTT
        QLTtave(i,j,bi,bj)  = QLTtave(i,j,bi,bj)
     &                      + qLatent(i,j,bi,bj)*DDTT
        QSNtave(i,j,bi,bj)  = QSNtave(i,j,bi,bj)
     &                      + qSensible(i,j,bi,bj)*DDTT
        QLWtave(i,j,bi,bj)  = QLWtave(i,j,bi,bj)
     &                      + qLongwave(i,j,bi,bj)*DDTT
        QSWtave(i,j,bi,bj)  = QSWtave(i,j,bi,bj)
     &                      + qShortwave(i,j,bi,bj)*DDTT
        SSTtave(i,j,bi,bj)  = SSTtave(i,j,bi,bj)
     &                      + SSTocn2cpl(i,j,bi,bj)*DDTT
       ENDDO
      ENDDO

C-    Keep record of how much time has been integrated over
c     DO K=1,Nr
      DO K=1,1
       CPL_TimeAve(k,bi,bj)=CPL_TimeAve(k,bi,bj)+DDTT
      ENDDO                                                                                    


#endif /* ALLOW_TIMEAVE */

#endif /* COMPONENT_MODULE */

      RETURN
      END



