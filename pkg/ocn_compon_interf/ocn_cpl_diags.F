C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_cpl_diags.F,v 1.3 2007/05/10 21:15:52 jscott Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE OCN_CPL_DIAGS( bi, bj, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_CPL_DIAGS                                 |
C     | o Calculate CPL diagnostics                              |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"
#include "CPL_DIAGS.h"

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
        SLPtave(i,j,bi,bj)  = SLPtave(i,j,bi,bj)
     &               + DDTT * atmSLPr(i,j,bi,bj)
        HFtave(i,j,bi,bj)   = HFtave(i,j,bi,bj)
     &               + DDTT * HeatFlux(i,j,bi,bj)
        QSWtave(i,j,bi,bj)  = QSWtave(i,j,bi,bj)
     &               + DDTT * qShortWave(i,j,bi,bj)
c       QLTtave(i,j,bi,bj)  = QLTtave(i,j,bi,bj)
c    &               + DDTT * qLatent(i,j,bi,bj)
c       QSNtave(i,j,bi,bj)  = QSNtave(i,j,bi,bj)
c    &               + DDTT * qSensible(i,j,bi,bj)
c       QLWtave(i,j,bi,bj)  = QLWtave(i,j,bi,bj)
c    &               + DDTT * qLongWave(i,j,bi,bj)
c       UGtave(i,j,bi,bj)   = UGtave(i,j,bi,bj)
c    &               + DDTT * uVelGround(i,j,bi,bj)
c       VGtave(i,j,bi,bj)   = VGtave(i,j,bi,bj)
c    &               + DDTT * vVelGround(i,j,bi,bj)
        TXtave(i,j,bi,bj)   = TXtave(i,j,bi,bj)
     &               + DDTT * tauX(i,j,bi,bj)
        TYtave(i,j,bi,bj)   = TYtave(i,j,bi,bj)
     &               + DDTT * tauY(i,j,bi,bj)
        FWtave(i,j,bi,bj)   = FWtave(i,j,bi,bj)
     &               + DDTT * FWFlux(i,j,bi,bj)
        SFxtave(i,j,bi,bj)  = SFxtave(i,j,bi,bj)
     &               + DDTT * iceSaltFlx(i,j,bi,bj)
        SICtave(i,j,bi,bj)  = SICtave(i,j,bi,bj)
     &               + DDTT * seaIceMass(i,j,bi,bj)
        MXLtave(i,j,bi,bj)  = MXLtave(i,j,bi,bj)
     &               + DDTT * ocMxlD2cpl(i,j,bi,bj)
        SSTtave(i,j,bi,bj)  = SSTtave(i,j,bi,bj)
     &               + DDTT * SSTocn2cpl(i,j,bi,bj)
        SSStave(i,j,bi,bj)  = SSStave(i,j,bi,bj)
     &               + DDTT * SSSocn2cpl(i,j,bi,bj)
        vSqtave(i,j,bi,bj)  = vSqtave(i,j,bi,bj)
     &               + DDTT * vSqocn2cpl(i,j,bi,bj)
        IF ( ocnCpl_exchange_DIC ) THEN
          aCO2tave(i,j,bi,bj)  = aCO2tave(i,j,bi,bj)
     &               + DDTT * airCO2(i,j,bi,bj)
          sWSpdtave(i,j,bi,bj) = sWSpdtave(i,j,bi,bj)
     &               + DDTT * surfWSpeed(i,j,bi,bj)
          iceftave(i,j,bi,bj)  = iceftave(i,j,bi,bj)
     &               + DDTT * fracIce(i,j,bi,bj)
          fCO2tave(i,j,bi,bj)  = fCO2tave(i,j,bi,bj)
     &               + DDTT * fluxCO2cpl(i,j,bi,bj)
        ENDIF
       ENDDO
      ENDDO

C-    Keep record of how much time has been integrated over
      DO K=1,1
       CPL_TimeAve(k,bi,bj)=CPL_TimeAve(k,bi,bj)+DDTT
      ENDDO                                                                                    
#endif /* ALLOW_TIMEAVE */

#endif /* COMPONENT_MODULE */

      RETURN
      END
