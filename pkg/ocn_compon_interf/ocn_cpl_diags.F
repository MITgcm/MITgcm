C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_cpl_diags.F,v 1.4 2010/01/03 19:26:54 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: OCN_CPL_DIAGS
C     !INTERFACE:
      SUBROUTINE OCN_CPL_DIAGS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R OCN_CPL_DIAGS
C     | o Calculate CPL time-average diagnostics
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"
#include "CPL_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time in simulation (s)
C     myIter :: Current Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
#ifdef ALLOW_TIMEAVE

C     !LOCAL VARIABLES:
C     bi, bj :: Tile indices
      INTEGER bi, bj
      INTEGER i,j
      _RL DDTT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( cpl_taveFreq.GT.0. _d 0 ) THEN
C--   note: will activate this "if" (and remove the one in front of
C           OCN_CPL_DIAGS call) to do other diags than time-ave.

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DDTT = deltaTclock
         DO j=1,sNy
          DO i=1,sNx
            SLPtave(i,j,bi,bj)  = SLPtave(i,j,bi,bj)
     &                   + DDTT * atmSLPr(i,j,bi,bj)
            HFtave(i,j,bi,bj)   = HFtave(i,j,bi,bj)
     &                   + DDTT * HeatFlux(i,j,bi,bj)
            QSWtave(i,j,bi,bj)  = QSWtave(i,j,bi,bj)
     &                   + DDTT * qShortWave(i,j,bi,bj)
c           QLTtave(i,j,bi,bj)  = QLTtave(i,j,bi,bj)
c    &                   + DDTT * qLatent(i,j,bi,bj)
c           QSNtave(i,j,bi,bj)  = QSNtave(i,j,bi,bj)
c    &                   + DDTT * qSensible(i,j,bi,bj)
c           QLWtave(i,j,bi,bj)  = QLWtave(i,j,bi,bj)
c    &                   + DDTT * qLongWave(i,j,bi,bj)
c           UGtave(i,j,bi,bj)   = UGtave(i,j,bi,bj)
c    &                   + DDTT * uVelGround(i,j,bi,bj)
c           VGtave(i,j,bi,bj)   = VGtave(i,j,bi,bj)
c    &                   + DDTT * vVelGround(i,j,bi,bj)
            TXtave(i,j,bi,bj)   = TXtave(i,j,bi,bj)
     &                   + DDTT * tauX(i,j,bi,bj)
            TYtave(i,j,bi,bj)   = TYtave(i,j,bi,bj)
     &                   + DDTT * tauY(i,j,bi,bj)
            FWtave(i,j,bi,bj)   = FWtave(i,j,bi,bj)
     &                   + DDTT * FWFlux(i,j,bi,bj)
            SFxtave(i,j,bi,bj)  = SFxtave(i,j,bi,bj)
     &                   + DDTT * iceSaltFlx(i,j,bi,bj)
            SICtave(i,j,bi,bj)  = SICtave(i,j,bi,bj)
     &                   + DDTT * seaIceMass(i,j,bi,bj)
            MXLtave(i,j,bi,bj)  = MXLtave(i,j,bi,bj)
     &                   + DDTT * ocMxlD2cpl(i,j,bi,bj)
            SSTtave(i,j,bi,bj)  = SSTtave(i,j,bi,bj)
     &                   + DDTT * SSTocn2cpl(i,j,bi,bj)
            SSStave(i,j,bi,bj)  = SSStave(i,j,bi,bj)
     &                   + DDTT * SSSocn2cpl(i,j,bi,bj)
            vSqtave(i,j,bi,bj)  = vSqtave(i,j,bi,bj)
     &                   + DDTT * vSqocn2cpl(i,j,bi,bj)
           IF ( ocnCpl_exchange_DIC ) THEN
            aCO2tave(i,j,bi,bj)  = aCO2tave(i,j,bi,bj)
     &                    + DDTT * airCO2(i,j,bi,bj)
            sWSpdtave(i,j,bi,bj) = sWSpdtave(i,j,bi,bj)
     &                    + DDTT * surfWSpeed(i,j,bi,bj)
            iceftave(i,j,bi,bj)  = iceftave(i,j,bi,bj)
     &                    + DDTT * fracIce(i,j,bi,bj)
            fCO2tave(i,j,bi,bj)  = fCO2tave(i,j,bi,bj)
     &                    + DDTT * fluxCO2cpl(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

C-    Keep record of how much time has been integrated over
         CPL_timeAve(bi,bj) = CPL_timeAve(bi,bj)+DDTT

C--   end bi,bj loops
        ENDDO
       ENDDO

C--   end if cpl_taveFreq > 0
c     ENDIF
#endif /* ALLOW_TIMEAVE */
#endif /* COMPONENT_MODULE */

      RETURN
      END
