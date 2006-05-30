C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_step_temp.F,v 1.2 2006/05/30 22:48:59 mlosch Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_STEP_TEMP
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_TEMP(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_STEP_TEMP
C     | o Step Forward Surface and SeaIce Temperature.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"
      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C- input:
C     bi,bj   :: tile indices
C   iMin,iMax :: computation domain: 1rst index range
C   jMin,jMax :: computation domain: 2nd  index range
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
C-- Modify fluxes hold in commom blocks
C- input:
C     icFlxSW :: (Inp) short-wave heat flux (+=down): downward comp. only
C- output
C     icFlxSW :: (Out) net SW flux into ocean (+=down)
C     icFlxAtm:: net flux of energy from the atmosphere [W/m2] (+=down)
C     icFrwAtm:: evaporation to the atmosphere (kg/m2/s) (>0 if evaporate)
C--
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     tFrzOce   :: sea-water freezing temperature [oC] (function of S)
C     dTsrf     :: surf. temp adjusment: Ts^n+1 - Ts^n
      INTEGER i,j
      _RL tFrzOce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL dTsrf  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpflx(1:2), tmpdTs(1:2)
#ifdef SHORTWAVE_HEATING
      _RL opFrac, icFrac
#endif
      LOGICAL dBugFlag

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,1P4E14.6)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      dBugFlag = debugLevel.GE.debLevB
C-    Initialise flxAtm,evpAtm
       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
          icFlxAtm(i,j,bi,bj) = 0.
          icFrwAtm(i,j,bi,bj) = 0.
        ENDDO
       ENDDO

c     IF ( fluidIsWater ) THEN
       CALL THSICE_ALBEDO(
     I          bi, bj, siLo, siHi, sjLo, sjHi,
     I          iMin,iMax, jMin,jMax,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     I          snowAge(siLo,sjLo,bi,bj),
     O          siceAlb(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ;
C     Solve for surface and ice temperature (implicitly) ; compute surf. fluxes
C-------
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) THEN
           WRITE(6,'(A,2I4,2I2)') 'ThSI_STEP_T: i,j=',i,j,bi,bj
           WRITE(6,1010) 'ThSI_STEP_T: iceMask, hIc, hSn, Tsf  =',
     &                   iceMask(i,j,bi,bj), iceHeight(i,j,bi,bj),
     &                   snowHeight(i,j,bi,bj), Tsrf(i,j,bi,bj)
          ENDIF
#endif
C-      surface net SW flux:
          icFlxSW(i,j,bi,bj) = icFlxSW(i,j,bi,bj)
     &                       *(1. _d 0 - siceAlb(i,j,bi,bj))
          tFrzOce(i,j) = -mu_Tf*sOceMxL(i,j,bi,bj)
         ELSE
          tFrzOce(i,j) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO

       CALL THSICE_SOLVE4TEMP(
     I          bi, bj, siLo, siHi, sjLo, sjHi,
     I          iMin,iMax, jMin,jMax, dBugFlag, 
     I          useBulkForce, useEXF,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), tFrzOce, tmpflx,
     U          icFlxSW(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     O          Tice1(siLo,sjLo,bi,bj), Tice2(siLo,sjLo,bi,bj), tmpdTs,
     O          sHeating(siLo,sjLo,bi,bj), flxCndBt(siLo,sjLo,bi,bj),
     O          icFlxAtm(siLo,sjLo,bi,bj), icFrwAtm(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

#ifdef SHORTWAVE_HEATING
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
          icFrac  = iceMask(i,j,bi,bj)
          opFrac = 1. _d 0 - icFrac
C--    Update Fluxes :
          Qsw(i,j,bi,bj) = opFrac*Qsw(i,j,bi,bj)
     &                   - icFrac*icFlxSW(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO
#endif /* SHORTWAVE_HEATING */
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
