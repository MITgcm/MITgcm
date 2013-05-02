C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_check_conserv.F,v 1.5 2013/05/02 20:01:21 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

C     !ROUTINE: THSICE_CHECK_CONSERV
C     !INTERFACE:
      SUBROUTINE THSICE_CHECK_CONSERV(
     I             dBugFlag, i, j, bi, bj, iceStart,
     I             iceFrac, compact, hIce, hSnow, qicen,
     I             qleft, ffresh, fsalt,
     I             myTime, myIter, myThid )
C     *==========================================================*
C     | S/R  THSICE_CHECK_CONSERV
C     | o Check Conservation of Energy, water and salt
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c #include "PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      LOGICAL dBugFlag
      INTEGER i,j, bi,bj
      INTEGER iceStart
      _RL iceFrac
      _RL compact, hIce, hSnow, qicen(nlyr)
      _RL qleft, fsalt, ffresh
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
      _RL dEnerg, dWater, dSalt
      _RL flxFrac
      _RL flxAtm, frwAtm
      LOGICAL dBugLoc

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,1P4E14.6)

          dBugLoc = .FALSE.
#ifdef ALLOW_DBUG_THSICE
          dBugLoc = dBug(i,j,bi,bj)
#endif
          flxFrac = iceFrac
          flxAtm = icFlxAtm(i,j,bi,bj)
          frwAtm = icFrwAtm(i,j,bi,bj)
          IF (iceStart.EQ.1) flxFrac = 1.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
          dEnerg= -rhos*snowHeight(i,j,bi,bj)*qsnow
     &            -rhoi*iceHeight(i,j,bi,bj)
     &             *(Qice1(i,j,bi,bj)+Qice2(i,j,bi,bj))*0.5
          dWater = rhos*snowheight(i,j,bi,bj)+rhoi*iceHeight(i,j,bi,bj)
          dSalt =  rhoi*iceHeight(i,j,bi,bj)*saltice
         IF (dBugLoc) WRITE(6,1010) 'ThSI_CHK: Ener0,Water0,Salt0 =',
     &    dEnerg, dWater, dSalt
C--
          dEnerg = dEnerg*iceFrac
     &     + compact*( rhos*hSnow*qsnow
     &               + rhoi*hIce*(qicen(1)+qicen(2))*0.5
     &               )
          dWater = dWater*iceFrac
     &     - compact*( rhos*hSnow + rhoi*hIce )
          dSalt =  dSalt*iceFrac
     &     - compact* rhoi*hIce*saltice

         IF (dBugLoc) WRITE(6,1010) 'ThSI_CHK: dEner,dH20,dSal /dt=',
     &    dEnerg/thSIce_deltaT,dWater/thSIce_deltaT,dSalt/thSIce_deltaT
         IF (dBugLoc) WRITE(6,1010) 'ThSI_CHK: fxH,fxW,fxS=',
     &    flxAtm-qleft, -ffresh-frwAtm,-fsalt
          dEnerg = dEnerg + thSIce_deltaT*flxFrac*(flxAtm-qleft)
          dWater = dWater - thSIce_deltaT*flxFrac*(ffresh+frwAtm)
          dSalt  = dSalt  - thSIce_deltaT*flxFrac*fsalt

#ifdef ALLOW_TIMEAVE
          ice_flx2oc_Ave(i,j,bi,bj) = ice_flx2oc_Ave(i,j,bi,bj)
     &          + dEnerg
          ice_frw2oc_Ave(i,j,bi,bj) = ice_frw2oc_Ave(i,j,bi,bj)
     &          + dWater
          ice_salFx_Ave(i,j,bi,bj) = ice_salFx_Ave(i,j,bi,bj)
     &          + dSalt
#endif /*ALLOW_TIMEAVE*/
C--
         IF (dBugLoc) WRITE(6,1010) 'ThSI_CHK: resid.H,W,S=',
     &    dEnerg/thSIce_deltaT,dWater/thSIce_deltaT,dSalt/thSIce_deltaT
         IF (dBugLoc) WRITE(6,1010) 'ThSI_CHK: hIc,hSn,snow*dt=',
     &    hIce, hSnow
c    &    hIce, hSnow, snowPrc(i,j,bi,bj)*thSIce_deltaT/rhos

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif  /*ALLOW_THSICE*/

      RETURN
      END
