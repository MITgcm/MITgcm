C $Header: /u/gcmpack/MITgcm/model/src/set_parms.F,v 1.16 2012/07/13 20:44:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_PARMS
C     !INTERFACE:
      SUBROUTINE SET_PARMS( myThid )

C     !DESCRIPTION:
C     Set model "parameters" that might depend on the use of some pkgs;
C     called from INITIALISE_FIXED, after INI_PARMS & PACKAGES_READPARAMS
C     NOTES: After leaving this S/R, parameters will not change anymore.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: My Thread Id number
      INTEGER myThid

C     !FUNCTIONS:
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k
      _RL tmpVar
CEOP

C--   Set (or reset) On/Off flags :

C-    For off-line calculation, switch off Momentum and Active-tracers (=T,S):
#ifdef ALLOW_OFFLINE
      IF ( useOffLine ) THEN
        CALL OFFLINE_RESET_PARMS( myThid )
      ENDIF
#endif /* ALLOW_OFFLINE */

      _BEGIN_MASTER(myThid)

C--   On/Off flags for each terms of the momentum equation
      nonHydrostatic   = momStepping .AND. nonHydrostatic
      quasiHydrostatic = momStepping .AND. quasiHydrostatic
      momAdvection = momStepping .AND. momAdvection
      momViscosity = momStepping .AND. momViscosity
      momForcing   = momStepping .AND. momForcing
      useCoriolis  = momStepping .AND. useCoriolis
      use3dCoriolis= useCoriolis .AND. use3dCoriolis
      useCDscheme  = momStepping .AND. useCDscheme
      momPressureForcing= momStepping .AND. momPressureForcing
      implicitIntGravWave=momPressureForcing .AND. implicitIntGravWave
      momImplVertAdv   = momAdvection .AND. momImplVertAdv
      implicitViscosity= momViscosity .AND. implicitViscosity
      use3Dsolver      = nonHydrostatic.OR. implicitIntGravWave
      calc_wVelocity   = momStepping .OR. exactConserv

C--   Free-surface & pressure method
      uniformFreeSurfLev = usingZCoords
C- Note: comment line below to revert to full-cell hydrostatic-pressure
C        calculation in surface grid-cell below ice-shelf
      uniformFreeSurfLev = usingZCoords .AND. .NOT.useShelfIce
      IF ( selectNHfreeSurf.NE.0 .AND.
     &      ( .NOT.nonHydrostatic .OR. usingPCoords
     &        .OR. .NOT.exactConserv
     &      ) ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** SET_PARMS: ',
     &                       'reset selectNHfreeSurf to zero'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        selectNHfreeSurf = 0
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
      doResetHFactors = .TRUE.
#endif
#ifndef NONLIN_FRSURF
      doResetHFactors = .FALSE.
#endif

C--   Set default Vorticity-Term Scheme:
      IF ( vectorInvariantMomentum ) THEN
        IF ( selectVortScheme.EQ.UNSET_I ) THEN
          selectVortScheme = 1
          IF ( upwindVorticity )    selectVortScheme = 0
          IF ( highOrderVorticity ) selectVortScheme = 0
        ENDIF
      ELSEIF ( selectVortScheme.NE.UNSET_I ) THEN
        WRITE(msgBuf,'(A,A)') '** WARNING ** SET_PARMS: ',
     &   'Vector-Invariant Momentum unused => ignore selectVortScheme'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
C--   Momentum viscosity on/off flag.
      IF ( momViscosity        ) THEN
       vfFacMom = 1. _d 0
      ELSE
       vfFacMom = 0. _d 0
      ENDIF
C--   Momentum advection on/off flag.
      IF ( momAdvection        ) THEN
       afFacMom = 1. _d 0
      ELSE
       afFacMom = 0. _d 0
      ENDIF
C--   Momentum forcing on/off flag.
      IF ( momForcing ) THEN
       foFacMom = 1. _d 0
      ELSE
       foFacMom = 0. _d 0
      ENDIF
C--   Coriolis term on/off flag.
      IF ( useCoriolis ) THEN
       cfFacMom = 1. _d 0
      ELSE
       cfFacMom = 0. _d 0
      ENDIF
C--   Pressure term on/off flag.
      IF ( momPressureForcing ) THEN
       pfFacMom = 1. _d 0
      ELSE
       pfFacMom = 0. _d 0
      ENDIF
C--   Metric terms on/off flag.
      IF ( metricTerms ) THEN
       mTFacMom = 1. _d 0
      ELSE
       mTFacMom = 0. _d 0
      ENDIF

C--   Advection and Forcing for Temp and salt  on/off flags
      tempVertDiff4 = .FALSE.
      saltVertDiff4 = .FALSE.
      DO k=1,Nr
        tempVertDiff4 = tempVertDiff4 .OR. ( diffKr4T(k).GT.0. _d 0 )
        saltVertDiff4 = saltVertDiff4 .OR. ( diffKr4S(k).GT.0. _d 0 )
      ENDDO
      tempAdvection = tempStepping .AND. tempAdvection
      tempVertDiff4 = tempStepping .AND. tempVertDiff4
      tempForcing   = tempStepping .AND. tempForcing
      saltAdvection = saltStepping .AND. saltAdvection
      saltVertDiff4 = saltStepping .AND. saltVertDiff4
      saltForcing   = saltStepping .AND. saltForcing
      tempImplVertAdv = tempAdvection .AND. tempImplVertAdv
      saltImplVertAdv = saltAdvection .AND. saltImplVertAdv
      doThetaClimRelax = tempForcing .AND.
     &                 ( tauThetaClimRelax.GT.0. _d 0 )
      doSaltClimRelax  = saltForcing .AND.
     &                 ( tauSaltClimRelax .GT.0. _d 0 )

C--   Dynamically Active Tracers : set flags
      tempIsActiveTr = momPressureForcing .AND. tempAdvection
      saltIsActiveTr = momPressureForcing .AND. saltAdvection
      IF ( eosType.EQ.'IDEALG' .AND. atm_Rq.EQ.0. ) THEN
        saltIsActiveTr = .FALSE.
      ELSEIF ( eosType.EQ.'LINEAR' ) THEN
        IF ( tAlpha.EQ.0. ) tempIsActiveTr = .FALSE.
        IF ( sBeta .EQ.0. ) saltIsActiveTr = .FALSE.
      ENDIF

C--   When using the dynamical pressure in EOS (with Z-coord.),
C     needs to activate specific part of the code (restart & exchange)
c     useDynP_inEos_Zc = .FALSE.
      useDynP_inEos_Zc = ( fluidIsWater .AND. usingZCoords
     &              .AND. ( eosType .EQ. 'JMD95P' .OR.
     &                      eosType .EQ. 'UNESCO' .OR.
     &                      eosType .EQ. 'MDJWF'  .OR.
     &                      eosType .EQ. 'TEOS10'      )  )

C--   Adjust parameters related to length of the simulation

C-    Need to adjust endTime for sub-timestep mismatch , since in
C     several places, test for last iteration with time==endTime :
      tmpVar = startTime + deltaTClock*FLOAT(nTimeSteps)
      IF ( endTime.NE.tmpVar ) THEN
       IF ( ABS(endTime-tmpVar).GT.deltaTClock*1. _d -6 ) THEN
        WRITE(msgBuf,'(A,A)') '** WARNING ** SET_PARMS: ',
     &   '(endTime-baseTime) not multiple of time-step'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A,1PE20.13)') '** WARNING ** SET_PARMS: ',
     &   'Previous endTime=', endTime
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A,1PE20.13)') '** WARNING ** SET_PARMS: ',
     &   'Adjusted endTime=', tmpVar
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       endTime = tmpVar
      ENDIF

#ifdef ALLOW_LONGSTEP
      IF ( usePTRACERS ) THEN
        CALL LONGSTEP_CHECK_ITERS(myThid)
      ENDIF
#endif /* ALLOW_LONGSTEP */

C--  After this point, main model parameters are not supposed to be modified.
       WRITE(msgBuf,'(A,A)') 'SET_PARMS: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be set
      _BARRIER

      RETURN
      END
