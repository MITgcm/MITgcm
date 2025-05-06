#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif

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
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: My Thread Id number
      INTEGER myThid

C     !FUNCTIONS:
c     INTEGER  ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount, k
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
      errCount = 0

C--   Make metric term & Coriolis settings consistent with underlying grid.
      IF ( usingCartesianGrid ) THEN
        selectMetricTerms = 0
        useNHMTerms   = .FALSE.
      ENDIF
      IF ( usingCylindricalGrid ) THEN
        useNHMTerms   = .FALSE.
        WRITE(msgBuf,'(A)') 'S/R INI_PARMS ; Cylinder OK'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( usingCurvilinearGrid ) THEN
C-    Horizontal metric terms not implemented for Curvilinear-Grid
        selectMetricTerms = 0
      ENDIF
      IF ( usingCylindricalGrid ) THEN
C-    No current alternative metric terms formulation for Cylindrical-Grid
        selectMetricTerms = MIN( selectMetricTerms, 1 )
      ENDIF
      IF ( selectCoriMap.EQ.-1 ) THEN
        IF ( usingCartesianGrid.OR.usingCylindricalGrid ) THEN
C       default is to use Beta-Plane Coriolis
          selectCoriMap = 1
        ELSE
C       default for other grids is to use Spherical Coriolis
          selectCoriMap = 2
        ENDIF
      ENDIF
      IF ( .NOT.(nonHydrostatic.OR.quasiHydrostatic) )
     &                          select3dCoriScheme = 0
      IF ( (selectCoriMap.EQ.0 .OR.selectCoriMap.EQ.1)
     &     .AND. fPrime.EQ.0. ) select3dCoriScheme = 0

C--   On/Off flags for each terms of the momentum equation
      nonHydrostatic   = momStepping .AND. nonHydrostatic
      quasiHydrostatic = momStepping .AND. quasiHydrostatic
      momAdvection     = momStepping .AND. momAdvection
      momViscosity     = momStepping .AND. momViscosity
      momForcing       = momStepping .AND. momForcing
      momTidalForcing  = momForcing  .AND. momTidalForcing
      useCoriolis      = momStepping .AND. useCoriolis
      IF ( .NOT.useCoriolis ) select3dCoriScheme = 0
      useCDscheme      = momStepping .AND. useCDscheme
      momPressureForcing= momStepping .AND. momPressureForcing
      implicitIntGravWave=momPressureForcing .AND. implicitIntGravWave
      momImplVertAdv   = momAdvection .AND. momImplVertAdv
      useNHMTerms      = momAdvection .AND. useNHMTerms
      IF ( .NOT.momAdvection ) selectMetricTerms = 0
      implicitViscosity= momViscosity .AND. implicitViscosity
      useSmag3D        = momViscosity .AND. useSmag3D
      use3Dsolver      = nonHydrostatic.OR. implicitIntGravWave
      calc_wVelocity   = momStepping .OR. exactConserv

#ifndef ALLOW_3D_VISCAH
      IF ( viscAhDfile.NE.' ' .OR. viscAhZfile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') 'SET_PARMS: ',
     &      'viscAhDfile and viscAhZfile cannot be used with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'SET_PARMS: ',
     &      '"#undef ALLOW_3D_VISCAH" in MOM_COMMON_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif
#ifndef ALLOW_3D_VISCA4
      IF ( viscA4Dfile.NE.' ' .OR. viscA4Zfile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') 'SET_PARMS: ',
     &      'viscA4Dfile and viscA4Zfile cannot be used with'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'SET_PARMS: ',
     &      '"#undef ALLOW_3D_VISCA4" in MOM_COMMON_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef ALLOW_MOM_COMMON
C-    On/Off flags for viscosity coefficients
      useVariableVisc   =
     &      viscAhGrid  .NE.zeroRL .OR. viscA4Grid  .NE.zeroRL
     &  .OR. viscC2smag .NE.zeroRL .OR. viscC4smag  .NE.zeroRL
     &  .OR. viscC2leith.NE.zeroRL .OR. viscC2leithD.NE.zeroRL
     &  .OR. viscC2LeithQG.NE.zeroRL
     &  .OR. viscC4leith.NE.zeroRL .OR. viscC4leithD.NE.zeroRL
     &  .OR. viscAhDfile.NE.' '    .OR.  viscAhZfile.NE.' '
     &  .OR. viscA4Dfile.NE.' '    .OR.  viscA4Zfile.NE.' '

      useHarmonicVisc   = viscAh .NE.zeroRL
     &  .OR. viscAhD    .NE.zeroRL .OR. viscAhZ     .NE.zeroRL
     &  .OR. viscAhGrid .NE.zeroRL .OR. viscC2smag  .NE.zeroRL
     &  .OR. viscC2leith.NE.zeroRL .OR. viscC2leithD.NE.zeroRL
     &  .OR. viscC2LeithQG.NE.zeroRL
     &  .OR. viscAhDfile.NE. ' '   .OR. viscAhZfile .NE. ' '

      useBiharmonicVisc = viscA4.NE.zeroRL
     &  .OR. viscA4D    .NE.zeroRL .OR. viscA4Z     .NE.zeroRL
     &  .OR. viscA4Grid .NE.zeroRL .OR. viscC4smag  .NE.zeroRL
     &  .OR. viscC4leith.NE.zeroRL .OR. viscC4leithD.NE.zeroRL
     &  .OR. viscA4Dfile.NE. ' '   .OR. viscA4Zfile .NE. ' '

      useVariableVisc   = momViscosity .AND. useVariableVisc
      useHarmonicVisc   = momViscosity .AND. useHarmonicVisc
      useBiharmonicVisc = momViscosity .AND. useBiharmonicVisc
#endif /* ALLOW_MOM_COMMON */
      IF ( ( bottomDragQuadratic.EQ.0. .AND. zRoughBot.EQ.0. )
     &     .OR. .NOT.momViscosity ) selectBotDragQuadr = -1

C--   Free-surface & pressure method
      uniformFreeSurfLev = usingZCoords
C- Note: comment line below to revert to full-cell hydrostatic-pressure
C        calculation in surface grid-cell below ice-shelf
      uniformFreeSurfLev = usingZCoords .AND. .NOT.useShelfIce
     &                                  .AND. topoFile.EQ.' '
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
#ifdef ALLOW_AUTODIFF
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
      useAbsVorticity = vectorInvariantMomentum .AND. useAbsVorticity
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
      IF ( selectMetricTerms.GE.1 ) THEN
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
      doThetaClimRelax = ( tempForcing .OR.( useOffLine.AND.useKPP ) )
     &             .AND. ( tauThetaClimRelax.GT.0. _d 0 )
      doSaltClimRelax  = ( saltForcing .OR.( useOffLine.AND.useKPP ) )
     &             .AND. ( tauSaltClimRelax .GT.0. _d 0 )

C--   Dynamically Active Tracers : set flags
      tempIsActiveTr = momPressureForcing .AND. tempAdvection
      saltIsActiveTr = momPressureForcing .AND. saltAdvection
      IF ( eosType.EQ.'IDEALG' .AND. atm_Rq.EQ.0. ) THEN
        saltIsActiveTr = .FALSE.
      ELSEIF ( eosType.EQ.'LINEAR' ) THEN
        IF ( tAlpha.EQ.0. ) tempIsActiveTr = .FALSE.
        IF ( sBeta .EQ.0. ) saltIsActiveTr = .FALSE.
      ENDIF

C--   Set default for latitude-band where relaxation to climatology applies
C     note: done later (once domain size is known) if using CartesianGrid
      IF ( latBandClimRelax .EQ. UNSET_RL) THEN
        IF ( usingSphericalPolarGrid ) latBandClimRelax= 180. _d 0
        IF ( usingCurvilinearGrid )    latBandClimRelax= 180. _d 0
      ENDIF

      IF ( usingZCoords ) THEN
C--   Select which pressure to use in EOS:
C     set default according to EOS type (as it was until chkpt65t)
        IF ( selectP_inEOS_Zc.EQ.UNSET_I ) THEN
          IF ( eosType .EQ. 'JMD95P' .OR.  eosType .EQ. 'UNESCO'
     &    .OR. eosType .EQ. 'MDJWF'  .OR.  eosType .EQ. 'TEOS10'
     &       ) THEN
           selectP_inEOS_Zc = 2
          ELSE
           selectP_inEOS_Zc = 0
          ENDIF
        ELSEIF ( selectP_inEOS_Zc.LT.0
     &      .OR. selectP_inEOS_Zc.GT.3 ) THEN
          WRITE(msgBuf,'(A,I9,A)') 'SET_PARMS: selectP_inEOS_Zc=',
     &                  selectP_inEOS_Zc, ' : invalid selection'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ELSEIF ( .NOT.nonHydrostatic ) THEN
          selectP_inEOS_Zc = MIN( selectP_inEOS_Zc, 2 )
        ENDIF
        IF ( ( eosType .EQ. 'LINEAR' .OR.  eosType .EQ. 'POLY3 ' )
     &      .AND. selectP_inEOS_Zc.NE.0  ) THEN
          WRITE(msgBuf,'(A,I9,2A)') 'SET_PARMS: selectP_inEOS_Zc=',
     &     selectP_inEOS_Zc, ' : invalid with eosType=', eosType
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ELSE
        selectP_inEOS_Zc = -1
      ENDIF
C--   When using the dynamical pressure in EOS (with Z-coord.),
C     needs to activate specific part of the code (restart & exchange)
      storePhiHyd4Phys = selectP_inEOS_Zc.GE.2
C-    pkg/atm_phys uses main-model geopotential:
      storePhiHyd4Phys = storePhiHyd4Phys .OR. useAtm_Phys

C--   Adjust parameters related to length of the simulation

C-    Need to adjust endTime for sub-timestep mismatch , since in
C     several places, test for last iteration with time==endTime :
      tmpVar = startTime + deltaTClock*DFLOAT(nTimeSteps)
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

      IF ( OLx.LE.0 .OR. OLy.LE.0 ) THEN
C--   Overlap-size will be checked later in CONFIG_CHECK. This minimal
C     check here is just to set-up the model grid (INI_GRID) safely.
        WRITE(msgBuf,'(2A)') 'SET_PARMS: ',
     &      'model unusable with no overlap (OLx,OLy = 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'SET_PARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R SET_PARMS'
      ENDIF

C--   After this point, main model parameters are not supposed to be modified.
       WRITE(msgBuf,'(A,A)') 'SET_PARMS: done'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be set
      _BARRIER

      RETURN
      END
