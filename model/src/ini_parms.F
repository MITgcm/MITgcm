C $Header: /u/gcmpack/MITgcm/model/src/ini_parms.F,v 1.34 1998/10/28 03:11:37 cnh Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE INI_PARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_PARMS                                     |
C     | o Routine to set model "parameters"                      |
C     |==========================================================|
C     | Notes:                                                   |
C     | ======                                                   |
C     | The present version of this routine is a place-holder.   |
C     | A production version needs to handle parameters from an  |
C     | external file and possibly reading in some initial field |
C     | values.                                                  |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"

C     === Routine arguments ===
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid

C     === Local variables ===
C     dxSpacing, dySpacing - Default spacing in X and Y.
C                            Units are that of coordinate system
C                            i.e. cartesian => metres
C                                  s. polar => degrees
C     goptCount - Used to count the nuber of grid options
C                 (only one is allowed! )
C     msgBuf    - Informational/error meesage buffer
C     errIO     - IO error flag
C     iUnit - Work variable for IO unit number
C     record - Work variable for IO buffer
C     K, I, J - Loop counters
C     xxxDefault - Default value for variable xxx
      _RL  dxSpacing
      _RL  dySpacing
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      INTEGER goptCount
      INTEGER K, I, J, IL, iUnit
      INTEGER errIO
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
C     Default values for variables which have vertical coordinate system
C     dependency.
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault
C     zCoordInputData - These are used to select between different coordinate systems.
C     pCoordInputData   The vertical coordinate system in the rest of the model is 
C     rCoordInputData   written in terms of r. In the model "data" file input data can
C     coordsSet         be interms of z, p or r.
C                       e.g. delZ or delP or delR for the vertical grid spacing.
C                       The following rules apply:
C                       All parameters must use the same vertical coordinate system.
C                       e.g. delZ and viscAz is legal but
C                            delZ and viscAr is an error.
C                       Similarly specifyinh delZ and delP is an error.
C                       zCoord..., pCoord..., rCoord... are used to flag when z, p or r are
C                       used. coordsSet counts how many vertical coordinate systems have been
C                       used to specify variables. coordsSet > 1 is an error.
C
      LOGICAL zCoordInputData
      LOGICAL pCoordInputData
      LOGICAL rCoordInputData
      INTEGER coordsSet

C--   Continuous equation parameters
      NAMELIST /PARM01/
     & gravity, gBaro, rhonil, tAlpha, sBeta, f0, beta,
     &   viscAh,  viscAz,  viscA4,
     &  diffKhT, diffKzT, diffK4T, 
     &  diffKhS, diffKzS, diffK4S,
     &  GMmaxslope,GMlength,GMalpha,GMdepth,GMkbackground,GMmaxval,
     &  tRef, sRef, eosType,
     & momViscosity,  momAdvection, momForcing, useCoriolis,
     & momPressureForcing, metricTerms,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & implicitFreeSurface, rigidLid, freeSurfFac, hFacMin, hFacMinDz,
     & tempStepping, saltStepping, momStepping, implicitDiffusion,
     & viscAr, diffKrT, diffKrS, hFacMinDr,
     & viscAp, diffKpT, diffKpS, hFacMinDp,
     & rhoConst, buoyancyRelation

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dChkResFreq, cg2dTargetResidual, cg2dpcOffDFac,
     & cg3dMaxIters, cg3dChkResFreq, cg3dTargetResidual

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, deltaT, deltaTmom, deltaTtracer, abEps, 
     & tauCD, rCD,
     & startTime, endTime, chkPtFreq, dumpFreq, taveFreq, deltaTClock,
     & pChkPtFreq, cAdjFreq, tauThetaClimRelax, tauSaltClimRelax,
     & periodicExternalForcing, externForcingPeriod, externForcingCycle

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, delZ, dxSpacing, dySpacing, delX, delY,
     & usingSphericalPolarGrid, phiMin, thetaMin, rSphere,
     & l, m, n, delP, delR, rkFac

C--   Input files
      NAMELIST /PARM05/
     & bathyFile, hydrogThetaFile, hydrogSaltFile, 
     & zonalWindFile, meridWindFile, thetaClimFile,
     & saltClimFile


C
      _BEGIN_MASTER(myThid)

C--   Initialise "which vertical coordinate system used" flags.
      zCoordInputData = .FALSE.
      pCoordInputData = .FALSE.
      rCoordInputData = .FALSE.
      usingPCoords    = .FALSE.
      usingZCoords    = .FALSE.
      coordsSet       = 0

C--   Open the parameter file
      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
      OPEN(UNIT=modelDataUnit,FILE='data',STATUS='OLD',
     &     err=1,IOSTAT=errIO)
      IF ( errIO .GE. 0 ) GOTO 2
    1 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Unable to open model parameter'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'file "data"'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
    2 CONTINUE

 1000 CONTINUE
      READ(modelDataUnit,FMT='(A)',END=1001) RECORD
      IL = MAX(ILNBLNK(RECORD),1)
      IF ( RECORD(1:1) .NE. commentCharacter )
     &    WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
       WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
      GOTO 1000
 1001 CONTINUE
      CLOSE(modelDataUnit)

C--   Report contents of model parameter file
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Model parameter file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      iUnit = scrUnit2
      REWIND(iUnit)
 2000 CONTINUE
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      GOTO 2000
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)


C--   Read settings from model parameter file "data".
      iUnit = scrUnit1
      REWIND(iUnit)

C--   Set default "physical" parameters
      DO K =1,Nr
       tRef(K) = 30.D0 - FLOAT( K )
      ENDDO
      gravity  =   9.81D0
      gBaro    = gravity
      rhoNil   = 999.8D0
      rhoConst = 999.8D0
      f0       = 1.D-4 
      beta     = 1.D-11
      viscAh   = 1.D3
      diffKhT  = 1.D3
      diffKhS  = 1.D3
      viscArDefault = 1.D-3
      viscAz   = UNSET_RL    
      viscAr   = UNSET_RL
      viscAp   = UNSET_RL
      diffKrTDefault = 1.D-5
      diffKzT  = UNSET_RL
      diffKpT  = UNSET_RL
      diffKrT  = UNSET_RL
      diffKrSDefault = 1.D-5
      diffKzS  = UNSET_RL
      diffKpS  = UNSET_RL
      diffKrS  = UNSET_RL
      viscA4   = 0.
      diffK4T  = 0.
      diffK4S  = 0.
      GMmaxslope   =   1.D-2
      GMlength     = 200.D3
      GMalpha      = 0.D0
      GMdepth      = 1000.D0
      GMkbackground= 0.D0
      GMmaxval     = 2500.D0
      tAlpha       = 2.D-4
      sBeta        = 7.4D-4
      eosType      = 'LINEAR'
      buoyancyRelation    = 'OCEANIC'
      implicitFreeSurface = .TRUE.
      rigidLid            = .FALSE.
      freeSurfFac         = 1.D0
      hFacMin             = 0.D0
      hFacMinDrDefault    = 0.D0
      hFacMinDr           = UNSET_RL
      hFacMinDz           = UNSET_RL
      hFacMinDp           = UNSET_RL
      momViscosity        = .TRUE.
      momAdvection        = .TRUE.
      momForcing          = .TRUE.
      useCoriolis         = .TRUE.
      momPressureForcing  = .TRUE.
      momStepping         = .TRUE.
      tempStepping        = .TRUE.
      saltStepping        = .TRUE.
      metricTerms         = .TRUE.
      implicitDiffusion   = .FALSE.
      READ(UNIT=iUnit,NML=PARM01,IOSTAT=errIO,err=3)
      IF ( errIO .GE. 0 ) GOTO 4
    3 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
   4  CONTINUE
      IF ( implicitFreeSurface ) freeSurfFac = 1.D0
      IF ( rigidLid            ) freeSurfFac = 0.D0
C--   Momentum viscosity on/off flag.
      IF ( momViscosity        ) THEN
       vfFacMom = 1.D0
      ELSE
       vfFacMom = 0.D0
      ENDIF
C--   Momentum advection on/off flag.
      IF ( momAdvection        ) THEN
       afFacMom = 1.D0
      ELSE
       afFacMom = 0.D0
      ENDIF
C--   Momentum forcing on/off flag.
      IF ( momForcing ) THEN
       foFacMom = 1.D0
      ELSE
       foFacMom = 0.D0
      ENDIF
C--   Coriolis term on/off flag.
      IF ( useCoriolis ) THEN
       cfFacMom = 1.D0
      ELSE
       cfFacMom = 0.D0
      ENDIF
C--   Pressure term on/off flag.
      IF ( momPressureForcing ) THEN
       pfFacMom = 1.D0
      ELSE
       pfFacMom = 0.D0
      ENDIF
C--   Metric terms on/off flag.
      IF ( metricTerms ) THEN
       mTFacMom = 1.D0
      ELSE
       mTFacMom = 1.D0
      ENDIF
C--   z,p,r coord input switching.
      IF ( viscAz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( viscAp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( viscAr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAz
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscAp
      IF ( viscAr .EQ. UNSET_RL )          viscAr = viscArDefault

      IF ( diffKzT .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpT .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrT .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKzT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKpT
      IF ( diffKrT .EQ. UNSET_RL )          diffKrT = diffKrTDefault

      IF ( diffKzS .NE. UNSET_RL ) zCoordInputData  = .TRUE.
      IF ( diffKpS .NE. UNSET_RL ) pCoordInputData  = .TRUE.
      IF ( diffKrS .NE. UNSET_RL ) rCoordInputData  = .TRUE.
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKzS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKpS
      IF ( diffKrS .EQ. UNSET_RL )          diffKrS = diffKrSDefault

      IF ( hFacMinDz .NE. UNSET_RL ) zCoordInputData = .TRUE.
      IF ( hFacMinDp .NE. UNSET_RL ) pCoordInputData = .TRUE.
      IF ( hFacMinDr .NE. UNSET_RL ) rCoordInputData = .TRUE.
      IF ( hFacMinDz .EQ. UNSET_RL ) hFacMinDr       = hFacMinDz
      IF ( hFacMinDp .EQ. UNSET_RL ) hFacMinDr       = hFacMinDp
      IF ( hFacMinDr .EQ. UNSET_RL ) hFacMinDr       = hFacMinDrDefault

      IF ( implicitFreeSurface .AND.  rigidLid ) THEN
       WRITE(msgBuf,'(A,A)')
     & 'S/R INI_PARMS: Cannot select both implicitFreeSurface',
     & ' and rigidLid.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
      IF ( rhoConst .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoConst must be greater than 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_rhoConst = 1.D0 / rhoConst
      ENDIF
      IF ( gravity .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: gravity must be greater than 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_gravity = 1.D0 / gravity
      ENDIF

C--   Elliptic solver parameters
      cg2dMaxIters       = 150
      cg2dTargetResidual = 1.D-7
      cg2dChkResFreq     = 1
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1.D-7
      cg3dChkResFreq     = 1
      cg2dpcOffDFac      = 0.51D0
      READ(UNIT=iUnit,NML=PARM02,IOSTAT=errIO,err=5)
      IF ( errIO .GE. 0 ) GOTO 6
    5 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data".'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM02'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
   6  CONTINUE

C--   Time stepping parameters
      startTime         = 0.
      nTimeSteps        = 0
      endTime           = 0.
      nIter0            = 0
      deltaT            = 0.
      deltaTClock       = 0.
      deltaTtracer      = 0.
      deltaTMom         = 0.
      abEps             = 0.01
      pchkPtFreq        = 0.
      chkPtFreq         = 3600.*25
      dumpFreq          = 3600.*100
      taveFreq          = 0.
      writeStatePrec    = precFloat64
      nCheckLev         = 1
      checkPtSuff(1)    = 'ckptA'
      checkPtSuff(2)    = 'ckptB'
      cAdjFreq          = -1.D0
      rCD               = -1.D0
      tauCD             =  0.D0
      tauThetaClimRelax =  0.D0
      doThetaClimRelax  = .FALSE.
      tauSaltClimRelax  =  0.D0
      doSaltClimRelax   = .FALSE.
      periodicExternalForcing = .FALSE.
      externForcingPeriod     = 0.
      externForcingCycle      = 0.
      READ(UNIT=iUnit,NML=PARM03,IOSTAT=errIO,err=7)
      IF ( errIO .GE. 0 ) GOTO 8
    7 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM03'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
   8  CONTINUE
C     Process "timestepping" params
C     o Time step size
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTmom
      IF ( deltaT       .EQ. 0. ) deltaT       = deltaTtracer
      IF ( deltaTmom    .EQ. 0. ) deltaTmom    = deltaT
      IF ( deltaTtracer .EQ. 0. ) deltaTtracer = deltaT
      IF ( deltaTClock  .EQ. 0. ) deltaTClock  = deltaT
      IF ( periodicExternalForcing ) THEN
       IF ( externForcingCycle*externForcingPeriod .EQ. 0. ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle,externForcingPeriod =0'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( INT(externForcingCycle/externForcingPeriod) .NE.
     &          externForcingCycle/externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle <> N*externForcingPeriod'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( externForcingCycle.le.externForcingPeriod ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingCycle < externForcingPeriod'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
       IF ( externForcingPeriod.lt.deltaTclock ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R INI_PARMS: externForcingPeriod < deltaTclock'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
      ENDIF
C     o Convection frequency
      IF ( cAdjFreq .LT. 0. ) THEN
       cAdjFreq = deltaTClock
      ENDIF
C     o CD coupling
      IF ( tauCD .EQ. 0.D0 ) THEN
        tauCD = deltaTmom
      ENDIF
      IF ( rCD .LT. 0. ) THEN
       rCD = 1. - deltaTMom/tauCD
      ENDIF
C     o Temperature climatology relaxation time scale
      IF ( tauThetaClimRelax .EQ. 0.D0 ) THEN
       doThetaClimRelax     = .FALSE.
       lambdaThetaClimRelax = 0.D0
      ELSE
       doThetaClimRelax     = .TRUE.
       lambdaThetaClimRelax = 1./tauThetaClimRelax
      ENDIF
C     o Salinity climatology relaxation time scale
      IF ( tauSaltClimRelax .EQ. 0.D0 ) THEN
       doSaltClimRelax     = .FALSE.
       lambdaSaltClimRelax = 0.D0
      ELSE
       doSaltClimRelax     = .TRUE.
       lambdaSaltClimRelax = 1./tauSaltClimRelax
      ENDIF
C     o Time step count
      IF ( endTime .NE. 0 ) THEN
       IF ( deltaTClock .NE. 0 ) nTimeSteps = 
     &  INT((endTime-startTime)/deltaTClock)
      ENDIF
C     o Finish time
      IF ( endTime .EQ. 0. ) endTime = FLOAT(nTimeSteps)*deltaTClock

C     o If taveFreq is finite, then we must make sure the diagnostics
C       code is being compiled
#ifndef ALLOW_DIAGNOSTICS
      IF (taveFreq.NE.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: taveFreq <> 0  but you have'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'not compiled the model with the diagnostics routines.'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'Re-compile with:  #define ALLOW_DIAGNOSTICS  or  -DALLOW_DIAGNOSTICS'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
#endif
 
C--   Grid parameters
C     In cartesian coords distances are in metres
      rkFac = UNSET_I
      usingCartesianGrid = .TRUE.
      delRDefault =  1.D2
      DO K =1,Nr
       delZ(K) = UNSET_RL
       delP(K) = UNSET_RL
       delR(K) = UNSET_RL
      ENDDO
      dxSpacing = 20.D0 * 1000.D0
      dySpacing = 20.D0 * 1000.D0
      DO i=1,Nx
       delX(i) = dxSpacing
      ENDDO
      DO j=1,Ny
       delY(j) = dySpacing
      ENDDO
C     In spherical polar distances are in degrees
      usingSphericalPolarGrid = .FALSE.
      phiMin    = -5.0
      thetaMin  = 0.
      rSphere   = 6370. * 1.D3
      recip_rSphere  = 1.D0/rSphere
      IF ( usingSphericalPolarGrid ) THEN
       dxSpacing = 1.
       dySpacing = 1.
       DO I=1,Nx
        delX(I) = dxSpacing
       ENDDO
       DO J=1,Ny
        delY(J) = dySpacing
       ENDDO
      ENDIF 

      READ(UNIT=iUnit,NML=PARM04,IOSTAT=errIO,err=9)
      IF ( errIO .GE. 0 ) GOTO 10
    9 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM04'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
  10  CONTINUE
C
      IF ( rSphere .NE. 0 ) THEN
       recip_rSphere = 1.D0/rSphere
      ELSE
       recip_rSphere = 0.
      ENDIF
C--   Initialize EOS coefficients (3rd order polynomial)
      IF (eostype.eq.'POLY3') THEN
       OPEN(37,FILE='POLY3.COEFFS',STATUS='OLD',FORM='FORMATTED')
       READ(37,*) I
       IF (I.NE.Nr) THEN
        WRITE(msgBuf,'(A)')
     &  'ini_parms: attempt to read POLY3.COEFFS failed'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  '           because bad # of levels in data'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'Bad data in POLY3.COEFFS'
       ENDIF
       READ(37,*) (eosRefT(K),eosRefS(K),eosSig0(K),K=1,Nr)
       DO K=1,Nr
        READ(37,*) (eosC(I,K),I=1,9)
       ENDDO
       CLOSE(37)
      ENDIF
C--   Check for conflicting grid definitions.
      goptCount = 0
      IF ( usingCartesianGrid )      goptCount = goptCount+1
      IF ( usingSphericalPolarGrid ) goptCount = goptCount+1
      IF ( goptCount .NE. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: More than one coordinate system requested'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
C--   Make metric term settings consistent with underlying grid.
      IF ( usingCartesianGrid ) THEN
       usingSphericalPolarMterms = .FALSE.
       metricTerms = .FALSE.
       mTFacMom = 0
       useBetaPlaneF = .TRUE.
      ENDIF
      IF ( usingSphericalPolarGrid ) THEN
       useConstantF  = .FALSE.
       useBetaPlaneF = .FALSE.
       useSphereF    = .TRUE.
       omega         = 2.D0 * PI / ( 3600.D0 * 24.D0 )
       usingSphericalPolarMterms = .TRUE.
       metricTerms = .TRUE.
       mTFacMom = 1
      ENDIF
C--   p, z, r coord parameters
      DO K = 1, Nr
       IF ( delZ(K) .NE. UNSET_RL ) zCoordInputData = .TRUE.
       IF ( delP(K) .NE. UNSET_RL ) pCoordInputData = .TRUE.
       IF ( delR(K) .NE. UNSET_RL ) rCoordInputData = .TRUE.
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delZ(K)
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delP(K)
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delRDefault
      ENDDO
C     Check for multiple coordinate systems
      coordsSet = 0
      IF ( zCoordInputData ) coordsSet = coordsSet + 1
      IF ( pCoordInputData ) coordsSet = coordsSet + 1
      IF ( rCoordInputData ) coordsSet = coordsSet + 1
      IF ( coordsSet .GT. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot mix z, p and r in the input data.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

C--   Input files
      bathyFile       = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
      zonalWindFile   = ' '
      meridWindFile   = ' '
      thetaClimFile   = ' '
      saltClimFile    = ' '
      READ(UNIT=iUnit,NML=PARM05,IOSTAT=errIO,err=11)
      IF ( errIO .GE. 0 ) GOTO 12
   11 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist PARM05'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
  12  CONTINUE

C
C--   Set factors required for mixing pressure and meters as vertical coordinate.
C     rkFac is a "sign" parameter which is used where the orientation of the vertical
C     coordinate (pressure or meters) relative to the vertical index (K) is important.
C     rkFac =  1 applies when K and the coordinate are in the opposite sense.
C     rkFac = -1 applies when K and the coordinate are in the same sense.
C     horiVertRatio is a parameter that maps horizontal units to vertical units.
C     It is used in certain special cases where lateral and vertical terms are
C     being combined and a single frame of reference is needed.
      IF ( zCoordInputData .AND. rkFac .EQ. UNSET_I ) THEN 
       rkFac       = 1.D0
       horiVertRatio = 1.D0
      ENDIF
      IF ( pCoordInputData .AND. rkFac .EQ. UNSET_I ) THEN
       rkFac = -1.D0
       horiVertRatio = Gravity * rhoConst
      ENDIF
      IF ( rCoordInputData .AND. rkFac .EQ. UNSET_I ) THEN
       rkFac =  1.D0
       horiVertRatio = 1.D0
      ENDIF
      recip_rkFac = 1.D0 / rkFac
      recip_horiVertRatio = 1./horiVertRatio
      IF ( zCoordInputData ) usingZCoords = .TRUE.
      IF ( pCoordInputData ) usingPCoords = .TRUE.
C
      CLOSE(iUnit)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER
C

      RETURN
      END

