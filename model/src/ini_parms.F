C $Header: /u/gcmpack/MITgcm/model/src/ini_parms.F,v 1.44 1999/06/29 18:33:26 adcroft Exp $

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
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CG2D.h"
#ifdef ALLOW_KPP
#include "KPPMIX.h"
#endif

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
      _RL delRDefault(Nr)
      _RS rkFacDefault
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
     &   viscAh,  viscAz,  viscA4, cosPower,
     &  diffKhT, diffKzT, diffK4T, 
     &  diffKhS, diffKzS, diffK4S,
     &  GMmaxslope,GMlength,GMalpha,GMdepth,GMkbackground,GMmaxval,
     &  tRef, sRef, eosType,
     & no_slip_sides,no_slip_bottom,
     & momViscosity,  momAdvection, momForcing, useCoriolis,
     & momPressureForcing, metricTerms,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & implicitFreeSurface, rigidLid, freeSurfFac, hFacMin, hFacMinDz,
     & tempStepping, saltStepping, momStepping, implicitDiffusion,
     & viscAr, diffKrT, diffKrS, hFacMinDr,
     & viscAp, diffKpT, diffKpS, hFacMinDp,
     & rhoConst, buoyancyRelation, HeatCapacity_Cp,
     & writeBinaryPrec, readBinaryPrec, writeStatePrec,
     & openBoundaries, nonHydrostatic, globalFiles,
     & allowFreezing

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
     & zonalWindFile, meridWindFile,
     & thetaClimFile, saltClimFile,
     & surfQfile, EmPmRfile

C--   Open Boundaries
      NAMELIST /PARM06/
     & OB_Jnorth, OB_Jsouth, OB_Ieast, OB_Iwest

#ifdef ALLOW_KPP
C--   KPP vertical mixing parameters
      NAMELIST /PARM07/
     & usingKPPmixing, KPPmixingMaps, KPPwriteState,
     & minKPPviscAz, maxKPPviscAz, minKPPghat, maxKPPghat,
     & minKPPdiffKzT, maxKPPdiffKzT, minKPPdiffKzS, maxKPPdiffKzS,
     & epsln, epsilon, vonk, conc1, conam, concm, conc2, zetam,
     & conas, concs, conc3, zetas,
     & Ricr, cekman, cmonob, concv, hbf, Vtc,
     & zmin, zmax, umin, umax,
     & num_v_smooth_Ri, num_v_smooth_BV,
     & num_z_smooth_sh, num_m_smooth_sh,
     & Riinfty, BVSQcon, difm0, difs0, dift0,
     & difmiw, difsiw, diftiw, difmcon, difscon, diftcon,
     & cstar, cg
#endif

C
      _BEGIN_MASTER(myThid)

C     Defaults values for input parameters
      CALL SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault, rkFacDefault,
     I   myThid )

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
     &     IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
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
      ENDIF     

      DO WHILE ( .TRUE. )
       READ(modelDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter )
     &     WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
        WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
      ENDDO
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
      DO WHILE ( .TRUE. )
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      ENDDO
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)


C--   Read settings from model parameter file "data".
      iUnit = scrUnit1
      REWIND(iUnit)

C--   Set default "physical" parameters
      viscAz   = UNSET_RL    
      viscAr   = UNSET_RL
      viscAp   = UNSET_RL
      diffKzT  = UNSET_RL
      diffKpT  = UNSET_RL
      diffKrT  = UNSET_RL
      diffKzS  = UNSET_RL
      diffKpS  = UNSET_RL
      diffKrS  = UNSET_RL
      gBaro    = UNSET_RL
      rhoConst = UNSET_RL
      hFacMinDr           = UNSET_RL
      hFacMinDz           = UNSET_RL
      hFacMinDp           = UNSET_RL
      READ(UNIT=iUnit,NML=PARM01) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
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
      ENDIF
      IF ( implicitFreeSurface ) freeSurfFac = 1.D0
      IF ( rigidLid            ) freeSurfFac = 0.D0
      IF ( gBaro .EQ. UNSET_RL ) gBaro=gravity
      IF ( rhoConst .EQ. UNSET_RL ) rhoConst=rhoNil
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
      IF ( rhoNil .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: rhoNil must be greater than 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_rhoNil = 1.D0 / rhoNil
      ENDIF
      IF ( HeatCapacity_Cp .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: HeatCapacity_Cp must be greater than 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_Cp = 1.D0 / HeatCapacity_Cp
      ENDIF
      IF ( gravity .LE. 0. ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: gravity must be greater than 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ELSE
       recip_gravity = 1.D0 / gravity
      ENDIF
C     Set globalFiles flag for READ_WRITE_FLD package
      CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )

C--   Elliptic solver parameters
      READ(UNIT=iUnit,NML=PARM02) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
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
      ENDIF    

C--   Time stepping parameters
      rCD               = -1.D0
      READ(UNIT=iUnit,NML=PARM03) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
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
      ENDIF   
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

C     o Start time
      IF ( nIter0 .NE. 0 .AND. startTime .EQ. 0. )
     &   startTime = deltaTClock*float(nIter0)
C     o nIter0
      IF ( nIter0 .EQ. 0 .AND. startTime .NE. 0. )
     &   nIter0 = INT( startTime/deltaTClock )
C     o End Time
      IF ( nTimeSteps .NE. 0 .AND. endTime .EQ. 0. )
     &   endTime = startTime + deltaTClock*float(nTimeSteps)
C     o nTimeSteps
      IF ( nTimeSteps .EQ. 0 .AND. endTime .NE. 0. )
     &   nTimeSteps = INT((endTime-startTime)/deltaTClock)
C     o Consistent?
      IF ( endTime-startTime .NE. deltaTClock*float(nTimeSteps) ) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: both endTime and nTimeSteps have been set'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: but are inconsistent'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

C     o If taveFreq is finite, then we must make sure the diagnostics
C       code is being compiled
#ifndef INCLUDE_DIAGNOSTICS_INTERFACE_CODE
      IF (taveFreq.NE.0.) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: taveFreq <> 0  but you have'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'not compiled the model with the diagnostics routines.'
        CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A,A)')
     &  'Re-compile with:  #define INCLUDE_DIAGNOSTICS_INTERFACE_CODE',
     &  '              or  -DINCLUDE_DIAGNOSTICS_INTERFACE_CODE'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF
#endif
 
C--   Grid parameters
C     In cartesian coords distances are in metres
      rkFac = UNSET_RS
      DO K =1,Nr
       delZ(K) = UNSET_RL
       delP(K) = UNSET_RL
       delR(K) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      recip_rSphere  = 1.D0/rSphere
      dxSpacing = UNSET_RL
      dySpacing = UNSET_RL
      READ(UNIT=iUnit,NML=PARM04) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
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
      ENDIF    
C
      IF ( dxSpacing .NE. UNSET_RL ) THEN
       DO i=1,Nx
        delX(i) = dxSpacing
       ENDDO
      ENDIF
      IF ( dySpacing .NE. UNSET_RL ) THEN
       DO j=1,Ny
        delY(j) = dySpacing
       ENDDO
      ENDIF
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
       IF ( delR(K) .EQ. UNSET_RL ) delR(K) = delRDefault(K)
       IF ( delR(K) .EQ. 0. ) THEN
         WRITE(msgBuf,'(A,I4)')
     &  'S/R INI_PARMS: No value for delZ/delP/delR at K = ',K
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R INI_PARMS'
       ENDIF
      ENDDO
C     Check for multiple coordinate systems
      CoordsSet = 0
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
      READ(UNIT=iUnit,NML=PARM05) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN    
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
      ENDIF       

C
C--   Set factors required for mixing pressure and meters as vertical coordinate.
C     rkFac is a "sign" parameter which is used where the orientation of the vertical
C     coordinate (pressure or meters) relative to the vertical index (K) is important.
C     rkFac =  1 applies when K and the coordinate are in the opposite sense.
C     rkFac = -1 applies when K and the coordinate are in the same sense.
C     horiVertRatio is a parameter that maps horizontal units to vertical units.
C     It is used in certain special cases where lateral and vertical terms are
C     being combined and a single frame of reference is needed.
      IF ( zCoordInputData .AND. rkFac .EQ. UNSET_RS ) THEN 
       rkFac       = 1.D0
       horiVertRatio = 1.D0
      ENDIF
      IF ( pCoordInputData .AND. rkFac .EQ. UNSET_RS ) THEN
       rkFac = -1.D0
       horiVertRatio = Gravity * rhoConst
      ENDIF
      IF ( rCoordInputData .AND. rkFac .EQ. UNSET_RS ) THEN
       rkFac =  1.D0
       horiVertRatio = 1.D0
      ENDIF
      IF ( rkFac .EQ. UNSET_RS ) rkFac=rkFacDefault
      recip_rkFac = 1.D0 / rkFac
      recip_horiVertRatio = 1./horiVertRatio
      IF ( zCoordInputData ) usingZCoords = .TRUE.
      IF ( pCoordInputData ) usingPCoords = .TRUE.

C-- OBCS
      IF (openBoundaries) THEN
       READ(UNIT=iUnit,NML=PARM06)
       DO J=1,Ny
        if (OB_Ieast(J).lt.0) OB_Ieast(J)=OB_Ieast(J)+Nx+1
       ENDDO
       DO I=1,Nx
        if (OB_Jnorth(I).lt.0) OB_Jnorth(I)=OB_Jnorth(I)+Ny+1
       ENDDO
       write(0,*) 'OB Jn =',OB_Jnorth
       write(0,*) 'OB Js =',OB_Jsouth
       write(0,*) 'OB Ie =',OB_Ieast
       write(0,*) 'OB Iw =',OB_Iwest
      ENDIF

#ifdef ALLOW_KPP
C--   KPP vertical mixing parameters
      usingKPPmixing      = .FALSE.
      KPPmixingMaps       = .FALSE.
      KPPwriteState       = .FALSE.
      minKPPghat    = 0.
      maxKPPghat    = 1.e10
      minKPPviscAz  = 0.
      DO K =1,Nr
       maxKPPviscAz(K) = 100.
      ENDDO
      minKPPdiffKzT = 0.
      maxKPPdiffKzT = 100.
      minKPPdiffKzS = 0.
      maxKPPdiffKzS = 100.

c-----------------------------------------------------------------------
c define some non-dimensional constants and
c the vertical mixing coefficients in m-k-s units
c-----------------------------------------------------------------------

      epsln   = 1.e-20      !
      epsilon = 0.1         ! nondimensional extent of surface layer
      vonk    = 0.40        ! von Karmans constant
      conc1   = 5.0         ! scalar coefficients
      conam   = 1.257       ! .
      concm   = 8.380       ! .
      conc2   = 16.0        ! .
      zetam   = -0.2        ! .
      conas   = -28.86      ! .
      concs   = 98.96       ! .
      conc3   = 16.0        ! .
      zetas   = -1.0        ! .

c     parameters for subroutine "bldepth"

      Ricr    = 0.30        ! critical bulk Richardson Number
      cekman  = 0.7         ! coefficient for ekman depth
      cmonob  = 1.0         ! coefficient for Monin-Obukhov depth
      concv   = 1.8         ! ratio of interior buoyancy frequency to
                            ! buoyancy frequency at entrainment depth
      hbf     = 1.0         ! fraction of bounadry layer depth to
                            ! which absorbed solar radiation
                            ! contributes to surface buoyancy forcing
      Vtc     = concv * sqrt(0.2/concs/epsilon) / vonk**2 / Ricr
                            ! non-dimensional coefficient for velocity
                            ! scale of turbulant velocity shear

c     parameters and common arrays for subroutines "kmixinit" and "wscale"

      zmin    = -4.e-7      ! minimum limit for zehat in table (m3/s3)
      zmax    = 0.0         ! maximum limit for zehat in table (m3/s3)
      umin    = 0.0         ! minimum limit for ustar in table (m/s)
      umax    = .04         ! maximum limit for ustar in table (m/s)

c     parameters for subroutine "Ri_iwmix"

      num_v_smooth_Ri = 1   ! number of times Ri is vertically smoothed
      num_v_smooth_BV = 1   ! number of times BV is vertically smoothed
      num_z_smooth_sh = 0   ! number of times shear is zonally smoothed
      num_m_smooth_sh = 0   ! number of times shear is meridionally smoothed
      Riinfty = 0.7         ! local Richardson Number limit for shear instability
      BVSQcon = -0.2e-4     ! Brunt-Vaisala squared                   (1/s^2)
      difm0   = 0.005       ! max visc due to shear instability       (m^2/s)
      difs0   = 0.005       ! max tracer diffusivity ..               (m^2/s)
      dift0   = 0.005       ! max heat diffusivity ..                 (m^2/s)
      difmiw  = 0.001       ! viscosity from background internal wave (m^2/s)
      difsiw  = 0.00003     ! tracer diffusivity ..                   (m^2/s)
      diftiw  = 0.00003     ! heat diffusivity ..                     (m^2/s)
      difmcon = 0.1         ! viscosity due to convective instability (m^2/s)
      difscon = 0.1         ! tracer diffusivity ..                   (m^2/s)
      diftcon = 0.1         ! heat diffusivity ..                     (m^2/s)

c     parameters for subroutine "blmix"

      cstar   = 10.       ! proportionality coefficient for nonlocal transport

                          ! non-dimensional coefficient for counter-gradient term
      cg      = cstar * vonk * (concs * vonk * epsilon)**(1./3.)

      READ(UNIT=iUnit,NML=PARM07,IOSTAT=errIO,err=13)
      IF ( errIO .GE. 0 ) GOTO 14
   13 CONTINUE
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
     &  'Problem in namelist PARM07'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R INI_PARMS'
   14 CONTINUE
#endif

C
      CLOSE(iUnit)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER
C

      RETURN
      END

