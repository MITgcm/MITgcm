C $Header: /u/gcmpack/MITgcm/model/src/ini_parms.F,v 1.18 1998/06/15 05:13:56 cnh Exp $

#include "CPP_EEOPTIONS.h"

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
      REAL dxSpacing
      REAL dySpacing
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      INTEGER goptCount
      INTEGER K, I, J, IL, iUnit
      INTEGER errIO
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C--   Continuous equation parameters
      NAMELIST /PARM01/
     & gravity, gBaro, rhonil, tAlpha, sBeta, f0, beta,
     &   viscAh,  viscAz,  viscA4,
     &  diffKhT, diffKzT, diffK4T,
     &  diffKhS, diffKzS, diffK4S,
     &  GMmaxslope,GMlength,GMalpha,GMdepth,GMkbackground,
     &  tRef, sRef, eosType,
     & momViscosity,  momAdvection, momForcing, useCoriolis,
     & momPressureForcing, metricTerms,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing,
     & implicitFreeSurface, rigidLid, freeSurfFac,
     & tempStepping, saltStepping, momStepping, implicitDiffusion

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dChkResFreq, cg2dTargetResidual, cg2dpcOffDFac

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, deltaT, deltaTmom, deltaTtracer, abEps, tauCD, rCD,
     & startTime, endTime, chkPtFreq, dumpFreq, deltaTClock, pChkPtFreq,
     & cAdjFreq, tauThetaClimRelax, tauSaltClimRelax

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, delZ, dxSpacing, dySpacing, delX, delY,
     & usingSphericalPolarGrid, phiMin, thetaMin, rSphere,
     & l, m, n

C--   Input files
      NAMELIST /PARM05/
     & bathyFile, hydrogThetaFile, hydrogSaltFile, 
     & zonalWindFile, meridWindFile, thetaClimFile,
     & saltClimFile

C
      _BEGIN_MASTER(myThid)

C--   Open the parameter file
      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
      OPEN(UNIT=modelDataUnit,FILE='data',STATUS='OLD',err=1,IOSTAT=errIO)
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
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Model parameter file "data"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
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
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
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
      DO K =1,Nz
       tRef(K) = 30.D0 - FLOAT(K)
      ENDDO
      gravity =   9.81 D0
      gBaro   = gravity
      rhoNil   = 999.8 D0
      f0=1.D-4 
      beta = 1. _d -11
      viscAh=1.d3
      diffKhT=1.0d3
      diffKhS=1.0d3
      viscAz=1.d-3
      diffKzT=1.d-5
      diffKzS=1.d-5
      viscA4=0.
      diffK4T=0.
      diffK4S=0.
      GMmaxslope=1.d-2
      GMlength=200.d3
      GMalpha=0.
      GMdepth=1000.
      GMkbackground=0.
      tAlpha=2.d-4
      sBeta=7.4d-4
      eosType='LINEAR'
      implicitFreeSurface = .TRUE.
      rigidLid            = .FALSE.
      freeSurfFac         = 1. _d 0
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
      IF ( implicitFreeSurface ) freeSurfFac = 1. _d 0
      IF ( rigidLid            ) freeSurfFac = 0. _d 0
      IF ( momViscosity        ) THEN
       vfFacMom = 1. _d 0
      ELSE
       vfFacMom = 0. _d 0
      ENDIF
      IF ( momAdvection        ) THEN
       afFacMom = 1. _d 0
      ELSE
       afFacMom = 0. _d 0
      ENDIF
      IF ( momForcing ) THEN
       foFacMom = 1. _d 0
      ELSE
       foFacMom = 0. _d 0
      ENDIF
      IF ( useCoriolis ) THEN
       cfFacMom = 1. _d 0
      ELSE
       cfFacMom = 0. _d 0
      ENDIF
      IF ( momPressureForcing ) THEN
       pfFacMom = 1. _d 0
      ELSE
       pfFacMom = 0. _d 0
      ENDIF
      IF ( metricTerms ) THEN
       mTFacMom = 1. _d 0
      ELSE
       mTFacMom = 1. _d 0
      ENDIF

      IF ( implicitFreeSurface .AND.  rigidLid ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: Cannot select implicitFreeSurface and rigidLid.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

C--   Elliptic solver parameters
      cg2dMaxIters   = 150
      cg2dTargetResidual = 1. _d -7
      cg2dChkResFreq = 1
      cg2dpcOffDFac  = 0.51 _d 0
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
      startTime      = 0.
      nTimeSteps     = 0
      endTime        = 0.
      nIter0         = 0
      deltaT         = 0.
      deltaTClock    = 0.
      deltaTtracer   = 0.
      deltaTMom      = 0.
      abEps          = 0.01
      pchkPtFreq     = 0.
      chkPtFreq      = 3600.*25
      dumpFreq       = 3600.*100
      writeStatePrec = precFloat32
      nCheckLev      = 1
      checkPtSuff(1) = 'ckptA'
      checkPtSuff(2) = 'ckptB'
      cAdjFreq       = -1. _d 0
      rCD            = -1. _d 0
      tauCD          = 0. _d 0
      tauThetaClimRelax = 0. _d 0
      doThetaClimRelax  = .FALSE.
      tauSaltClimRelax  = 0. _d 0
      doSaltClimRelax   = .FALSE.
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
C     o Convection frequency
      IF ( cAdjFreq .LT. 0. ) THEN
       cAdjFreq = deltaTClock
      ENDIF
C     o CD coupling
      IF ( tauCD .EQ. 0. _d 0 ) THEN
        tauCD = deltaTmom
      ENDIF
      IF ( rCD .LT. 0. ) THEN
       rCD = 1. - deltaTMom/tauCD
      ENDIF
C     o Temperature climatology relaxation time scale
      IF ( tauThetaClimRelax .EQ. 0. _d 0 ) THEN
       doThetaClimRelax     = .FALSE.
       lambdaThetaClimRelax = 0. _d 0
      ELSE
       doThetaClimRelax     = .TRUE.
       lambdaThetaClimRelax = 1./tauThetaClimRelax
      ENDIF
C     o Salinity climatology relaxation time scale
      IF ( tauSaltClimRelax .EQ. 0. _d 0 ) THEN
       doSaltClimRelax     = .FALSE.
       lambdaSaltClimRelax = 0. _d 0
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
 
C--   Grid parameters
C     In cartesian coords distances are in metres
      usingCartesianGrid = .TRUE.
      DO K =1,Nz
       delZ(K) = 100. _d 0
      ENDDO
      dxSpacing = 20. _d 0 * 1000. _d 0
      dySpacing = 20. _d 0 * 1000. _d 0
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
      rSphere   = 6370. * 1. _d 3
      rRsphere  = 1. _d 0/rSphere
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

      IF ( rSphere .NE. 0 ) THEN
       rRSphere = 1. _d 0/rSphere
      ELSE
       rRSphere = 0.
      ENDIF

C     Initialize EOS coefficients (3rd order polynomial)
      IF (eostype.eq.'POLY3') THEN
       OPEN(37,FILE='POLY3.COEFFS',STATUS='OLD',FORM='FORMATTED')
       READ(37,*) I
       IF (I.NE.Nz) THEN
        WRITE(0,*) 'ini_parms: attempt to read POLY3.COEFFS failed'
        WRITE(0,*) '           because bad # of levels in data'
        STOP 'Bad data in POLY3.COEFFS'
       ENDIF
       READ(37,*) (eosRefT(K),eosRefS(K),eosSig0(K),K=1,Nz)
       DO K=1,Nz
        READ(37,*) (eosC(I,K),I=1,9)
        write(0,'(i3,13f8.3)') K,eosRefT(K),eosRefS(K),eosSig0(K),
     &                (eosC(I,K),I=1,9)
       ENDDO
       CLOSE(37)
      ENDIF

      goptCount = 0
      IF ( usingCartesianGrid )      goptCount = goptCount+1
      IF ( usingSphericalPolarGrid ) goptCount = goptCount+1
      IF ( goptCount .NE. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: More than one coordinate system requested'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

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
       omega         = 2. _d 0 * PI / ( 3600. _d 0 * 24. _d 0 )
       usingSphericalPolarMterms = .TRUE.
       metricTerms = .TRUE.
       mTFacMom = 1
      ENDIF

C--   Input files
      bathyFile       = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
      zonalWindFile   = ' '
      meridWindFile   = ' '
      thetaClimFile   = ' '
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

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER
C

      RETURN
      END

