C $Header: /u/gcmpack/MITgcm/model/src/ini_parms.F,v 1.5 1998/04/27 04:24:22 cnh Exp $

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
     & gravity, rhonil, tAlpha, sBeta, f0, beta,
     &   viscAh,  viscAz,  viscA4,
     &  diffKhT, diffKzT, diffK4T,
     &  diffKhS, diffKzS, diffK4S,
     &  tRef, sRef,
     & momViscosity,  momAdvection, momForcing, useCoriolis,
     & tempDiffusion, tempAdvection, tempForcing,
     & saltDiffusion, saltAdvection, saltForcing

C--   Elliptic solver parameters
      NAMELIST /PARM02/
     & cg2dMaxIters, cg2dChkResFreq, cg2dTargetResidual

C--   Time stepping parammeters
      NAMELIST /PARM03/
     & nIter0, nTimeSteps, deltaT, deltaTmom, deltaTtracer, abEps, tauCD, rCD,
     & startTime, endTime, chkPtFreq, dumpFreq

C--   Gridding parameters
      NAMELIST /PARM04/
     & usingCartesianGrid, delZ, dxSpacing, dySpacing, delX, delY,
     & usingSphericalPolarGrid, phiMin, thetaMin, rSphere,
     & l, m, n

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
      rhoNil   = 999.8 D0
      tAlpha=2.d-4
      sBeta=1.d-5
      f0=1.D-4 
      beta = 1. _d -11
      viscAh=1.d3
      viscA4=0.
      viscAz=1.d-3
      diffKhT=1.0d3
      diffKzT=1.d-5
      diffKhS=1.0d3
      diffKzS=1.d-1
      diffK4T=0.
      diffK4S=0.
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

C--   Elliptic solver parameters
      cg2dMaxIters   = 150
      cg2dTargetResidual = 1. _d -7
      cg2dChkResFreq = 1
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
      deltaTtracer   = 0.
      deltaTMom      = 0.
      abEps          = 0.01
      chkPtFreq      = 3600.*25
      dumpFreq       = 3600.*100
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
C     o Time step count
      IF ( endTime .NE. 0 ) THEN
       IF ( deltaT .NE. 0 ) nTimeSteps = INT(endTime/deltaT)
      ENDIF
C     o Finish time
      IF ( endTime .EQ. 0. ) endTime = FLOAT(nTimeSteps)*deltaT
 
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
      IF ( usingSphericalPolarGrid ) THEN
       phiMin    = -5.0
       thetaMin  = 0.
       dxSpacing = 1.
       dySpacing = 1.
       rSphere   = 6370. * 1. _d 3
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

      goptCount = 0
      IF ( usingCartesianGrid )      goptCount = goptCount+1
      IF ( usingSphericalPolarGrid ) goptCount = goptCount+1
      IF ( goptCount .NE. 1 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS: More than one coordinate system requested'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R INI_PARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER
C

      RETURN
      END

