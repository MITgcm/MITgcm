C $Header: /u/gcmpack/MITgcm/model/src/Attic/modeldata_example.F,v 1.8 2001/02/04 14:38:48 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MODELDATA_EXAMPLE( myThid )
C     /==========================================================\
C     | S/R MODELDATA_EXAMPLE                                    |
C     | o Write example data file                                |
C     |==========================================================|
C     | Notes                                                    |
C     | =====                                                    |
C     | Some systems require & as the namelist terminator.       |
C     | Other systems use a / character.                         |
C     \==========================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     -- Routine arguments --
      INTEGER myThid

C     -- Local variables --
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      
      WRITE(msgBuf,'(A)') '// Shown below is an example "data" file.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// To use this example copy and paste the  '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ">" lines. Then remove the text up to' 
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1) 
      WRITE(msgBuf,'(A)') '// and including the ">".'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># Example "data" file'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># Lines beginning "#" are comments'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      WRITE(msgBuf,'(A)') '># '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># o Continuous equation parameters'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   gravity - Accel due to gravity (m.s^2)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   rhonil  - Reference density (kg/m^3)'   
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tAlpha  - Thermal expansion coefficient (1/oC)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   sBeta   - Haline contraction coefficient (1/ppt)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   f0      - Reference coriolis parameter ( 1/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#             ( South edge f on beta plane.)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   beta    - df/dy ( s^-1.m^-1 ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   viscAh  - Horizontal eddy viscosity coefficient '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   viscAz  - Vertical eddy viscosity coefficient '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   viscA4  - Biharmonic eddy viscosity coefficient '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^4/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   diffKhT - Horizontal temperature diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   diffKzT - Vertical temperature diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   diffK4T - Biharmonic temperature diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^4/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   diffKhS - Horizontal salt diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   diffKzS - Vertical salt diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^2/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   diffK4S - Biharmonic salt diffusivity '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#             ( m^4/s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   momStepping - On/Off flag for momentum'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  equation.          '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   momViscosity - On/Off flag for momentum'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  mixing.                 '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   momAdvection - On/Off flag for momentum'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  self transport.         '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   momPressureForcing - On/Off flag for momentum'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#                        pressure terms.         '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   useCoriolis  - On/Off flag for momentum'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  equation coriolis term. '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tempStepping - On/Off flag for temperature'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  eqaution.         '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tempDiffusion- On/Off flag for temperature'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  mixing.                 '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tempAdvection- On/Off flag for temperature'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  transport.         '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tempForcing  - On/Off flag for temperature'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  forcing.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   saltDiffusion- On/Off flag for salt'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  mixing.                 '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   saltAdvection- On/Off flag for salt'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  transport.         '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   saltForcing  - On/Off flag for salt'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  forcing.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tRef         - Reference vertical pot. temp'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   sRef         - Reference vertical salinity'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>&PARM01'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> gravity=9.81,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> rhonil=999.8,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tAlpha=2.e-4,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> sBeta=7e-4'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> f0=1.e-4'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> viscAh=1.e3'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> viscAz=1.e-5'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> viscA4=0.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffKhT=1.e3'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffKzT=1.e-5'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffK4T=0.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffKhS=1.e3'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffKzS=1.e-5'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> diffK4S=0.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> momStepping=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> momViscosity=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> momAdvection=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> momPressureForcing=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> momForcing=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> useCoriolis=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tempStepping=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tempDiffusion=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tempAdvection=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tempForcing=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> saltDiffusion=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> saltAdvection=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> saltForcing=.TRUE.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tRef=20.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> sRef=35.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> implicitFreeSurface=.TRUE.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> rigidLid=.FALSE.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> GMmaxSlope=1.d-2,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> GMlength=200.d3,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> GMalpha=200.d3,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> GMdepth=1000.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> GMkBackground=0.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>/                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      WRITE(msgBuf,'(A)') '># '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># o Elliptic solver parameters'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   cg2dMaxIters - Maximum number of 2d '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                  solver iterations.   '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   cg2dChkReqFreq - Frequency solver tests '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                    convergence.       '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   cg2dTargetResidual - Solver target'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#                        residual.    '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>&PARM02'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> cg2dMaxIters=200,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> cg2dChkResFreq=5,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> cg2dTargetResidual=1.e-7,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>/                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      WRITE(msgBuf,'(A)') '># '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># o Timestepping parameters'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   nIter0 - Start timestep index'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   nTimeSteps - Number of timesteps in run.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   delT - Timestep ( s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   deltaTtracer - Tracer timestep ( s ).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   abEps - Adams-Bashforth stabilising '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#           factor.                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   tauCD - CD scheme coupling timescale (s)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   startTime - Integration starting time (s)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   endTime - Integration ending time (s)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   cAdjFreq - Convective adjustment period (s)'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   chkPtFreq - Frequency at which check  '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#               pointing is done ( s ).   '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   dumpFreq - Frequency at which model  '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#              state is stored ( s ).     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>&PARM03'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> nIter0=0'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> nTimeSteps=5000'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> delT=3600.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> deltaTtracer=3600.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> abEps=0.1'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> tauCD=345600.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> startTime=0.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> endTime=31104000.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> chkPtFreq=864000.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> dumpFreq=2592000.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> cAdjFreq=86400.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>/                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      WRITE(msgBuf,'(A)') '># '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># o Gridding parameters'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   l - Global domain grid-points in X'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   m - Global domain grid-points in Y'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   n - Grid-points in Z'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#   usingSphericalPolarGrid - On/Off flag for'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#       selecting spherical polar coordinates'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   usingCartesianGrid - On/Off flag for'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#        selecting cartesian coordinates'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   delX - Zonal grid spacing. Degrees'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#          for spherical polar and m for'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#          cartesian. A value for each point'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#          in X can be specified.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   delY - Meridional grid spacing. Degrees'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#          for spherical polar and m for'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') 
     & '>#          cartesian. A value for each point'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#          in Y can be specified.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   delZ - Vertical grid spacing (m).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   delP - Vertical grid spacing (Pa).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   phiMin - Southern boundary latitude'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#            (spherical polar grid).   '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#   rSphere- Radius of globe           '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>#            (spherical polar grid).   '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>&PARM04'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> n=20,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> l=122,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> m=86,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> usingSphericalPolarGrid=.TRUE.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> usingCartesianGrid=.FALSE.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> delx=1.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> dely=1.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> delz=  100.,  100.,  100.,  100.,  100.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>        100.,  100.,  100.,  100.,  100.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>        100.,  100.,  100.,  100.,  100.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>        100.,  100.,  100.,  100.,  100.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> phiMin=-80.,'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '> rSphere=6430.E3'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>/                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)

      WRITE(msgBuf,'(A)') '># Note: Some systems use & as the '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># namelist terminator. Other systems'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># use a / character (as shown here).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
C
      RETURN
      END

