C $Header: /u/gcmpack/MITgcm/model/src/config_summary.F,v 1.7 1998/05/21 18:25:48 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE CONFIG_SUMMARY( myThid )
C     /==========================================================\
C     | SUBROUTINE CONFIG_SUMMARY                                |
C     | o Summarize model prognostic variables.                  |
C     |==========================================================|
C     | This routine writes a tabulated summary of the model     |
C     | configuration.                                           |
C     | Note                                                     |
C     |  1. Under multi-process parallelism the summary          |
C     |     is only given for the per-process data.              |
C     |  2. Under multi-threading the summary is produced by     |
C     |     the master thread. This threads reads data managed by|
C     |     other threads.                                       |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of CONFIG_SUMMARY
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER                  I,J,K
      INTEGER                  bi, bj
      REAL                     xcoord(Nx)
      REAL                     ycoord(Ny)
      REAL                     zcoord(Nz)


      _BARRIER
      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Model configuration'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// "Physical" paramters ( PARM01 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_R8( tRef, Nz, INDEX_K,'tRef =',
     &'   /* Reference temperature profile ( oC or oK ) */')
      CALL WRITE_1D_R8( sRef, Nz, INDEX_K,'sRef =',
     &'   /* Reference salinity profile ( ppt ) */')
      CALL WRITE_1D_R8( viscAh, 1, INDEX_NONE,'viscAh =',
     &'   /* Lateral eddy viscosity ( m^2/s ) */')
      CALL WRITE_1D_R8( viscAz, 1, INDEX_NONE,'viscAz =',
     &'   /* Vertical eddy viscosity ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKhT, 1, INDEX_NONE,'diffKhT =',
     &'   /* Laplacian diffusion of heat laterally ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKzT, 1, INDEX_NONE,'diffKzT =',
     &'   /* Laplacian diffusion of heat vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( diffK4T, 1, INDEX_NONE,'diffK4T =',
     &'   /* Bihaarmonic diffusion of heat laterally ( m^4/s ) */')
      CALL WRITE_1D_R8( diffKhS, 1, INDEX_NONE,'diffKhS =',
     &'   /* Laplacian diffusion of salt laterally ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKzS, 1, INDEX_NONE,'diffKzS =',
     &'   /* Laplacian diffusion of salt vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( diffK4S, 1, INDEX_NONE,'diffK4S =',
     &'   /* Bihaarmonic diffusion of salt laterally ( m^4/s ) */')
      CALL WRITE_1D_R8( tAlpha,1, INDEX_NONE,'tAlpha =',
     &'   /* Linear EOS thermal expansion coefficient ( 1/degree ) */')
      CALL WRITE_1D_R8( sBeta, 1, INDEX_NONE,'sBeta =',
     &'   /* Linear EOS haline contraction coefficient ( 1/ppt ) */')
      CALL WRITE_1D_R8( rhonil,1, INDEX_NONE,'rhonil =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_1D_R8( gravity,1, INDEX_NONE,'gravity =',
     &'   /* Gravitational acceleration ( m/s^2 ) */')
      CALL WRITE_1D_R8( f0,1, INDEX_NONE,'f0 =',
     &'   /* Reference coriolis parameter ( 1/s ) */')
      CALL WRITE_1D_R8( beta,1, INDEX_NONE,'beta =',
     &'   /* Beta ( 1/(m.s) ) */')
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Elliptic solver(s) paramters ( PARM02 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_I( cg2dMaxIters,1, INDEX_NONE,'cg2dMaxIters =',
     &'   /* Upper limit on 2d con. grad iterations  */')
      CALL WRITE_1D_I( cg2dChkResFreq,1, INDEX_NONE,'cg2dChkResFreq =',
     &'   /* 2d con. grad convergence test frequency */')
      CALL WRITE_1D_R8( cg2dTargetResidual,1, INDEX_NONE,'cg2dTargetResidual =',
     &'   /* 2d con. grad target residual  */')

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Time stepping paramters ( PARM03 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_I( nIter0,1, INDEX_NONE,'nIter0 =',
     &'   /* Base timestep number  */')
      CALL WRITE_1D_I( nTimeSteps,1, INDEX_NONE,'nTimeSteps =',
     &'   /* Number of timesteps */')
      CALL WRITE_1D_R8( deltaTmom,1, INDEX_NONE,'deltatTmom =',
     &'   /* Momentum equation timestep ( s ) */')
      CALL WRITE_1D_R8( deltaTtracer,1, INDEX_NONE,'deltatTtracer =',
     &'   /* Tracer equation timestep ( s ) */')
      CALL WRITE_1D_R8( abEps,1, INDEX_NONE,'abEps =',
     &'   /* Adams-Bashforth stabilizing weight */')
      CALL WRITE_1D_R8( tauCD,1, INDEX_NONE,'tauCD =',
     &'   /* CD coupling time-scale ( s ) */')
      CALL WRITE_1D_R8( rCD,1, INDEX_NONE,'rCD =',
     &'   /* Normalised CD coupling parameter */')
      CALL WRITE_1D_R8( startTime,1, INDEX_NONE,'startTime =',
     &'   /* Run start time ( s ). */')
      CALL WRITE_1D_R8( endTime,1, INDEX_NONE,'endTime =',
     &'   /* Integration ending time ( s ). */')
      CALL WRITE_1D_R8( pChkPtFreq,1, INDEX_NONE,'pChkPtFreq =',
     &'   /* Permanent restart/checkpoint file interval ( s ). */')
      CALL WRITE_1D_R8( chkPtFreq,1, INDEX_NONE,'chkPtFreq =',
     &'   /* Rolling restart/checkpoint file interval ( s ). */')
      CALL WRITE_1D_R8( dumpFreq,1, INDEX_NONE,'dumpFreq =',
     &'   /* Model state write out interval ( s ). */')

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Gridding paramters ( PARM04 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_L( usingCartesianGrid,1, INDEX_NONE,'usingCartesianGrid =',
     &'   /* Cartesian coordinates flag ( True / False ) */')
      CALL WRITE_1D_L( usingSphericalPolarGrid,1, INDEX_NONE,'usingSphericalPolarGrid =',
     &'   /* Spherical coordinates flag ( True / False ) */')
      CALL WRITE_1D_R8( delZ,Nz, INDEX_K,'delZ = ',
     &'   /* W spacing ( m ) */')
      CALL WRITE_1D_R8( delX, Nx, INDEX_I,'delX = ',
     &'   /* U spacing ( m - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( delY, Ny, INDEX_J,'delY = ',
     &'   /* V spacing ( m - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( phiMin, 1, INDEX_NONE,'phiMin = ',
     &'   /* Southern boundary ( ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( thetaMin, 1, INDEX_NONE,'thetaMin = ',
     &'   /* Western boundary ( ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( rSphere, 1, INDEX_NONE,'rSphere = ',
     &'   /* Radius ( ignored - cartesian, m - spherical ) */')
      DO bi=1,nSx
       DO I=1,sNx
        xcoord((bi-1)*sNx+I) = xc(I,1,bi,1)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( xcoord, Nx, INDEX_I,'xcoord = ',
     &'   /* P-point X coordinate (  m - cartesian, degrees - spherical ) */')
      DO bj=1,nSy
       DO J=1,sNy
        ycoord((bj-1)*sNy+J) = yc(1,J,1,bj)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( ycoord, Ny, INDEX_J,'ycoord = ',
     &'   /* P-point Y coordinate (  m - cartesian, degrees - spherical ) */')
      DO K=1,Nz
       zcoord(K) = zc(K)
      ENDDO
      CALL WRITE_1D_R8( zcoord, Nz, INDEX_K,'zcoord = ',
     &'   /* P-point Z coordinate (  m ) */')



      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)
      _BARRIER


      RETURN
  100 FORMAT(A,
     &' '
     &)
      END

