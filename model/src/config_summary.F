C $Header: /u/gcmpack/MITgcm/model/src/config_summary.F,v 1.23 2000/06/21 19:14:10 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CONFIG_SUMMARY( myThid )
C     /==========================================================
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
      IMPLICIT NONE

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
      _RL                     xcoord(Nx)
      _RL                     ycoord(Ny)
      _RL                     rcoord(Nr)


      _BARRIER
      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Model configuration'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     & '// "Physical" paramters ( PARM01 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_R8( tRef, Nr, INDEX_K,'tRef =',
     &'   /* Reference temperature profile ( oC or oK ) */')
      CALL WRITE_1D_R8( sRef, Nr, INDEX_K,'sRef =',
     &'   /* Reference salinity profile ( ppt ) */')
      CALL WRITE_0D_R8( viscAh, INDEX_NONE,'viscAh =',
     &'   /* Lateral eddy viscosity ( m^2/s ) */')
      CALL WRITE_0D_R8( viscA4, INDEX_NONE,'viscAh =',
     &'   /* Lateral biharmonic viscosity ( m^4/s ) */')
      CALL WRITE_0D_L( no_slip_sides, INDEX_NONE,
     & 'no_slip_sides =', '  /* Viscous BCs: No-slip sides */')
      IF ( viscAz .NE. UNSET_RL ) THEN
       CALL WRITE_0D_R8( viscAz, INDEX_NONE,'viscAz =',
     & '   /* Vertical eddy viscosity ( m^2/s ) */')
      ENDIF
      IF ( viscAp .NE. UNSET_RL ) THEN
       CALL WRITE_0D_R8( viscAp, INDEX_NONE,'viscAp =',
     & '   /* Vertical eddy viscosity ( Pa^2/s ) */')
      ENDIF
      CALL WRITE_0D_R8( viscAr,  INDEX_NONE,'viscAr =',
     &'   /* Vertical eddy viscosity ( units of r^2/s ) */')
      CALL WRITE_0D_R8( diffKhT, INDEX_NONE,'diffKhT =',
     &'   /* Laplacian diffusion of heat laterally ( m^2/s ) */')
      CALL WRITE_0D_R8( diffK4T, INDEX_NONE,'diffK4T =',
     &'   /* Bihaarmonic diffusion of heat laterally ( m^4/s ) */')
      CALL WRITE_0D_R8( diffKzT, INDEX_NONE,'diffKzT =',
     &'   /* Laplacian diffusion of heat vertically ( m^2/s ) */')
      CALL WRITE_0D_R8( diffKrT, INDEX_NONE,'diffKrT =',
     &'   /* Laplacian diffusion of heat vertically ( m^2/s ) */')
      CALL WRITE_0D_R8( diffKhS, INDEX_NONE,'diffKhS =',
     &'   /* Laplacian diffusion of salt laterally ( m^2/s ) */')
      CALL WRITE_0D_R8( diffK4S, INDEX_NONE,'diffK4S =',
     &'   /* Bihaarmonic diffusion of salt laterally ( m^4/s ) */')
      CALL WRITE_0D_R8( diffKzS, INDEX_NONE,'diffKzS =',
     &'   /* Laplacian diffusion of salt vertically ( m^2/s ) */')
      CALL WRITE_0D_R8( diffKrS, INDEX_NONE,'diffKrS =',
     &'   /* Laplacian diffusion of salt vertically ( m^2/s ) */')
      CALL WRITE_0D_R8( tAlpha,  INDEX_NONE,'tAlpha =',
     &'   /* Linear EOS thermal expansion coefficient ( 1/degree ) */')
      CALL WRITE_0D_R8( sBeta,   INDEX_NONE,'sBeta =',
     &'   /* Linear EOS haline contraction coefficient ( 1/ppt ) */')
      IF ( eosType .EQ. 'POLY3' ) THEN
        WRITE(msgBuf,'(A)') 
     &   '// Polynomial EQS parameters ( from POLY3.COEFFS ) '
        DO K = 1, Nr
         WRITE(msgBuf,'(I3,13F8.3)')
     &   K,eosRefT(K),eosRefS(K),eosSig0(K), (eosC(I,K),I=1,9)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
        ENDDO
      ENDIF
      CALL WRITE_0D_R8( rhonil,  INDEX_NONE,'rhonil =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_0D_R8( rhoConst, INDEX_NONE,'rhoConst =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_0D_R8( gravity, INDEX_NONE,'gravity =',
     &'   /* Gravitational acceleration ( m/s^2 ) */')
      CALL WRITE_0D_R8( gBaro,   INDEX_NONE,'gBaro =',
     &'   /* Barotropic gravity ( m/s^2 ) */')
      CALL WRITE_0D_R8( f0,      INDEX_NONE,'f0 =',
     &'   /* Reference coriolis parameter ( 1/s ) */')
      CALL WRITE_0D_R8( beta,    INDEX_NONE,'beta =',
     &'   /* Beta ( 1/(m.s) ) */')
      CALL WRITE_0D_R8( freeSurfFac, INDEX_NONE,'freeSurfFac =',
     &'   /* Implcit free surface factor */')
      CALL WRITE_0D_L( implicitFreeSurface, INDEX_NONE,
     &                 'implicitFreeSurface =',
     &'   /* Implicit free surface on/off flag */')
      CALL WRITE_0D_L( rigidLid, INDEX_NONE,
     &                 'rigidLid =',
     &'   /* Rigid lid on/off flag */')
      CALL WRITE_0D_L( momStepping,  INDEX_NONE,
     & 'momStepping =', '  /* Momentum equation on/off flag */')
      CALL WRITE_0D_L( momAdvection, INDEX_NONE,
     & 'momAdvection =', '  /* Momentum advection on/off flag */')
      CALL WRITE_0D_L( momViscosity, INDEX_NONE,
     & 'momViscosity =', '  /* Momentum viscosity on/off flag */')
      CALL WRITE_0D_L( useCoriolis,  INDEX_NONE,
     & 'useCoriolis =', '  /* Coriolis on/off flag */')
      CALL WRITE_0D_L( momForcing,   INDEX_NONE,
     & 'momForcing =', '  /* Momentum forcing on/off flag */')
      CALL WRITE_0D_L( momPressureForcing, INDEX_NONE,
     & 'momPressureForcing =',  
     & '  /* Momentum pressure term on/off flag */')
      CALL WRITE_0D_L( tempStepping,  INDEX_NONE,
     & 'tempStepping =', '  /* Temperature equation on/off flag */')
      CALL WRITE_0D_L( openBoundaries, INDEX_NONE,
     & 'openBoundaries =', '  /* OpenBoundaries on/off flag */')
      CALL WRITE_0D_L( nonHydrostatic, INDEX_NONE,
     & 'nonHydrostatic =', '  /* Non-Hydrostatic on/off flag */')
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') 
     & '// Elliptic solver(s) paramters ( PARM02 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_I( cg2dMaxIters,   INDEX_NONE,'cg2dMaxIters =',
     &'   /* Upper limit on 2d con. grad iterations  */')
      CALL WRITE_0D_I( cg2dChkResFreq, INDEX_NONE,'cg2dChkResFreq =',
     &'   /* 2d con. grad convergence test frequency */')
      CALL WRITE_0D_R8( cg2dTargetResidual, INDEX_NONE,
     & 'cg2dTargetResidual =',
     &'   /* 2d con. grad target residual  */')

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     & '// Time stepping paramters ( PARM03 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_I( nIter0, INDEX_NONE,'nIter0 =',
     &'   /* Base timestep number  */')
      CALL WRITE_0D_I( nTimeSteps, INDEX_NONE,'nTimeSteps =',
     &'   /* Number of timesteps */')
      CALL WRITE_0D_R8( deltaTmom, INDEX_NONE,'deltatTmom =',
     &'   /* Momentum equation timestep ( s ) */')
      CALL WRITE_0D_R8( deltaTtracer, INDEX_NONE,'deltatTtracer =',
     &'   /* Tracer equation timestep ( s ) */')
      CALL WRITE_0D_R8( deltaTClock, INDEX_NONE,'deltatTClock  =',
     &'   /* Model clock timestep ( s ) */')
      CALL WRITE_0D_R8( cAdjFreq, INDEX_NONE,'cAdjFreq =',
     &'   /* Convective adjustment interval ( s ) */')
      CALL WRITE_0D_R8( abeps, INDEX_NONE,'abeps =',
     &'   /* Adams-Bashforth stabilizing weight */')
      CALL WRITE_0D_R8( tauCD, INDEX_NONE,'tauCD =',
     &'   /* CD coupling time-scale ( s ) */')
      CALL WRITE_0D_R8( rCD, INDEX_NONE,'rCD =',
     &'   /* Normalised CD coupling parameter */')
      CALL WRITE_0D_R8( startTime, INDEX_NONE,'startTime =',
     &'   /* Run start time ( s ). */')
      CALL WRITE_0D_R8( endTime, INDEX_NONE,'endTime =',
     &'   /* Integration ending time ( s ). */')
      CALL WRITE_0D_R8( pChkPtFreq, INDEX_NONE,'pChkPtFreq =',
     &'   /* Permanent restart/checkpoint file interval ( s ). */')
      CALL WRITE_0D_R8( chkPtFreq, INDEX_NONE,'chkPtFreq =',
     &'   /* Rolling restart/checkpoint file interval ( s ). */')
      CALL WRITE_0D_R8( dumpFreq, INDEX_NONE,'dumpFreq =',
     &'   /* Model state write out interval ( s ). */')

      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     & '// Gridding paramters ( PARM04 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_L( usingCartesianGrid, INDEX_NONE,
     & 'usingCartesianGrid =',
     &'   /* Cartesian coordinates flag ( True / False ) */')
      CALL WRITE_0D_L( usingSphericalPolarGrid, INDEX_NONE,
     & 'usingSphericalPolarGrid =',
     &'   /* Spherical coordinates flag ( True / False ) */')
      CALL WRITE_1D_R8( delZ,Nr, INDEX_K,'delZ = ',
     &'   /* W spacing ( m ) */')
      CALL WRITE_1D_R8( delP,Nr, INDEX_K,'delP = ',
     &'   /* W spacing ( Pa ) */')
      CALL WRITE_1D_R8( delR,Nr, INDEX_K,'delR = ',
     &'   /* W spacing ( units of r ) */')
      CALL WRITE_1D_R8( delX, Nx, INDEX_I,'delX = ',
     &'   /* U spacing ( m - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( delY, Ny, INDEX_J,'delY = ',
     &'   /* V spacing ( m - cartesian, degrees - spherical ) */')
      CALL WRITE_0D_R8( phiMin, INDEX_NONE,'phiMin = ',
     &'   /* South edge (ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_0D_R8( thetaMin, INDEX_NONE,'thetaMin = ',
     &'   /* West edge ( ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_0D_R8( rSphere, INDEX_NONE,'rSphere = ',
     &'   /* Radius ( ignored - cartesian, m - spherical ) */')
      DO bi=1,nSx
       DO I=1,sNx
        xcoord((bi-1)*sNx+I) = xC(I,1,bi,1)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( xcoord, sNx*nSx, INDEX_I,'xcoord = ',
     &'   /* P-point X coord ( m - cartesian, degrees - spherical ) */')
      DO bj=1,nSy
       DO J=1,sNy
        ycoord((bj-1)*sNy+J) = yC(1,J,1,bj)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( ycoord, sNy*nSy, INDEX_J,'ycoord = ',
     &'   /* P-point Y coord ( m - cartesian, degrees - spherical ) */')
      DO K=1,Nr
       rcoord(K) = rC(K)
      ENDDO
      CALL WRITE_1D_R8( rcoord, Nr, INDEX_K,'rcoord = ',
     &'   /* P-point R coordinate (  units of r ) */')



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

