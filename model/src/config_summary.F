C $Header: /u/gcmpack/MITgcm/model/src/config_summary.F,v 1.20 1999/03/22 15:54:03 adcroft Exp $

#include "CPP_OPTIONS.h"

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
      REAL                     xcoord(Nx)
      REAL                     ycoord(Ny)
      REAL                     rcoord(Nr)


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
      CALL WRITE_1D_R8( viscAh, 1, INDEX_NONE,'viscAh =',
     &'   /* Lateral eddy viscosity ( m^2/s ) */')
      CALL WRITE_1D_R8( viscA4, 1, INDEX_NONE,'viscA4 =',
     &'   /* Lateral biharmonic viscosity ( m^4/s ) */')
      CALL WRITE_1D_L( no_slip_sides,1, INDEX_NONE,
     & 'no_slip_sides =', '  /* Viscous BCs: No-slip sides */')
      IF ( viscAz .NE. UNSET_RL ) THEN
       CALL WRITE_1D_R8( viscAz, 1, INDEX_NONE,'viscAz =',
     & '   /* Vertical eddy viscosity ( m^2/s ) */')
      ENDIF
      IF ( viscAp .NE. UNSET_RL ) THEN
       CALL WRITE_1D_R8( viscAp, 1, INDEX_NONE,'viscAp =',
     & '   /* Vertical eddy viscosity ( Pa^2/s ) */')
      ENDIF
      CALL WRITE_1D_R8( viscAr, 1, INDEX_NONE,'viscAr =',
     &'   /* Vertical eddy viscosity ( units of r^2/s ) */')
      CALL WRITE_1D_R8( diffKhT, 1, INDEX_NONE,'diffKhT =',
     &'   /* Laplacian diffusion of heat laterally ( m^2/s ) */')
      CALL WRITE_1D_R8( diffK4T, 1, INDEX_NONE,'diffK4T =',
     &'   /* Bihaarmonic diffusion of heat laterally ( m^4/s ) */')
      CALL WRITE_1D_R8( diffKzT, 1, INDEX_NONE,'diffKzT =',
     &'   /* Laplacian diffusion of heat vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKrT, 1, INDEX_NONE,'diffKrT =',
     &'   /* Laplacian diffusion of heat vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKhS, 1, INDEX_NONE,'diffKhS =',
     &'   /* Laplacian diffusion of salt laterally ( m^2/s ) */')
      CALL WRITE_1D_R8( diffK4S, 1, INDEX_NONE,'diffK4S =',
     &'   /* Bihaarmonic diffusion of salt laterally ( m^4/s ) */')
      CALL WRITE_1D_R8( diffKzS, 1, INDEX_NONE,'diffKzS =',
     &'   /* Laplacian diffusion of salt vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( diffKrS, 1, INDEX_NONE,'diffKrS =',
     &'   /* Laplacian diffusion of salt vertically ( m^2/s ) */')
      CALL WRITE_1D_R8( tAlpha,1, INDEX_NONE,'tAlpha =',
     &'   /* Linear EOS thermal expansion coefficient ( 1/degree ) */')
      CALL WRITE_1D_R8( sBeta, 1, INDEX_NONE,'sBeta =',
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
      CALL WRITE_1D_R8( rhonil,1, INDEX_NONE,'rhonil =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_1D_R8( rhoConst,1, INDEX_NONE,'rhoConst =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_1D_R8( gravity,1, INDEX_NONE,'gravity =',
     &'   /* Gravitational acceleration ( m/s^2 ) */')
      CALL WRITE_1D_R8( gBaro,1, INDEX_NONE,'gBaro =',
     &'   /* Barotropic gravity ( m/s^2 ) */')
      CALL WRITE_1D_R8( f0,1, INDEX_NONE,'f0 =',
     &'   /* Reference coriolis parameter ( 1/s ) */')
      CALL WRITE_1D_R8( beta,1, INDEX_NONE,'beta =',
     &'   /* Beta ( 1/(m.s) ) */')
      CALL WRITE_1D_R8( freeSurfFac,1, INDEX_NONE,'freeSurfFac =',
     &'   /* Implcit free surface factor */')
      CALL WRITE_1D_L( implicitFreeSurface,1, INDEX_NONE,
     &                 'implicitFreeSurface =',
     &'   /* Implicit free surface on/off flag */')
      CALL WRITE_1D_L( rigidLid,1, INDEX_NONE,
     &                 'rigidLid =',
     &'   /* Rigid lid on/off flag */')
      CALL WRITE_1D_L( momStepping,1, INDEX_NONE,
     & 'momStepping =', '  /* Momentum equation on/off flag */')
      CALL WRITE_1D_L( momAdvection,1, INDEX_NONE,
     & 'momAdvection =', '  /* Momentum advection on/off flag */')
      CALL WRITE_1D_L( momViscosity,1, INDEX_NONE,
     & 'momViscosity =', '  /* Momentum viscosity on/off flag */')
      CALL WRITE_1D_L( useCoriolis,1, INDEX_NONE,
     & 'useCoriolis =', '  /* Coriolis on/off flag */')
      CALL WRITE_1D_L( momForcing,1, INDEX_NONE,
     & 'momForcing =', '  /* Momentum forcing on/off flag */')
      CALL WRITE_1D_L( momPressureForcing,1, INDEX_NONE,
     & 'momPressureForcing =',  
     & '  /* Momentum pressure term on/off flag */')
      CALL WRITE_1D_L( tempStepping,1, INDEX_NONE,
     & 'tempStepping =', '  /* Temperature equation on/off flag */')
      CALL WRITE_1D_L( openBoundaries,1, INDEX_NONE,
     & 'openBoundaries =', '  /* OpenBoundaries on/off flag */')
      CALL WRITE_1D_L( nonHydrostatic,1, INDEX_NONE,
     & 'nonHydrostatic =', '  /* Non-Hydrostatic on/off flag */')
      CALL WRITE_1D_R8( GMMaxSlope,1, INDEX_NONE,'GMMaxSlope =',
     &'   /* Max. slope allowed in GM/Redi tensor */')
      CALL WRITE_1D_R8( GMLength,1, INDEX_NONE,'GMLength =',
     &'   /* Length to use in Visbeck et al. formula for K (m) */')
      CALL WRITE_1D_R8( GMAlpha,1, INDEX_NONE,'GMAlpha =',
     &'   /* alpha to use in Visbeck et al. formula for K */')
      CALL WRITE_1D_R8( GMdepth,1, INDEX_NONE,'GMdepth =',
     &'   /* Depth to integrate for Visbeck et. al Richardson # (m) */')
      CALL WRITE_1D_R8( GMkbackground,1, INDEX_NONE,'GMkbackground =',
     &'   /* background value of GM/Redi coefficient m^2/s */')
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
      CALL WRITE_1D_I( cg2dMaxIters,1, INDEX_NONE,'cg2dMaxIters =',
     &'   /* Upper limit on 2d con. grad iterations  */')
      CALL WRITE_1D_I( cg2dChkResFreq,1, INDEX_NONE,'cg2dChkResFreq =',
     &'   /* 2d con. grad convergence test frequency */')
      CALL WRITE_1D_R8( cg2dTargetResidual,1, INDEX_NONE,
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
      CALL WRITE_1D_I( nIter0,1, INDEX_NONE,'nIter0 =',
     &'   /* Base timestep number  */')
      CALL WRITE_1D_I( nTimeSteps,1, INDEX_NONE,'nTimeSteps =',
     &'   /* Number of timesteps */')
      CALL WRITE_1D_R8( deltaTmom,1, INDEX_NONE,'deltaTmom =',
     &'   /* Momentum equation timestep ( s ) */')
      CALL WRITE_1D_R8( deltaTtracer,1, INDEX_NONE,'deltaTtracer =',
     &'   /* Tracer equation timestep ( s ) */')
      CALL WRITE_1D_R8( deltaTClock ,1, INDEX_NONE,'deltaTClock  =',
     &'   /* Model clock timestep ( s ) */')
      CALL WRITE_1D_R8( cAdjFreq,1, INDEX_NONE,'cAdjFreq =',
     &'   /* Convective adjustment interval ( s ) */')
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
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     & '// Gridding paramters ( PARM04 in namelist ) '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//  '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_1D_L( usingCartesianGrid,1, INDEX_NONE,
     & 'usingCartesianGrid =',
     &'   /* Cartesian coordinates flag ( True / False ) */')
      CALL WRITE_1D_L( usingSphericalPolarGrid,1, INDEX_NONE,
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
      CALL WRITE_1D_R8( phiMin, 1, INDEX_NONE,'phiMin = ',
     &'   /* South edge (ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( thetaMin, 1, INDEX_NONE,'thetaMin = ',
     &'   /* West edge ( ignored - cartesian, degrees - spherical ) */')
      CALL WRITE_1D_R8( rSphere, 1, INDEX_NONE,'rSphere = ',
     &'   /* Radius ( ignored - cartesian, m - spherical ) */')
      DO bi=1,nSx
       DO I=1,sNx
        xcoord((bi-1)*sNx+I) = xc(I,1,bi,1)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( xcoord, sNx*nSx, INDEX_I,'xcoord = ',
     &'   /* P-point X coord ( m - cartesian, degrees - spherical ) */')
      DO bj=1,nSy
       DO J=1,sNy
        ycoord((bj-1)*sNy+J) = yc(1,J,1,bj)
       ENDDO
      ENDDO
      CALL WRITE_1D_R8( ycoord, sNy*nSy, INDEX_J,'ycoord = ',
     &'   /* P-point Y coord ( m - cartesian, degrees - spherical ) */')
      DO K=1,Nr
       rcoord(K) = rc(K)
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

