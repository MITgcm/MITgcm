C $Header: /u/gcmpack/MITgcm/model/src/config_summary.F,v 1.31 2002/02/10 00:35:30 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CONFIG_SUMMARY
C     !INTERFACE:
      SUBROUTINE CONFIG_SUMMARY( myThid )
C     !DESCRIPTION: \bv
C     *=========================================================*
C     | SUBROUTINE CONFIG_SUMMARY                                 
C     | o Summarize model parameter settings.                     
C     *=========================================================* 
C     | This routine writes a tabulated summary of the kernel     
C     | model configuration. Information describes all the 
C     | parameter setting in force and the meaning and units of
C     | those parameters. Individal packages report a similar
C     | table for each package using the same format as employed
C     | here. If parameters are missing or incorrectly described
C     | or dimensioned please contact support@mitgcm.org
C     *=========================================================* 
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of CONFIG_SUMMARY
      INTEGER myThid
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Temp. for building output string.
C     I,J,K  :: Loop counters.
C     bi,bj  :: Tile loop counters.
C     xcoord :: Temps. for building lists of values for uni-dimensionally
C     ycoord :: varying parameters.
C     zcoord ::
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER                  I,J,K
      INTEGER                  bi, bj
      _RL                     xcoord(Nx)
      _RL                     ycoord(Ny)
      _RL                     rcoord(Nr)
      INTEGER coordLine
      INTEGER tileLine
CEOP


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
     &'   /* Implicit free surface factor */')
      CALL WRITE_0D_L( implicitFreeSurface, INDEX_NONE,
     &                 'implicitFreeSurface =',
     &'   /* Implicit free surface on/off flag */')
      CALL WRITE_0D_L( rigidLid, INDEX_NONE,
     &                 'rigidLid =',
     &'   /* Rigid lid on/off flag */')
      CALL WRITE_0D_R8( implicSurfPress, INDEX_NONE,
     &'implicSurfPress =',
     &'   /* Surface Pressure implicit factor (0-1)*/')
      CALL WRITE_0D_R8( implicDiv2Dflow, INDEX_NONE,
     &'implicDiv2Dflow =',
     &'   /* Barot. Flow Div. implicit factor (0-1)*/')
      CALL WRITE_0D_L( exactConserv, INDEX_NONE,
     &'exactConserv =',
     &'   /* Exact Volume Conservation on/off flag*/')
      CALL WRITE_0D_L( uniformLin_PhiSurf, INDEX_NONE,
     &'uniformLin_PhiSurf =',
     &'   /* use uniform Bo_surf on/off flag*/')
      CALL WRITE_0D_I( nonlinFreeSurf, INDEX_NONE,
     &'nonlinFreeSurf =',
     &'   /* Non-linear Free Surf. options (-1,0,1,2,3)*/')
      WRITE(msgBuf,'(2A)') '     -1,0= Off ; 1,2,3= On,',
     &  ' 2=+rescale gU,gV, 3=+update cg2d solv.'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_R8( hFacInf, INDEX_NONE,
     &'hFacInf =',
     &'   /* lower threshold for hFac (nonlinFreeSurf only)*/')
      CALL WRITE_0D_R8( hFacSup, INDEX_NONE,
     &'hFacSup =',
     &'   /* upper threshold for hFac (nonlinFreeSurf only)*/')
      CALL WRITE_0D_L( useRealFreshWaterFlux, INDEX_NONE,
     &'useRealFreshWaterFlux =',
     &'   /* Real Fresh Water Flux on/off flag*/')
      IF (useRealFreshWaterFlux .AND. nonlinFreeSurf.GT.0) THEN
      CALL WRITE_0D_R8( temp_EvPrRn, INDEX_NONE,
     &'temp_EvPrRn =',
     &' /* Temp. of Evap/Prec/R (UNSET=use local T)(oC)*/')
      CALL WRITE_0D_R8( salt_EvPrRn, INDEX_NONE,
     &'salt_EvPrRn =',
     &' /* Salin. of Evap/Prec/R (UNSET=use local S)(ppt)*/')
      CALL WRITE_0D_R8( trac_EvPrRn, INDEX_NONE,
     &'trac_EvPrRn =',
     &' /* Tracer in Evap/Prec/R (UNSET=use local Tr)*/')
      ELSE
      CALL WRITE_0D_R8( convertFW2Salt, INDEX_NONE,
     &'convertFW2Salt =',
     &' /* convert F.W. Flux to Salt Flux (-1=use local S)(ppt)*/')
      ENDIF

      CALL WRITE_0D_L( staggerTimeStep, INDEX_NONE,
     &                 'staggerTimeStep =',
     &'   /* Stagger time stepping on/off flag */')
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
      CALL WRITE_0D_L( groundAtK1, INDEX_NONE, 'groundAtK1 =',
     &'   /* Lower Boundary (ground) at the surface(k=1) ( T / F ) */')
      CALL WRITE_1D_R8( Ro_SeaLevel,1, INDEX_NONE,'Ro_SeaLevel =',
     &'   /* r(1) ( units of r ) */')
      CALL WRITE_1D_R8( rkFac,1, INDEX_NONE,'rkFac =',
     &'   /* minus Vertical index orientation  */')
      CALL WRITE_1D_R8( horiVertRatio,1, INDEX_NONE,'horiVertRatio =',
     &'   /* Ratio on units : Horiz - Vertical */')
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

C     Grid along selected grid lines
      coordLine = 1
      tileLine  = 1
      CALL WRITE_XY_XLINE_RS( dxF, coordLine, tileLine,
     I 'dxF','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dxF, coordLine, tileLine, 
     I 'dxF','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dyF, coordLine, tileLine,
     I 'dyF','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dyF, coordLine, tileLine, 
     I 'dyF','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dxG, coordLine, tileLine,
     I 'dxG','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dxG, coordLine, tileLine, 
     I 'dxG','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dyG, coordLine, tileLine,
     I 'dyG','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dyG, coordLine, tileLine, 
     I 'dyG','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dxC, coordLine, tileLine,
     I 'dxC','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dxC, coordLine, tileLine, 
     I 'dxC','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dyC, coordLine, tileLine,
     I 'dyC','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dyC, coordLine, tileLine, 
     I 'dyC','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dxV, coordLine, tileLine,
     I 'dxV','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dxV, coordLine, tileLine, 
     I 'dxV','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( dyU, coordLine, tileLine,
     I 'dyU','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( dyU, coordLine, tileLine, 
     I 'dyU','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( rA, coordLine, tileLine,
     I 'rA','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( rA, coordLine, tileLine, 
     I 'rA','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( rAw, coordLine, tileLine,
     I 'rAw','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( rAw, coordLine, tileLine, 
     I 'rAw','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_XLINE_RS( rAs, coordLine, tileLine,
     I 'rAs','( m - cartesian, degrees - spherical )')
      CALL WRITE_XY_YLINE_RS( rAs, coordLine, tileLine, 
     I 'rAs','( m - cartesian, degrees - spherical )')

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

