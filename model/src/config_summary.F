C $Header: /u/gcmpack/MITgcm/model/src/config_summary.F,v 1.76 2005/06/26 16:32:56 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CONFIG_SUMMARY

C     !INTERFACE:
      SUBROUTINE CONFIG_SUMMARY( myThid )

C     !DESCRIPTION:
C     This routine summarizes the model parameter settings by writing a
C     tabulated list of the kernel model configuration variables.  It
C     describes all the parameter settings in force and the meaning and
C     units of those parameters. Individal packages report a similar
C     table for each package using the same format as employed here. If
C     parameters are missing or incorrectly described or dimensioned
C     please contact <MITgcm-support@mitgcm.org>

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  Number of this instance of CONFIG_SUMMARY
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
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
      _RL                     rcoord(Nr+1)
      INTEGER coordLine
      INTEGER tileLine


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
      WRITE(msgBuf,'(A,A40)') 'buoyancyRelation = ', buoyancyRelation
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      CALL WRITE_0D_L( fluidIsAir,   INDEX_NONE,
     & 'fluidIsAir  =', '  /* fluid major constituent is Air */')
      CALL WRITE_0D_L( fluidIsWater, INDEX_NONE,
     & 'fluidIsWater=', '  /* fuild major constituent is Water */')
      CALL WRITE_0D_L( usingPCoords, INDEX_NONE,
     & 'usingPCoords =', '  /* use p (or p*) vertical coordinate */')
      CALL WRITE_0D_L( usingZCoords, INDEX_NONE,
     & 'usingZCoords =', '  /* use z (or z*) vertical coordinate */')
      CALL WRITE_1D_R8( tRef, Nr, INDEX_K,'tRef =',
     &'   /* Reference temperature profile ( oC or oK ) */')
      CALL WRITE_1D_R8( sRef, Nr, INDEX_K,'sRef =',
     &'   /* Reference salinity profile ( ppt ) */')
      CALL WRITE_0D_R8( viscAh, INDEX_NONE,'viscAh =',
     &'   /* Lateral eddy viscosity ( m^2/s ) */')
      IF ( viscAhD.NE.viscAh ) 
     & CALL WRITE_0D_R8( viscAhD, INDEX_NONE,'viscAhD =',
     & '  /* Lateral eddy viscosity (Divergence)( m^2/s ) */')
      IF ( viscAhZ.NE.viscAh ) 
     & CALL WRITE_0D_R8( viscAhZ, INDEX_NONE,'viscAhZ =',
     & '  /* Lateral eddy viscosity (Vorticity) ( m^2/s ) */')
      CALL WRITE_0D_R8( viscAhMax, INDEX_NONE,'viscAhMax =',
     &'   /* Maximum lateral eddy viscosity ( m^2/s ) */')
      CALL WRITE_0D_R8( viscAhGrid, INDEX_NONE,'viscAhGrid =',
     &'   /* Grid dependent lateral eddy viscosity ( non-dim. ) */')
      CALL WRITE_0D_L( useFullLeith, INDEX_NONE,
     &'useFullLeith =',
     &'   /* Use Full Form of Leith Viscosity on/off flag*/')
      CALL WRITE_0D_R8( viscC2leith, INDEX_NONE,'viscC2leith =',
     &' /* Leith harmonic viscosity factor (on grad(vort),non-dim.) */')
      CALL WRITE_0D_R8( viscC2leithD, INDEX_NONE,'viscC2leithD =',
     &' /* Leith harmonic viscosity factor (on grad(div),non-dim.) */')
      CALL WRITE_0D_R8( viscC2smag, INDEX_NONE,'viscC2smag =',
     &'   /* Smagorinsky harmonic viscosity factor (non-dim.) */')
      CALL WRITE_0D_R8( viscA4, INDEX_NONE,'viscA4 =',
     &'   /* Lateral biharmonic viscosity ( m^4/s ) */')
      IF ( viscA4D.NE.viscA4 ) 
     & CALL WRITE_0D_R8( viscA4D, INDEX_NONE,'viscA4D =',
     & '  /* Lateral biharmonic viscosity (Divergence)( m^4/s ) */')
      IF ( viscA4Z.NE.viscA4 ) 
     & CALL WRITE_0D_R8( viscA4Z, INDEX_NONE,'viscA4Z =',
     & '  /* Lateral biharmonic viscosity (Vorticity) ( m^4/s ) */')
      CALL WRITE_0D_R8( viscA4Max, INDEX_NONE,'viscA4Max =',
     &'   /* Maximum biharmonic viscosity ( m^2/s ) */')
      CALL WRITE_0D_R8( viscA4Grid, INDEX_NONE,'viscA4Grid =',
     &'   /* Grid dependent biharmonic viscosity ( non-dim. ) */')
      CALL WRITE_0D_R8( viscC4leith, INDEX_NONE,'viscC4leith =',
     &' /* Leith biharm viscosity factor (on grad(vort), non-dim.) */')
      CALL WRITE_0D_R8( viscC4leithD, INDEX_NONE,'viscC4leithD =',
     &' /* Leith biharm viscosity factor (on grad(div), non-dim.) */')
      CALL WRITE_0D_L( no_slip_sides, INDEX_NONE,
     & 'no_slip_sides =', '  /* Viscous BCs: No-slip sides */')
      CALL WRITE_0D_R8( viscAr,  INDEX_NONE,'viscAr =',
     &'   /* Vertical eddy viscosity ( units of r^2/s ) */')
      CALL WRITE_0D_L( no_slip_bottom, INDEX_NONE,
     & 'no_slip_bottom =', '  /* Viscous BCs: No-slip bottom */')
      CALL WRITE_0D_R8( diffKhT, INDEX_NONE,'diffKhT =',
     &'   /* Laplacian diffusion of heat laterally ( m^2/s ) */')
      CALL WRITE_0D_R8( diffK4T, INDEX_NONE,'diffK4T =',
     &'   /* Bihaarmonic diffusion of heat laterally ( m^4/s ) */')
      CALL WRITE_0D_R8( diffKhS, INDEX_NONE,'diffKhS =',
     &'   /* Laplacian diffusion of salt laterally ( m^2/s ) */')
      CALL WRITE_0D_R8( diffK4S, INDEX_NONE,'diffK4S =',
     &'   /* Bihaarmonic diffusion of salt laterally ( m^4/s ) */')
      CALL WRITE_1D_R8( diffKrNrT, Nr, INDEX_K,'diffKrNrT =',
     & ' /* vertical profile of vertical diffusion of Temp ( m^2/s )*/')
      CALL WRITE_1D_R8( diffKrNrS, Nr, INDEX_K,'diffKrNrS =',
     & ' /* vertical profile of vertical diffusion of Salt ( m^2/s )*/')
      CALL WRITE_0D_R8( diffKrBL79surf, INDEX_NONE,'diffKrBL79surf =',
     &'   /* Surface diffusion for Bryan and Lewis 1979 ( m^2/s ) */')
      CALL WRITE_0D_R8( diffKrBL79deep, INDEX_NONE,'diffKrBL79deep =',
     &'   /* Deep diffusion for Bryan and Lewis 1979 ( m^2/s ) */')
      CALL WRITE_0D_R8( diffKrBL79scl, INDEX_NONE,'diffKrBL79scl =',
     &'   /* Depth scale for Bryan and Lewis 1979 ( m ) */')
      CALL WRITE_0D_R8( diffKrBL79Ho, INDEX_NONE,'diffKrBL79Ho =',
     &'   /* Turning depth for Bryan and Lewis 1979 ( m ) */')
      WRITE(msgBuf,'(2A)') ' Equation of State : eosType = ', eosType
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
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
      IF ( fluidIsAir ) THEN
       CALL WRITE_0D_R8( atm_Rd, INDEX_NONE, 'atm_Rd =',
     & '  /* gas constant for dry air ( J/kg/K ) */')
      CALL WRITE_0D_R8( atm_Cp, INDEX_NONE, 'atm_Cp =',
     & '  /* specific heat (Cp) of dry air ( J/kg/K ) */')
      CALL WRITE_0D_R8( atm_kappa, INDEX_NONE, 'atm_kappa =',
     & '  /* kappa (=Rd/Cp ) of dry air */')
       CALL WRITE_0D_R8( atm_Rq, INDEX_NONE, 'atm_Rq =',
     &  ' /* water vap. specific vol. anomaly relative to dry air */')
      CALL WRITE_0D_R8( atm_Po, INDEX_NONE, 'atm_Po =',
     & '  /* standard reference pressure ( Pa ) */')
      CALL WRITE_0D_I( integr_GeoPot, INDEX_NONE, 'integr_GeoPot =',
     & '  /* select how the geopotential is integrated */')
      CALL WRITE_0D_I( selectFindRoSurf, INDEX_NONE, 
     & 'selectFindRoSurf=',
     & '  /* select how Surf.Ref. pressure is defined */')
      ENDIF
      CALL WRITE_0D_R8( rhonil,  INDEX_NONE,'rhonil =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_0D_R8( rhoConst, INDEX_NONE,'rhoConst =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_0D_R8( rhoConstFresh, INDEX_NONE,'rhoConstFresh =',
     &'   /* Reference density ( kg/m^3 ) */')
      CALL WRITE_0D_R8( gravity, INDEX_NONE,'gravity =',
     &'   /* Gravitational acceleration ( m/s^2 ) */')
      CALL WRITE_0D_R8( gBaro,   INDEX_NONE,'gBaro =',
     &'   /* Barotropic gravity ( m/s^2 ) */')
      CALL WRITE_0D_R8(rotationPeriod,INDEX_NONE,'rotationPeriod =',
     &'   /* Rotation Period ( s ) */')
      CALL WRITE_0D_R8( omega,   INDEX_NONE,'omega =',
     &'   /* Angular velocity ( rad/s ) */')
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
      CALL WRITE_0D_I( select_rStar, INDEX_NONE,
     &'select_rStar =',
     &'   /* r* Coordinate options (not yet implemented)*/')
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
      ELSE
      CALL WRITE_0D_R8( convertFW2Salt, INDEX_NONE,
     &'convertFW2Salt =',
     &' /* convert F.W. Flux to Salt Flux (-1=use local S)(ppt)*/')
      ENDIF

      CALL WRITE_0D_L( nonHydrostatic, INDEX_NONE,
     & 'nonHydrostatic =', '  /* Non-Hydrostatic on/off flag */')
      CALL WRITE_0D_L( momStepping,  INDEX_NONE,
     & 'momStepping =', '  /* Momentum equation on/off flag */')
      CALL WRITE_0D_L( momAdvection, INDEX_NONE,
     & 'momAdvection =', '  /* Momentum advection on/off flag */')
      CALL WRITE_0D_L( momViscosity, INDEX_NONE,
     & 'momViscosity =', '  /* Momentum viscosity on/off flag */')
      CALL WRITE_0D_L( momImplVertAdv, INDEX_NONE, 'momImplVertAdv =',
     &                '/* Momentum implicit vert. advection on/off*/')
      CALL WRITE_0D_L( implicitViscosity, INDEX_NONE,
     & 'implicitViscosity =', ' /* Implicit viscosity on/off flag */')
      CALL WRITE_0D_L( useCoriolis,  INDEX_NONE,
     & 'useCoriolis =', '  /* Coriolis on/off flag */')
      CALL WRITE_0D_L( useCDscheme,  INDEX_NONE,
     & 'useCDscheme =', '  /* CD scheme on/off flag */')
      CALL WRITE_0D_L( useJamartWetPoints,  INDEX_NONE,
     & 'useJamartWetPoints=',' /* Coriolis WetPoints method flag */')
      CALL WRITE_0D_L( useJamartMomAdv,  INDEX_NONE,
     & 'useJamartMomAdv=',' /* V.I. Non-linear terms Jamart flag */')
      CALL WRITE_0D_L( SadournyCoriolis,  INDEX_NONE,
     & 'SadournyCoriolis=',' /* Sadourny Coriolis discr. flag */')
      CALL WRITE_0D_L( upwindVorticity,  INDEX_NONE,
     & 'upwindVorticity=',' /* Upwind bias vorticity flag */')
      CALL WRITE_0D_L( useAbsVorticity,  INDEX_NONE,
     & 'useAbsVorticity=',' /* Work with f+zeta in Coriolis */')
      CALL WRITE_0D_L( highOrderVorticity,  INDEX_NONE,
     & 'highOrderVorticity=',' /* High order interp. of vort. flag */')
      CALL WRITE_0D_L( upwindShear,  INDEX_NONE,
     & 'upwindShear=', ' /* Upwind vertical Shear advection flag */')
      CALL WRITE_0D_L( momForcing,   INDEX_NONE,
     & 'momForcing =', '  /* Momentum forcing on/off flag */')
      CALL WRITE_0D_L( momPressureForcing, INDEX_NONE,
     & 'momPressureForcing =',  
     & '  /* Momentum pressure term on/off flag */')
      CALL WRITE_0D_L( staggerTimeStep, INDEX_NONE,
     &                 'staggerTimeStep =',
     &'   /* Stagger time stepping on/off flag */')
      CALL WRITE_0D_L( multiDimAdvection, INDEX_NONE, 
     & 'multiDimAdvection =',
     &'   /* enable/disable Multi-Dim Advection */')
      CALL WRITE_0D_L( useMultiDimAdvec, INDEX_NONE, 
     & 'useMultiDimAdvec =',
     &'   /* Multi-Dim Advection is/is-not used */')
      CALL WRITE_0D_L( implicitDiffusion, INDEX_NONE,
     & 'implicitDiffusion =','/* Implicit Diffusion on/off flag */')
      CALL WRITE_0D_L( tempStepping,  INDEX_NONE,
     & 'tempStepping =', '  /* Temperature equation on/off flag */')
      CALL WRITE_0D_L( tempAdvection,  INDEX_NONE,
     & 'tempAdvection=', '  /* Temperature advection on/off flag */')
      CALL WRITE_0D_L( tempImplVertAdv,INDEX_NONE,'tempImplVertAdv =',
     &                '/* Temp. implicit vert. advection on/off */')
      CALL WRITE_0D_L( tempForcing,  INDEX_NONE,
     & 'tempForcing  =', '  /* Temperature forcing on/off flag */')
      CALL WRITE_0D_L( saltStepping,  INDEX_NONE,
     & 'saltStepping =', '  /* Salinity equation on/off flag */')
      CALL WRITE_0D_L( saltAdvection,  INDEX_NONE,
     & 'saltAdvection=', '  /* Salinity advection on/off flag */')
      CALL WRITE_0D_L( saltImplVertAdv,INDEX_NONE,'saltImplVertAdv =',
     &                '/* Sali. implicit vert. advection on/off */')
      CALL WRITE_0D_L( saltForcing,  INDEX_NONE,
     & 'saltForcing  =', '  /* Salinity forcing on/off flag */')
      CALL WRITE_0D_L( debugMode,  INDEX_NONE,
     & ' debugMode  =', '  /* Debug Mode on/off flag */')
      CALL WRITE_0D_I( debLevA, INDEX_NONE,
     & '   debLevA  =', '  /* 1rst level of debugging */')
      CALL WRITE_0D_I( debLevB, INDEX_NONE,
     & '   debLevB  =', '  /* 2nd  level of debugging */')
      CALL WRITE_0D_I( debugLevel, INDEX_NONE,
     & ' debugLevel =', '  /* select debugging level */')
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
      CALL WRITE_0D_R8( cg2dTargetResWunit, INDEX_NONE,
     & 'cg2dTargetResWunit =',
     &'   /* CG2d target residual [W units] */')
      CALL WRITE_0D_I( cg2dPreCondFreq, INDEX_NONE,'cg2dPreCondFreq =',
     &'   /* Freq. for updating cg2d preconditioner */')

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
     &'   /* Run starting timestep number  */')
      CALL WRITE_0D_I( nTimeSteps, INDEX_NONE,'nTimeSteps =',
     &'   /* Number of timesteps */')
      CALL WRITE_0D_R8( deltaTmom, INDEX_NONE,'deltatTmom =',
     &'   /* Momentum equation timestep ( s ) */')
      CALL WRITE_0D_R8( deltaTfreesurf,INDEX_NONE,'deltaTfreesurf =',
     &'   /* FreeSurface equation timestep ( s ) */')
      CALL WRITE_1D_R8( dTtracerLev, Nr, INDEX_K, 'dTtracerLev =',
     &'   /* Tracer equation timestep ( s ) */')
      CALL WRITE_0D_R8( deltaTClock, INDEX_NONE,'deltatTClock  =',
     &'   /* Model clock timestep ( s ) */')
      CALL WRITE_0D_R8( cAdjFreq, INDEX_NONE,'cAdjFreq =',
     &'   /* Convective adjustment interval ( s ) */')
      CALL WRITE_0D_L( forcing_In_AB,INDEX_NONE,'forcing_In_AB =',
     &'   /* put T,S Forcing in Adams-Bash. stepping */')
      CALL WRITE_0D_R8( abEps, INDEX_NONE,'abEps =',
     &'   /* Adams-Bashforth-2 stabilizing weight */')
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL WRITE_0D_R8( alph_AB, INDEX_NONE,'alph_AB =',
     &'   /* Adams-Bashforth-3 primary factor */')
      CALL WRITE_0D_R8( beta_AB, INDEX_NONE,'beta_AB =',
     &'   /* Adams-Bashforth-3 secondary factor */')
      CALL WRITE_0D_L( startFromPickupAB2, INDEX_NONE,
     & 'startFromPickupAB2=',' /* start from AB-2 pickup */')
#endif
      IF (useCDscheme) THEN
      CALL WRITE_0D_R8( tauCD, INDEX_NONE,'tauCD =',
     &'   /* CD coupling time-scale ( s ) */')
      CALL WRITE_0D_R8( rCD, INDEX_NONE,'rCD =',
     &'   /* Normalised CD coupling parameter */')
      ENDIF
      CALL WRITE_0D_R8( baseTime, INDEX_NONE,'baseTime =',
     &'   /* Model base time ( s ). */')
      CALL WRITE_0D_R8( startTime, INDEX_NONE,'startTime =',
     &'   /* Run start time ( s ). */')
      CALL WRITE_0D_R8( endTime, INDEX_NONE,'endTime =',
     &'   /* Integration ending time ( s ). */')
      CALL WRITE_0D_R8( pChkPtFreq, INDEX_NONE,'pChkPtFreq =',
     &'   /* Permanent restart/checkpoint file interval ( s ). */')
      CALL WRITE_0D_R8( chkPtFreq, INDEX_NONE,'chkPtFreq =',
     &'   /* Rolling restart/checkpoint file interval ( s ). */')
      CALL WRITE_0D_L(pickup_write_mdsio,INDEX_NONE,
     &     'pickup_write_mdsio =', '   /* Model IO flag. */')
      CALL WRITE_0D_L(pickup_read_mdsio,INDEX_NONE,
     &     'pickup_read_mdsio =', '   /* Model IO flag. */')
#ifdef ALLOW_MNC
      CALL WRITE_0D_L(pickup_write_mnc,INDEX_NONE,
     &     'pickup_write_mnc =', '   /* Model IO flag. */')
      CALL WRITE_0D_L(pickup_read_mnc,INDEX_NONE,
     &     'pickup_read_mnc =', '   /* Model IO flag. */')
#endif
      CALL WRITE_0D_L(pickup_write_immed,INDEX_NONE,
     &     'pickup_write_immed =','   /* Model IO flag. */')
      CALL WRITE_0D_R8( dumpFreq, INDEX_NONE,'dumpFreq =',
     &'   /* Model state write out interval ( s ). */')
      CALL WRITE_0D_L(snapshot_mdsio,INDEX_NONE,
     &     'snapshot_mdsio =', '   /* Model IO flag. */')
#ifdef ALLOW_MNC
      CALL WRITE_0D_L(snapshot_mnc,INDEX_NONE,
     &     'snapshot_mnc =', '   /* Model IO flag. */')
#endif
      CALL WRITE_0D_R8( monitorFreq, INDEX_NONE,'monitorFreq =',
     &'   /* Monitor output interval ( s ). */')
      CALL WRITE_0D_L(monitor_stdio,INDEX_NONE,
     &     'monitor_stdio =', '   /* Model IO flag. */')
#ifdef ALLOW_MNC
      CALL WRITE_0D_L(monitor_mnc,INDEX_NONE,
     &     'monitor_mnc =', '   /* Model IO flag. */')
#endif
      CALL WRITE_0D_R8( externForcingPeriod, INDEX_NONE,
     &   'externForcingPeriod =', '   /* forcing period (s) */')
      CALL WRITE_0D_R8( externForcingCycle, INDEX_NONE,
     &   'externForcingCycle =', '   /* period of the cyle (s). */')
      CALL WRITE_0D_R8( tauThetaClimRelax, INDEX_NONE,
     &   'tauThetaClimRelax =', '   /* relaxation time scale (s) */')
      CALL WRITE_0D_R8( tauSaltClimRelax, INDEX_NONE,
     &   'tauSaltClimRelax =',  '   /* relaxation time scale (s) */')
      CALL WRITE_0D_R8( latBandClimRelax, INDEX_NONE,
     &   'latBandClimRelax =', '   /* max. Lat. where relaxation */')
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
      CALL WRITE_0D_L( usingCylindricalGrid, INDEX_NONE,
     & 'usingCylindricalGrid =',
     &'   /* Spherical coordinates flag ( True / False ) */')
      CALL WRITE_0D_R8( Ro_SeaLevel, INDEX_NONE,'Ro_SeaLevel =',
     &'   /* r(1) ( units of r ) */')
      CALL WRITE_0D_R8( rkSign, INDEX_NONE,'rkSign =',
     &'   /* index orientation relative to vertical coordinate */')
      CALL WRITE_0D_R8( horiVertRatio, INDEX_NONE,'horiVertRatio =',
     &'   /* Ratio on units : Horiz - Vertical */')
c     CALL WRITE_1D_R8( delZ,Nr, INDEX_K,'delZ = ',
c    &'   /* W spacing ( m ) */')
c     CALL WRITE_1D_R8( delP,Nr, INDEX_K,'delP = ',
c    &'   /* W spacing ( Pa ) */')
c     CALL WRITE_1D_R8( delR,Nr, INDEX_K,'delR = ',
c    &'   /* W spacing ( units of r ) */')
      CALL WRITE_1D_R8( drC,Nr, INDEX_K,'drC = ',
     &'   /* C spacing ( units of r ) */')
      CALL WRITE_1D_R8( drF,Nr, INDEX_K,'drF = ',
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
      DO K=1,Nr+1
       rcoord(K) = rF(K)
      ENDDO
      CALL WRITE_1D_R8( rcoord, Nr+1, INDEX_K,'rF = ',
     &'   /* W-Interf. R coordinate (  units of r ) */')

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

