!BOP
! !ROUTINE: FFIELDS.h
! !INTERFACE:
! include "FFIELDS.h"
! !DESCRIPTION:
! \bv
! *==========================================================*
! | FFIELDS.h
! | o Model forcing fields
! *==========================================================*
! | More flexible surface forcing configurations are
! | available via, e.g., pkg/exf
! *==========================================================*
! \ev
!EOP
!
! fu    :: Zonal surface wind stress in N/m^2
!          > 0 for increase in uVel, which is west to
!              east for cartesian and spherical polar grids
!          Typical range: -0.5 < fu < 0.5
!          Southwest C-grid U point
!
! fv    :: Meridional surface wind stress in N/m^2
!          > 0 for increase in vVel, which is south to
!              north for cartesian and spherical polar grids
!          Typical range: -0.5 < fv < 0.5
!          Southwest C-grid V point
!
! EmPmR :: Net upward freshwater flux in kg/m2/s
!          EmPmR = Evaporation - precipitation - runoff
!          > 0 for increase in salt (ocean salinity)
!          Typical range: -1e-4 < EmPmR < 1e-4
!          Southwest C-grid tracer point
!       NOTE: for backward compatibility EmPmRfile is specified in
!             m/s when using external_fields_load.F.  It is converted
!             to kg/m2/s by multiplying by rhoConstFresh.
!
!  saltFlux :: Net upward salt flux in g/kg.kg/m^2/s = g/m^2/s
!          flux of Salt taken out of the ocean per time unit (second).
!          Note: only used when salty sea-ice forms or melts.
!          > 0 for decrease in SSS.
!          Southwest C-grid tracer point
!
! Qnet  :: Net upward surface heat flux (including shortwave) in W/m^2
!          Qnet = latent + sensible + net longwave + net shortwave
!          > 0 for decrease in theta (ocean cooling)
!          Typical range: -250 < Qnet < 600
!          Southwest C-grid tracer point
!
! Qsw   :: Net upward shortwave radiation in W/m^2
!          Qsw = - ( downward - ice and snow absorption - reflected )
!          > 0 for decrease in theta (ocean cooling)
!          Typical range: -350 < Qsw < 0
!          Southwest C-grid tracer point
!
! SST   :: Sea surface temperature in degrees C for relaxation
!          Southwest C-grid tracer point
!
! SSS   :: Sea surface salinity in g/kg for relaxation
!          Southwest C-grid tracer point
!
! lambdaThetaClimRelax :: Inverse time scale for SST relaxation ( 1/s ).
!
! lambdaSaltClimRelax  :: Inverse time scale for SSS relaxation ( 1/s ).

! phiTide2d :: vertically uniform (2d-map), time-dependent geopotential
!              anomaly (e.g., tidal forcing); Units are m^2/s^2
! pLoad :: for the ocean:      atmospheric pressure anomaly (relative to
!                               "surf_pRef") at z=eta
!            Units are           Pa=N/m^2
!          for the atmosphere (hack): geopotential anomaly of the orography
!            Units are           m^2/s^2
! sIceLoad :: sea-ice loading, expressed in Mass of ice+snow / area unit
!            Units are           kg/m^2
!          Note: only used with Sea-Ice & RealFreshWater formulation
! addMass  :: source (<0: sink) of fluid in the domain interior
!             (generalisation of oceanic real fresh-water flux)
!            Units are           kg/s  (mass per unit of time)
! frictionHeating :: heating caused by friction and momentum dissipation
!            Units are           in W/m^2 (thickness integrated)
! eddyPsiX -Zonal Eddy Streamfunction in m^2/s used in taueddy_external_forcing.F
! eddyPsiY -Meridional Streamfunction in m^2/s used in taueddy_external_forcing.F
! EfluxY - y-component of Eliassen-Palm flux vector
! EfluxP - p-component of Eliassen-Palm flux vector

      COMMON /FFIELDS_fu/ fu
      COMMON /FFIELDS_fv/ fv
      COMMON /FFIELDS_Qnet/ Qnet
      COMMON /FFIELDS_Qsw/ Qsw
      COMMON /FFIELDS_EmPmR/ EmPmR
      COMMON /FFIELDS_saltFlux/ saltFlux
      COMMON /FFIELDS_SST/ SST
      COMMON /FFIELDS_SSS/ SSS
      COMMON /FFIELDS_lambdaThetaClimRelax/ lambdaThetaClimRelax
      COMMON /FFIELDS_lambdaSaltClimRelax/ lambdaSaltClimRelax
      COMMON /FFIELDS_phiTide/ phiTide2d
      COMMON /FFIELDS_pLoad/ pLoad
      COMMON /FFIELDS_sIceLoad/ sIceLoad

      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  saltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lambdaThetaClimRelax(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lambdaSaltClimRelax(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  phiTide2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pLoad    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  sIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

! gcmSST :: model in-situ Sea Surface Temperature (SST); corresponds to
!           surface-level model variable "theta", except if using TEOS-10 ;
!           in that case a conversion from model Conservative Temperature
!           "theta" is applied. Note: not defined under an ice-shelf
      COMMON /FFIELDS_INSITU_TEMP/ gcmSST
      _RL  gcmSST(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_ADDFLUID
      COMMON /FFIELDS_ADD_FLUID/ addMass
      _RL addMass(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_FRICTION_HEATING
      COMMON /FFIELDS_frictionHeat/ frictionHeating
      _RS frictionHeating(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
!  geothermalFlux :: Upward geothermal flux through bottom cell [W/m^2]
               ! > 0 for increase in theta (ocean warming)
               ! Typical range: 0 < geothermalFlux < 1.5 W/m^2
               ! (global mean on the order 0.09 - 0.1 W/m^2)
      COMMON /FFIELDS_geothermal/ geothermalFlux
      _RS geothermalFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_BALANCE_FLUXES
!  weight2BalanceFlx :: weight used for applying weighted correction
                  ! to global-mean surf. flux imbalance ; no-units
      COMMON /FFIELDS_W2BALANCE/ weight2BalanceFlx
      _RS weight2BalanceFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef SHORTWAVE_HEATING
! SWFrac3D :: fraction of solar short-wave flux penetrating the vertical
!             cell interfaces (no units), function of depth of cell
!             interface, potentially turbidity, cholorphyll concentration,
!             or other biogeochemical material;
!             the vertical dimension is Nr+1, because this makes it easier
!             to maintain the symmetry w.r.t. z vs. p-coordinates.
      COMMON /FFIELDS_SWFRAC/ SWFrac3D
      _RS  SWFrac3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
#endif

#ifdef ALLOW_EDDYPSI
! uEulerMean  :: The Eulerian mean Zonal  velocity (residual less bolus velocity)
! vEulerMean  :: The Eulerian mean Merid. velocity (residual less bolus velocity)
! tauxEddy    :: The eddy stress used in the momentum equation of a residual model
! tauyEddy    :: The eddy stress used in the momentum equation of a residual model

      COMMON /FFIELDS_eddyPsi_RS/ eddyPsiX, eddyPsiY
      _RS  eddyPsiX (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  eddyPsiY (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      COMMON /FFIELDS_eddyPsi_RL/                                                 &
     &      tauxEddy, tauyEddy, uEulerMean, vEulerMean
      _RL tauxEddy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tauyEddy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uEulerMean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vEulerMean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_EDDYPSI */

#ifndef EXCLUDE_FFIELDS_LOAD
! loadedRec     :: time-record currently loaded (in temp arrays *[1])
! taux[0,1]     :: Temp. for zonal wind stress
! tauy[0,1]     :: Temp. for merid. wind stress
! Qnet[0,1]     :: Temp. for heat flux
! EmPmR[0,1]    :: Temp. for fresh water flux
! saltFlux[0,1] :: Temp. for isurface salt flux
! SST[0,1]      :: Temp. for theta climatalogy
! SSS[0,1]      :: Temp. for theta climatalogy
! Qsw[0,1]      :: Temp. for short wave component of heat flux
! pLoad[0,1]    :: Temp. for atmospheric pressure at z=eta
! [0,1]         :: End points for interpolation

      COMMON /FFIELDS_I/ loadedRec
      INTEGER :: loadedRec(nSx,nSy)

      COMMON /TDFIELDS/                                                           &
     &      taux0, tauy0, Qnet0, EmPmR0, SST0, SSS0,                              &
     &      taux1, tauy1, Qnet1, EmPmR1, SST1, SSS1,                              &
#ifdef SHORTWAVE_HEATING                                                          
     &      Qsw0, Qsw1,                                                           &
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX        
     &      geothFlux0, geothFlux1,                                               &
#endif
#ifdef ATMOSPHERIC_LOADING  
     &      pLoad0, pLoad1,                                                       &
#endif
     &      saltFlux0, saltFlux1                                                  

      _RS  taux0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  saltFlux0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  taux1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  tauy1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  saltFlux1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SHORTWAVE_HEATING
      _RS  Qsw0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
      _RS  geothFlux0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  geothFlux1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ATMOSPHERIC_LOADING
      _RS  pLoad0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pLoad1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif /* EXCLUDE_FFIELDS_LOAD */

! surfaceForcingU     units are  r_unit.m/s^2 (=m^2/s^2 if r=z)
!            -> usage in gU:     gU = gU + surfaceForcingU/drF [m/s^2]
! surfaceForcingV     units are  r_unit.m/s^2 (=m^2/s^-2 if r=z)
!            -> usage in gU:     gV = gV + surfaceForcingV/drF [m/s^2]
!
! surfaceForcingS     units are  r_unit.g/kg/s (=g/kg.m/s if r=z)
!        - EmPmR * S_surf plus salinity relaxation*drF(1)
!            -> usage in gS:     gS = gS + surfaceForcingS/drF [g/kg/s]
!
! surfaceForcingT     units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
!        - Qnet (+Qsw) plus temp. relaxation*drF(1)
!            -> calculate        -lambda*(T(model)-T(clim))
!        Qnet assumed to be net heat flux including ShortWave rad.
!            -> usage in gT:     gT = gT + surfaceforcingT/drF [K/s]
! adjustColdSST_diag :: diagnostic field for how much too cold (below
!          Tfreezing) SST has been adjusted (with allowFreezing=T).
!          > 0 for increase of SST (up to Tfreezing).
!          Units are r_unit.K/s (=Kelvin.m/s if r=z).
!    Note: 1) allowFreezing option is a crude hack to fix too cold SST that
!          results from missing seaice component. It should never be used
!          with any seaice component, neither current seaice pkg (pkg/seaice
!          or pkg/thsice) nor a seaice component from atmos model when
!          coupled to it.
!          2) this diagnostic is currently used by KPP package (kpp_calc.F
!          and kpp_transport_t.F) although it is not very clear it should.

      COMMON /SURFACE_FORCING/                                                    &
     &      surfaceForcingU,                                                      &
     &      surfaceForcingV,                                                      &
     &      surfaceForcingT,                                                      &
     &      surfaceForcingS,                                                      &
     &      adjustColdSST_diag
      _RL  surfaceForcingU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  adjustColdSST_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

! botDragU :: bottom stress (for diagnostics), Zonal component
!            Units are N/m^2 ;   > 0 increase uVel @ bottom
! botDragV :: bottom stress (for diagnostics), Merid. component
!            Units are N/m^2 ;   > 0 increase vVel @ bottom
      COMMON /FFIELDS_bottomStress/ botDragU, botDragV
      _RS  botDragU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  botDragV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
