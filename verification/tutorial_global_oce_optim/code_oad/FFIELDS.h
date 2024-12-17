CBOP
C     !ROUTINE: FFIELDS.h
C     !INTERFACE:
C     include "FFIELDS.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | FFIELDS.h
C     | o Model forcing fields
C     *==========================================================*
C     | More flexible surface forcing configurations are
C     | available via, e.g., pkg/exf
C     *==========================================================*
C     \ev
CEOP
C
C     fu    :: Zonal surface wind stress in N/m^2
C              > 0 for increase in uVel, which is west to
C                  east for cartesian and spherical polar grids
C              Typical range: -0.5 < fu < 0.5
C              Southwest C-grid U point
C
C     fv    :: Meridional surface wind stress in N/m^2
C              > 0 for increase in vVel, which is south to
C                  north for cartesian and spherical polar grids
C              Typical range: -0.5 < fv < 0.5
C              Southwest C-grid V point
C
C     EmPmR :: Net upward freshwater flux in kg/m2/s
C              EmPmR = Evaporation - precipitation - runoff
C              > 0 for increase in salt (ocean salinity)
C              Typical range: -1e-4 < EmPmR < 1e-4
C              Southwest C-grid tracer point
C           NOTE: for backward compatibility EmPmRfile is specified in
C                 m/s when using external_fields_load.F.  It is converted
C                 to kg/m2/s by multiplying by rhoConstFresh.
C
C  saltFlux :: Net upward salt flux in g/kg.kg/m^2/s = g/m^2/s
C              flux of Salt taken out of the ocean per time unit (second).
C              Note: only used when salty sea-ice forms or melts.
C              > 0 for decrease in SSS.
C              Southwest C-grid tracer point
C
C     Qnet  :: Net upward surface heat flux (including shortwave) in W/m^2
C              Qnet = latent + sensible + net longwave + net shortwave
C              > 0 for decrease in theta (ocean cooling)
C              Typical range: -250 < Qnet < 600
C              Southwest C-grid tracer point
C
C     Qsw   :: Net upward shortwave radiation in W/m^2
C              Qsw = - ( downward - ice and snow absorption - reflected )
C              > 0 for decrease in theta (ocean cooling)
C              Typical range: -350 < Qsw < 0
C              Southwest C-grid tracer point
C
C     SST   :: Sea surface temperature in degrees C for relaxation
C              Southwest C-grid tracer point
C
C     SSS   :: Sea surface salinity in g/kg for relaxation
C              Southwest C-grid tracer point
C
C     lambdaThetaClimRelax :: Inverse time scale for SST relaxation ( 1/s ).
C
C     lambdaSaltClimRelax  :: Inverse time scale for SSS relaxation ( 1/s ).

C     phiTide2d :: vertically uniform (2d-map), time-dependent geopotential
C                  anomaly (e.g., tidal forcing); Units are m^2/s^2
C     pLoad :: for the ocean:      atmospheric pressure anomaly (relative to
C                                   "surf_pRef") at z=eta
C                Units are           Pa=N/m^2
C              for the atmosphere (hack): geopotential anomaly of the orography
C                Units are           m^2/s^2
C     sIceLoad :: sea-ice loading, expressed in Mass of ice+snow / area unit
C                Units are           kg/m^2
C              Note: only used with Sea-Ice & RealFreshWater formulation
C     addMass  :: source (<0: sink) of fluid in the domain interior
C                 (generalisation of oceanic real fresh-water flux)
C                Units are           kg/s  (mass per unit of time)
C     frictionHeating :: heating caused by friction and momentum dissipation
C                Units are           in W/m^2 (thickness integrated)
C     eddyPsiX -Zonal Eddy Streamfunction in m^2/s used in taueddy_external_forcing.F
C     eddyPsiY -Meridional Streamfunction in m^2/s used in taueddy_external_forcing.F
C     EfluxY - y-component of Eliassen-Palm flux vector
C     EfluxP - p-component of Eliassen-Palm flux vector

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

C     gcmSST :: model in-situ Sea Surface Temperature (SST); corresponds to
C               surface-level model variable "theta", except if using TEOS-10 ;
C               in that case a conversion from model Conservative Temperature
C               "theta" is applied. Note: not defined under an ice-shelf
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
C  geothermalFlux :: Upward geothermal flux through bottom cell [W/m^2]
C                    > 0 for increase in theta (ocean warming)
C                    Typical range: 0 < geothermalFlux < 1.5 W/m^2
C                    (global mean on the order 0.09 - 0.1 W/m^2)
      COMMON /FFIELDS_geothermal/ geothermalFlux
      _RS geothermalFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_BALANCE_FLUXES
C  weight2BalanceFlx :: weight used for applying weighted correction
C                       to global-mean surf. flux imbalance ; no-units
      COMMON /FFIELDS_W2BALANCE/ weight2BalanceFlx
      _RS weight2BalanceFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C- jmc: commented out until corresponding (ghost-like) code apparition
C     dQdT  :: Thermal relaxation coefficient in W/m^2/degrees
C              Southwest C-grid tracer point
c     COMMON /FFIELDS_dQdT/ dQdT
c     _RS  dQdT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c#ifdef ALLOW_EP_FLUX
c     COMMON /FFIELDS_eflux/ EfluxY,EfluxP
c     _RL  EfluxY (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c     _RL  EfluxP (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
c#endif

#ifdef ALLOW_EDDYPSI
C     uEulerMean  :: The Eulerian mean Zonal  velocity (residual less bolus velocity)
C     vEulerMean  :: The Eulerian mean Merid. velocity (residual less bolus velocity)
C     tauxEddy    :: The eddy stress used in the momentum equation of a residual model
C     tauyEddy    :: The eddy stress used in the momentum equation of a residual model

      COMMON /FFIELDS_eddyPsi_RS/ eddyPsiX, eddyPsiY
      _RS  eddyPsiX (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  eddyPsiY (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      COMMON /FFIELDS_eddyPsi_RL/
     &                tauxEddy, tauyEddy, uEulerMean, vEulerMean
      _RL tauxEddy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tauyEddy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL uEulerMean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vEulerMean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_EDDYPSI */

#ifndef EXCLUDE_FFIELDS_LOAD
C     loadedRec     :: time-record currently loaded (in temp arrays *[1])
C     taux[0,1]     :: Temp. for zonal wind stress
C     tauy[0,1]     :: Temp. for merid. wind stress
C     Qnet[0,1]     :: Temp. for heat flux
C     EmPmR[0,1]    :: Temp. for fresh water flux
C     saltFlux[0,1] :: Temp. for isurface salt flux
C     SST[0,1]      :: Temp. for theta climatalogy
C     SSS[0,1]      :: Temp. for theta climatalogy
C     Qsw[0,1]      :: Temp. for short wave component of heat flux
C     pLoad[0,1]    :: Temp. for atmospheric pressure at z=eta
C     [0,1]         :: End points for interpolation

      COMMON /FFIELDS_I/ loadedRec
      INTEGER loadedRec(nSx,nSy)

      COMMON /TDFIELDS/
     &                 taux0, tauy0, Qnet0, EmPmR0, SST0, SSS0,
     &                 taux1, tauy1, Qnet1, EmPmR1, SST1, SSS1,
     &                 saltFlux0, saltFlux1
#ifdef SHORTWAVE_HEATING
     &               , Qsw0, Qsw1
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
     &               , geothFlux0, geothFlux1
#endif
#ifdef ATMOSPHERIC_LOADING
     &               , pLoad0, pLoad1
#endif

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

C     surfaceForcingU     units are  r_unit.m/s^2 (=m^2/s^2 if r=z)
C                -> usage in gU:     gU = gU + surfaceForcingU/drF [m/s^2]
C     surfaceForcingV     units are  r_unit.m/s^2 (=m^2/s^-2 if r=z)
C                -> usage in gU:     gV = gV + surfaceForcingV/drF [m/s^2]
C
C     surfaceForcingS     units are  r_unit.g/kg/s (=g/kg.m/s if r=z)
C            - EmPmR * S_surf plus salinity relaxation*drF(1)
C                -> usage in gS:     gS = gS + surfaceForcingS/drF [g/kg/s]
C
C     surfaceForcingT     units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
C            - Qnet (+Qsw) plus temp. relaxation*drF(1)
C                -> calculate        -lambda*(T(model)-T(clim))
C            Qnet assumed to be net heat flux including ShortWave rad.
C                -> usage in gT:     gT = gT + surfaceforcingT/drF [K/s]
C     adjustColdSST_diag :: diagnostic field for how much too cold (below
C              Tfreezing) SST has been adjusted (with allowFreezing=T).
C              > 0 for increase of SST (up to Tfreezing).
C              Units are r_unit.K/s (=Kelvin.m/s if r=z).
C        Note: 1) allowFreezing option is a crude hack to fix too cold SST that
C              results from missing seaice component. It should never be used
C              with any seaice component, neither current seaice pkg (pkg/seaice
C              or pkg/thsice) nor a seaice component from atmos model when
C              coupled to it.
C              2) this diagnostic is currently used by KPP package (kpp_calc.F
C              and kpp_transport_t.F) although it is not very clear it should.

      COMMON /SURFACE_FORCING/
     &                         surfaceForcingU,
     &                         surfaceForcingV,
     &                         surfaceForcingT,
     &                         surfaceForcingS,
     &                         adjustColdSST_diag
      _RL  surfaceForcingU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  adjustColdSST_diag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     botDragU :: bottom stress (for diagnostics), Zonal component
C                Units are N/m^2 ;   > 0 increase uVel @ bottom
C     botDragV :: bottom stress (for diagnostics), Merid. component
C                Units are N/m^2 ;   > 0 increase vVel @ bottom
      COMMON /FFIELDS_bottomStress/ botDragU, botDragV
      _RS  botDragU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  botDragV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Extra mean heat flux field specific to this experiment
      COMMON /Mean_qnet/ Qnetm
      _RS  Qnetm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c  #include "../code_ad/cost_local.h"
C-    Content of this experiment specific header file above is added here:
C     Define some local weights specific to this experiment
C     whfluxm     :: weight for heat flux
C     wtheta      :: dummy weight for temperature
      COMMON /COST_LOCAL_WEIGHTS/ whfluxm, wtheta
      _RL whfluxm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wtheta ( Nr,nSx,nSy )
