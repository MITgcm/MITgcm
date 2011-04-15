C $Header: /u/gcmpack/MITgcm/model/inc/FFIELDS.h,v 1.41 2011/04/15 20:15:56 jmc Exp $
C $Name:  $
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
C     | available via pkg/exf and pkg/seaice
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
C  saltFlux :: Net upward salt flux in psu.kg/m^2/s
C              flux of Salt taken out of the ocean per time unit (second).
C              Note: a) only used when salty sea-ice forms or melts.
C                    b) units: when salinity (unit= psu) is expressed
C                       in g/kg, saltFlux unit becomes g/m^2/s.
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
C     dQdT  :: Thermal relaxation coefficient in W/m^2/degrees
C              Southwest C-grid tracer point
C
C     SST   :: Sea surface temperature in degrees C for relaxation
C              Southwest C-grid tracer point
C
C     SSS   :: Sea surface salinity in psu for relaxation
C              Southwest C-grid tracer point
C
C     lambdaThetaClimRelax :: Inverse time scale for relaxation ( 1/s ).
C
C     lambdaSaltClimRelax :: Inverse time scale for relaxation ( 1/s ).

C     pLoad :: for the ocean:      atmospheric pressure at z=eta
C                Units are           Pa=N/m^2
C              for the atmosphere: geopotential of the orography
C                Units are           meters (converted)
C  sIceLoad :: sea-ice loading, expressed in Mass of ice+snow / area unit
C                Units are           kg/m^2
C              Note: only used with Sea-Ice & RealFreshWater formulation
C     eddyPsiX -Zonal Eddy Streamfunction in m^2/s used in taueddy_external_forcing.F
C     eddyPsiY -Meridional Streamfunction in m^2/s used in taueddy_external_forcing.F
C     EfluxY - y-component of Eliassen-Palm flux vector
C     EfluxP - p-component of Eliassen-Palm flux vector

      COMMON /FFIELDS_fu/ fu
      COMMON /FFIELDS_fv/ fv
      COMMON /FFIELDS_Qnet/ Qnet
      COMMON /FFIELDS_Qsw/ Qsw
      COMMON /FFIELDS_dQdT/ dQdT
      COMMON /FFIELDS_EmPmR/ EmPmR
      COMMON /FFIELDS_saltFlux/ saltFlux
      COMMON /FFIELDS_SST/ SST
      COMMON /FFIELDS_SSS/ SSS
      COMMON /FFIELDS_lambdaThetaClimRelax/ lambdaThetaClimRelax
      COMMON /FFIELDS_lambdaSaltClimRelax/ lambdaSaltClimRelax
      COMMON /FFIELDS_pLoad/ pLoad
      COMMON /FFIELDS_sIceLoad/ sIceLoad

      _RS  fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  dQdT     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  saltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lambdaThetaClimRelax(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  lambdaSaltClimRelax(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pLoad    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  sIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_EP_FLUX
      COMMON /efluxFFIELDS/ EfluxY,EfluxP
      _RL  EfluxY (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  EfluxP (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_EDDYPSI
      COMMON /eddypsiFFIELDS/ eddyPsiX,eddyPsiY
      _RS  eddyPsiX (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  eddyPsiY (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

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
#ifdef ATMOSPHERIC_LOADING
      _RS  pLoad0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  pLoad1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef SHORTWAVE_HEATING
      _RS  Qsw1     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Qsw0     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif /* EXCLUDE_FFIELDS_LOAD */

C     surfaceForcingU     units are  r_unit.m/s^2 (=m^2/s^2 if r=z)
C                -> usage in gU:     gU = gU + surfaceForcingU/drF [m/s^2]
C     surfaceForcingV     units are  r_unit.m/s^2 (=m^2/s^-2 if r=z)
C                -> usage in gU:     gV = gV + surfaceForcingV/drF [m/s^2]
C
C     surfaceForcingS     units are  r_unit.psu/s (=psu.m/s if r=z)
C            - EmPmR * S_surf plus salinity relaxation*drF(1)
C                -> usage in gS:     gS = gS + surfaceForcingS/drF [psu/s]
C
C     surfaceForcingT     units are  r_unit.Kelvin/s (=Kelvin.m/s if r=z)
C            - Qnet (+Qsw) plus temp. relaxation*drF(1)
C                -> calculate        -lambda*(T(model)-T(clim))
C            Qnet assumed to be net heat flux including ShortWave rad.
C                -> usage in gT:     gT = gT + surfaceforcingT/drF [K/s]
C     surfaceForcingTice
C            - equivalent Temperature flux in the top level that corresponds
C              to the melting or freezing of sea-ice.
C              Note that the surface level temperature is modified
C              directly by the sea-ice model in order to maintain
C              water temperature under sea-ice at the freezing
C              point.  But we need to keep track of the
C              equivalent amount of heat that this surface-level
C              temperature change implies because it is used by
C              the KPP package (kpp_calc.F and kpp_transport_t.F).
C              Units are r_unit.K/s (=Kelvin.m/s if r=z) (>0 for ocean warming).

      COMMON /SURFACE_FORCING/
     &                         surfaceForcingU,
     &                         surfaceForcingV,
     &                         surfaceForcingT,
     &                         surfaceForcingS,
     &                         surfaceForcingTice
      _RL  surfaceForcingU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  surfaceForcingTice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
