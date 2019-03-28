C     $Header: /home/ubuntu/mnt/e9_copy/MITgcm_contrib/ifenty/Fenty_Thermo_Code_Updates/code_updates/seaice_growth_if.F,v 1.4 2010/09/25 16:41:06 ifenty Exp $
C     $Name:  $

#include "SEAICE_OPTIONS.h"

C     StartOfInterface
      SUBROUTINE SEAICE_GROWTH_IF( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE seaice_growth_if                              |
C     | o Updata ice thickness and snow depth                    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
C     EndOfInterface(global-font-lock-mode 1)

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface

C     TBC   : The freezing point of seawater (deg C)
C     TMELT : The freezing point of fresh water (deg K)
      _RL TBC, TMELT

#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft, hFlood
#endif /* ALLOW_SEAICE_FLOODING */

C     QSWO   - short wave heat flux over ocean (W/m^2)
C     QSWI   - short wave heat flux under ice  (W/m^2)
      _RL QSWO                         (1:sNx,1:sNy)
      _RL QSWI                         (1:sNx,1:sNy)

C     The shortwave heat flux in the open water fraction of the 
C     grid cell converging within the uppermost ocean grid cell (W/m^2)
      _RL QSWO_IN_FIRST_LAYER          (1:sNx,1:sNy)


C     (1 - QSWO_IN_FIRST_LAYER) (W/m^2)
      _RL QSWO_BELOW_FIRST_LAYER        (1:sNx,1:sNy)

#ifdef SEAICE_DEBUG
C     The total shortwave heat flux (into open water and through the ice)
C     converging within the uppermost ocean grid cell (W/m^2)
      _RL QSW_absorb_in_first_layer     (1:sNx,1:sNy)

C     ( 1 - QSW_absorb_in_first_layer)  (W/m^2) 
      _RL QSW_absorb_below_first_layer  (1:sNx,1:sNy)
#endif

C     The actual ice thickness (i.e., mean ice thickness / ice concentration) (m) 
      _RL HICE_ACTUAL                   (1:sNx,1:sNy)

C     The actual snow thickness (i.e., mean snow thickness / ice concentration) (m) 
      _RL HSNOW_ACTUAL                  (1:sNx,1:sNy)

C     wind speed (m/s)
      _RL UG                            (1:sNx,1:sNy)

c     RHOFW  : Freshwater density (kg m^-3)
C     CPW    : Seawater heat capacity   (J m^-3 K^-1)
c     LI     : Ice latent heat of fusion (J kg^-1)
c     QI, QS : Factors used to to map between energy fluxes 
c     RHOSW  : A reference seawater density (kg m^-3)
c     and snow/ice melt or growth
      _RL RHOFW,CPW,LI,QI,QS,RHOSW

C     Helper Variables for snow flooding
      _RL EnergyToMeltSnowAndIce        (1:sNx,1:sNy)
      _RL EnergyToMeltSnowAndIce2       (1:sNx,1:sNy)
      _RL FL_C1,FL_C2,FL_C3,FL_C4,deltaHS,deltaHI


#ifdef SEAICE_USE_ORIGINAL_HEAT_FLUX
      _RL GAMMA
#endif

#ifndef SEAICE_USE_ORIGINAL_HEAT_FLUX
C     Factor by which we increase the upper ocean friction velocity (u*) when 
C     ice is absent in a grid cell  (dimensionless)
      _RL MixedLayerTurbulenceFactor

c     The Stanton number for the McPhee 
c     ocean-ice heat flux parameterization (dimensionless)
      _RL STANTON_NUMBER

c     A typical friction velocity beneath sea ice for the 
c     McPhee heat flux parameterization (m/s)
      _RL USTAR_BASE

#endif /* SEAICE_USE_ORIGINAL_HEAT_FLUX */

C     Sea ice thickness growth rates (m/s)
      _RL NetExistingIceGrowthRate      (1:sNx,1:sNy)
      _RL IceGrowthRateUnderExistingIce (1:sNx,1:sNy)
      _RL IceGrowthRateFromSurface      (1:sNx,1:sNy)
      _RL IceGrowthRateOpenWater        (1:sNx,1:sNy)
      _RL IceGrowthRateMixedLayer       (1:sNx,1:sNy)

#ifdef ALLOW_SALT_PLUME
c     d(HEFF)/dt from heat fluxes in the open water fraction of the grid cell
      _RL IceGrowthRateInLeads          (1:sNx,1:sNy)

c     The fraction of salt released in leads by new ice production there
c     which is to be sent to the salt plume package
      _RL leadPlumeFraction             (1:sNx,1:sNy)
#endif

c     The minimum value of HEFF to be divided by during the calculation of d(AREA)/dt
      _RL HEFF_MIN
      
#ifdef SEAICE_DEBUG
c     Variables for debugging (predicted temperature change from 
c     various sources)
      _RL PredTempChange                (1:sNx,1:sNy)
      _RL PredTempChangeFromQSW         (1:sNx,1:sNy)
      _RL PredTempChangeFromOA_MQNET    (1:sNx,1:sNy)
      _RL PredTempChangeFromFIA         (1:sNx,1:sNy)
      _RL PredTempChangeFromNewIceVol   (1:sNx,1:sNy)
      _RL PredTempChangeFromF_IA_NET    (1:sNx,1:sNy)
      _RL PredTempChangeFromF_IO_NET    (1:sNx,1:sNy)

      _RL ExpectedIceVolumeChange       (1:sNx,1:sNy)
      _RL ExpectedSnowVolumeChange      (1:sNx,1:sNy)
#endif

      _RL ActualIceVolumeChange         (1:sNx,1:sNy)
      _RL ActualNewTotalSnowMelt        (1:sNx,1:sNy)

      _RL EnergyInNewIceVolume          (1:sNx,1:sNy)
      _RL NetEnergyOutOfOcean           (1:sNx,1:sNy)
      _RL NetEnergyOutOfIce             (1:sNx,1:sNy)

C     The energy taken out of the ocean which is not converted
C     to sea ice (Joules)
      _RL ResidualEnergyOutOfOcean      (1:sNx,1:sNy)

C     Snow accumulation rate over ice (m/s)
      _RL SnowAccRateOverIce            (1:sNx,1:sNy)

C     Total snow accumulation over ice (m)
      _RL SnowAccOverIce                (1:sNx,1:sNy)

C     The precipitation rate over the ice which goes immediately into the ocean
      _RL PrecipRateOverIceSurfaceToSea (1:sNx,1:sNy)

C     The potential snow melt rate if all snow surface heat flux convergences
C     goes to melting snow (m/s)
      _RL PotSnowMeltRateFromSurf       (1:sNx,1:sNy)

C     The potential thickness of snow which could be melted by snow surface
C     heat flux convergence (m)
      _RL PotSnowMeltFromSurf           (1:sNx,1:sNy)

C     The actual snow melt rate due to snow surface  heat flux convergence
      _RL SnowMeltRateFromSurface       (1:sNx,1:sNy)

C     The actual surface heat flux convergence used to melt snow (W/m^2)
      _RL SurfHeatFluxConvergToSnowMelt (1:sNx,1:sNy)

C     The actual thickness of snow to be melted by snow surface
C     heat flux convergence (m)
      _RL SnowMeltFromSurface           (1:sNx,1:sNy)

C     The freshwater contribution to the ocean from melting snow (m)
      _RL FreshwaterContribFromSnowMelt (1:sNx,1:sNy)

C     The freshwater contribution to (from) the ocean from melting (growing) ice (m)
      _RL FreshwaterContribFromIce      (1:sNx,1:sNy)

C     S_a : d(AREA)/dt
      _RL S_a                           (1:sNx,1:sNy)

C     S_a_from_IGROW : d(AREA)/dt [from ice growth rate from open water fluxes]
      _RL S_a_from_IGROW                (1:sNx,1:sNy)

C     S_h : d(HEFF)/dt
      _RL S_h                           (1:sNx,1:sNy)

C     S_hsnow : d(HSNOW)/dt
      _RL S_hsnow                       (1:sNx,1:sNy)

C     HSNOW_ORIG : The mean snow thickness before any accumulation, 
C     melt, or flooding (m)
      _RL HSNOW_ORIG                    (1:sNx,1:sNy)

C     F_ia  - sea ice/snow surface heat flux with atmosphere (W/m^2)

C     F_ia > 0 implies heat loss to atmosphere

C     F_ia < 0 implies atmospheric heat flux convergence (including
c     ice/snow surface melt)
      _RL F_ia                          (1:sNx,1:sNy)

C     F_ia_net - the net heat flux divergence at the sea ice/snow 
c     surface including sea ice conductive fluxes and atmospheric 
c     fluxes (W/m^2)

C     F_ia_net = 0 implies that at the sea ice/snow surface an energy 
c     balance condition is met (upward conductive fluxes balance 
c     surface heat loss to the atmosphere).

C     F_ia_net < 0 implies a net heat flux convergence at the ice/snow
c     surface, zero conductive fluxes, and net atmospheric convergence.
      _RL F_ia_net                      (1:sNx,1:sNy)

C     F_ia_net_before_snow - the net heat flux divergence at the 
c     ice/snow surface before snow melts (W/m^2)

C     F_ia_net_before_snow < 0 implies some snow, if present, will melt.
      _RL F_ia_net_before_snow          (1:sNx,1:sNy)

C     F_io_net - the net upward conductive heat flux through the 
c     ice + snow realized at the sea ice/snow surface.

C     F_io_net > 0 implies heat conducting upward from ice base,
c     and basal thickening of the ice.

c     F_io_net = 0 implies no upward heat conduction (i.e., the 
C     ice/snow surface temperature > SEAICE_freeze).  
      _RL F_io_net                      (1:sNx,1:sNy)

C     F_ao  - heat flux from atmosphere to ocean (W/m^2)
C     F_ao > 0
      _RL F_ao                          (1:sNx,1:sNy)

C     F_mi - heat flux from ocean to the ice (W/m^2)
      _RL F_mi                          (1:sNx,1:sNy)
      
c     The ocean temperature used for the calculation of turbulent 
c     ocean-ice heat fluxes (Celsius)
      _RL surf_theta

C     Helper variables for debugging
      _RL FLUX_TO_DELTA_TEMP,ENERGY_TO_DELTA_TEMP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
         kSurface        = Nr 
      else
         kSurface        = 1
      endif

c     For debugging
      FLUX_TO_DELTA_TEMP = SEAICE_deltaTtherm*
     &   recip_Cp*recip_rhoConst * recip_drF(1)

      ENERGY_TO_DELTA_TEMP = recip_Cp*recip_rhoConst*recip_drF(1)

      TBC   = SEAICE_freeze
      TMELT = 273.15 _d 0 
      RHOSW = 1026. _d 0
      RHOFW = rhoConstFresh

#ifndef SEAICE_USE_ORIGINAL_HEAT_FLUX
      STANTON_NUMBER = 0.0056 _d 0 
      USTAR_BASE = 0.0125 _d 0
#endif

      CPW = 4010. _d 0
      Li = 3.34 _d 5 

      QI = ONE/SEAICE_rhoIce/Li
      QS = ONE/SEAICE_rhoSnow/Li

c     Helper terms for snow flooding
      FL_C2 = SEAICE_rhoIce/RHOSW
      FL_C3 = (RHOSW-SEAICE_rhoIce)/SEAICE_rhoSnow
      FL_C4 = SEAICE_rhoSnow/SEAICE_rhoIce

#ifdef SEAICE_USE_ORIGINAL_HEAT_FLUX
c     GAMMA is used to calculate ocean-ice heat fluxes from
c     a user-specified flux timescale (SEAICE_gamma_t).  
c     Originally, 3 days in seconds was suggested as useful.
c     
      GAMMA =  10. _d 0 / SEAICE_gamma_t

c     (Note: this is changed from drF(1)/SEAICE_gamma_t for
c     model setups with higher vertical resolutions.)
c     
c     GAMMA and SEAICE_gamma_t are not used with the default
c     ocean-ice heat flux scheme of McPhee.
#endif

C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
c     
#ifdef ALLOW_AUTODIFF_TAMC
            act1 = bi - myBxLo(myThid)
            max1 = myBxHi(myThid) - myBxLo(myThid) + 1
            act2 = bj - myByLo(myThid)
            max2 = myByHi(myThid) - myByLo(myThid) + 1
            act3 = myThid - 1
            max3 = nTx*nTy
            act4 = ikey_dynamics - 1
            iicekey = (act1 + 1) + act2*max1
     &         + act3*max1*max2
     &         + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                       key = iicekey, byte = isbyte
C     ADJ STORE qnet(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                       key = iicekey, byte = isbyte
C     ADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Initialize variables
            DO J=1,sNy
               DO I=1,sNx
                  F_ia_net                        (I,J) = 0. _d 0
                  F_ia_net_before_snow            (I,J) = 0. _d 0
                  F_io_net                        (I,J) = 0. _d 0

                  F_ia                            (I,J) = 0. _d 0
                  F_ao                            (I,J) = 0. _d 0
                  F_mi                            (I,J) = 0. _d 0

                  QSWO                            (I,J) = 0. _d 0
                  QSWI                            (I,J) = 0. _d 0

                  QSWO_BELOW_FIRST_LAYER          (I,J) = 0. _d 0
                  QSWO_IN_FIRST_LAYER             (I,J) = 0. _d 0

                  S_a                             (I,J) = 0. _d 0
                  S_h                             (I,J) = 0. _d 0

                  IceGrowthRateUnderExistingIce   (I,J) = 0. _d 0
                  IceGrowthRateFromSurface        (I,J) = 0. _d 0
                  NetExistingIceGrowthRate        (I,J) = 0. _d 0
                  S_a_from_IGROW                  (I,J) = 0. _d 0

                  IceGrowthRateOpenWater          (I,J) = 0. _d 0
                  IceGrowthRateMixedLayer         (I,J) = 0. _d 0

#ifdef ALLOW_SALT_PLUME
                  IceGrowthRateInLeads            (I,J) = 0. _d 0
                  leadPlumeFraction               (I,J) = 0. _d 0
                  saltPlumeFlux             (I,J,bi,bj) = 0. _d 0
#endif

                  ActualIceVolumeChange           (I,J) = 0. _d 0
                  ActualNewTotalSnowMelt          (I,J) = 0. _d 0

                  EnergyInNewIceVolume            (I,J) = 0. _d 0
                  NetEnergyOutOfOcean             (I,J) = 0. _d 0
                  NetEnergyOutOfIce               (I,J) = 0. _d 0
                  ResidualEnergyOutOfOcean        (I,J) = 0. _d 0

#ifdef SEAICE_DEBUG
                  ExpectedIceVolumeChange         (I,J) = 0. _d 0
                  ExpectedSnowVolumeChange        (I,J) = 0. _d 0

                  PredTempChange                  (I,J) = 0. _d 0
                  PredTempChangeFromQSW           (I,J) = 0. _d 0
                  PredTempChangeFromOA_MQNET      (I,J) = 0. _d 0
                  PredTempChangeFromFIA           (I,J) = 0. _d 0
                  PredTempChangeFromF_IA_NET      (I,J) = 0. _d 0
                  PredTempChangeFromF_IO_NET      (I,J) = 0. _d 0
                  PredTempChangeFromNewIceVol     (I,J) = 0. _d 0

                  QSW_absorb_in_first_layer       (I,J) = 0. _d 0
                  QSW_absorb_below_first_layer    (I,J) = 0. _d 0
#endif

                  SnowAccRateOverIce              (I,J) = 0. _d 0
                  SnowAccOverIce                  (I,J) = 0. _d 0
                  PrecipRateOverIceSurfaceToSea   (I,J) = 0. _d 0

                  PotSnowMeltRateFromSurf         (I,J) = 0. _d 0
                  PotSnowMeltFromSurf             (I,J) = 0. _d 0
                  SnowMeltFromSurface             (I,J) = 0. _d 0
                  SnowMeltRateFromSurface         (I,J) = 0. _d 0
                  SurfHeatFluxConvergToSnowMelt   (I,J) = 0. _d 0

                  FreshwaterContribFromSnowMelt   (I,J) = 0. _d 0
                  FreshwaterContribFromIce        (I,J) = 0. _d 0

c     Following sea ice dynamics, the ice state (HEFF and 
c     AREA) are stored in time level 1 (N).  The ice state 
c     is moved to time level N minus 1 (Nm1) before 
c     thermodynamical calulations.  Following this routine,
c     the new ice state will be at time level N (except
c     HSNOW which isn't stored at multiple time levels.)
                  HEFFNm1(I,J,bi,bj) = HEFF(I,J,bi,bj) 
                  AREA(I,J,bi,bj) = MIN(1.0 _d 0, AREA(I,J,bi,bj)
                  AREANm1(I,J,bi,bj) = AREA(I,J,bi,bj) 



               ENDDO
            ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE tice(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE precip(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Sanity check on HEFF, HSNOW, AREA, and PRECIP. The ice 
c     dynamics subroutines could generate unphysical ice state 
c     variables (e.g., zero/negative AREA with positive HEFF 
c     and/or HSNOW)

c     Note: the zeroing out negative HEFF and/or HSNOW here (due
c     to ice dynamics) might violate mass/energy conservation.
c     If this is a problem, choose a conservative ice advection
c     and diffusion scheme. 
            DO J=1,sNy
               DO I=1,sNx

#ifdef SEAICE_DEBUG
                  IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &               (J .EQ. SEAICE_debugPointY) ) THEN

                     print '(A,i6,4(1x,1PE24.15))',
     &                  'beg iter, ANm1, HNm1, HSN',myIter,
     &                  AREANm1(I,J,bi,bj), HEFFNm1(I,J,bi,bj), 
     &                  HSNOW(I,J,bi,bj)

                  ENDIF
#endif

                  HEFFNm1(I,J,bi,bj) = MAX(ZERO,HEFFNm1(I,J,bi,bj)) 
                  HSNOW(I,J,bi,bj)   = MAX(ZERO,HSNOW(I,J,bi,bj)  ) 
                  AREANm1(I,J,bi,bj) = MAX(ZERO,AREANm1(I,J,bi,bj)) 

c     If either HEFF or AREA are zero, ensure all other ice
c     state variables are also zero.
                  IF ((HEFFNm1(I,J,bi,bj) .EQ. ZERO) .OR. 
     &               (AREANm1(I,J,bi,bj) .EQ. ZERO)) THEN 

                     AREANm1(I,J,bi,bj) = 0. _d 0
                     HSNOW(I,J,bi,bj)   = 0. _d 0
                     HEFFNm1(I,J,bi,bj) = 0. _d 0

                     HICE_ACTUAL(I,J)   = 0. _d 0
                     HSNOW_ACTUAL(I,J)  = 0. _d 0

                  ELSE          !/ Heff and Area > 0

c     Caclulate the actual (not mean) thicknesses 
c     of the ice and any snow in the grid cell 
                     HICE_ACTUAL(I,J)  = 
     &                  HEFFNm1(I,J,bi,bj)/AREANm1(I,J,bi,bj)

                     HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/
     &                  AREANm1(I,J,bi,bj)

                  ENDIF 

                  HSNOW_ORIG(I,J) = HSNOW(I,J,bi,bj)
                  
c     This is to prevent (spurious) negative precipitation
c     from decreasing snow depth.
                  IF (PRECIP(I,J,bi,bj) .LT. ZERO) THEN 
                     PRECIP(I,J,bi,bj) = ZERO 
                  ENDIF 
               ENDDO
            ENDDO


#ifdef SEAICE_SIMULATE_DIVERGENCE_IN_1D_MONDEL
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Simulate unresolved dynamical convergence: This optional  
c     code parameterizes unresolved small-scale mechanical 
c     divergence of the ice pack.  To simulate regular lead 
c     opening, a small fraction of the ice area is removed
c     each time step while conserving ice and snow mass.

            DO J=1,sNy
               DO I=1,sNx
c     Removes about 1.2% of the concentration in 48 hours.
                  AREANm1(I,J,bi,bj) = AREANm1(I,J,bi,bj)*0.99975 _d 0
               ENDDO
            ENDDO
#endif            
            

C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Snow : Treat precipitation as snow and accumulate it onto 
c     existing snow (or bare ice) it if the snow/ice surface 
c     temperature is below the freshwater freezing point.  If the
c     snow/ice surface temperature is at the freezing point, 
c     assume  precipitation falls as rain and flows to the sea
            DO J=1,sNy
               DO I=1,sNx
                  IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN

                     IF (TICE(I,J,bi,bj) .LT. TMELT) THEN
c     Snow falls onto freezing surface remaining
                        SnowAccRateOverIce(I,J) = 
     &                     PRECIP(I,J,bi,bj)*RHOFW/SEAICE_rhoSnow

                     ELSE 
c     The snow melts on impact is is considered 
c     nothing more than rain which runs to the sea
                        PrecipRateOverIceSurfaceToSea(I,J)=
     &                     PRECIP(I,J,bi,bj)
                     ENDIF

c     The mean depth of new snow accumulation over the
c     grid cell (m) 
                     SnowAccOverIce(I,J) =   SnowAccRateOverIce(I,J)
     &                  * SEAICE_deltaTtherm*AreaNm1(I,J,bi,bj)

                  ENDIF
               ENDDO
            ENDDO
            

C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Retrieve near-surface wind speed. 
            DO J=1,sNy
               DO I=1,sNx
C     copy the wind speed computed in exf_wind.F to UG
                  UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE tice   = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */


#ifndef FORBID_ICE_SURFACE_ATMOSPHERE_HEAT_FLUXES
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Calculate sea ice/snow thermodynamic fluxes within and out 
c     of the snow and ice.

            CALL SEAICE_BUDGET_ICE(
     I         UG, HICE_ACTUAL, HSNOW_ACTUAL, 
     U         TICE, 
     O         F_io_net,F_ia_net,F_ia, QSWI, 
     I         bi, bj, myTime, myIter, myThid )

#else 

c     Forbid ice surface atmosphere heat fluxes: Sometimes it is 
c     useful to have a setup without ice-atmosphere heat fluxes. 
C     This optional flag zeros fluxes at and through and snow/
c     ice surface.  Sensible heat fluxes from the ocean to the 
c     ice base and fluxes between the open water fraction of the 
c     cell and the atmosphere are left intact.

            DO J=1,sNy
               DO I=1,sNx
                  F_ia_net (I,J)  = 0. _d 0
                  F_ia (I,J)      = 0. _d 0
                  F_io_net(I,J)   = 0. _d 0
                  QSWI(I,J)       = 0. _d 0
                  TICE(I,J,bi,bj)   = TMELT
               ENDDO
            ENDDO
#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Debugging
#ifdef SEAICE_DEBUG
            print '(A,i6,4(1x,1PE24.15))',
     &         'sig: iter, F_ia,F_ia_net ',myIter,
     &         F_ia(SEAICE_debugPointX, SEAICE_debugPointY),
     &         F_ia_net(SEAICE_debugPointX, SEAICE_debugPointY)

            print '(A,i6,4(1x,1PE24.15))',
     &         'sig: iter,_io_net,QSWI   ',myIter,
     &         F_io_net(SEAICE_debugPointX, SEAICE_debugPointY),
     &         QSWI(SEAICE_debugPointX, SEAICE_debugPointY)

            print '(A,i6,4(1x,1PE24.15))',
     &         'sig: iter, TICE     ',myIter,
     &         TICE(SEAICE_debugPointX, SEAICE_debugPointY,bi,bj)

#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Calculate the snow and ice growth rates from 1) surface
c     melt and 2) basal thickening:  Surface energy heat flux 
c     convergence (F_ia_net) melts snow and ice.  Upward
c     conductive heat fluxes drive basal thickening.

c     If the surface energy heat flux convergence cannot melt all
c     the snow, F_ia_net is driven to zero.  If all snow is
c     melted, F_ia_net is reduced and the remaining energy melts
c     ice.  

            DO J=1,sNy
               DO I=1,sNx

                  F_ia_net_before_snow(I,J) = F_ia_net(I,J)

c     Continue if there is ice in the cell.
                  IF (AreaNm1(I,J,bi,bj) .LE. ZERO) THEN 
                     IceGrowthRateUnderExistingIce(I,J) = 0. _d 0
                     IceGrowthRateFromSurface(I,J)      = 0. _d 0
                     NetExistingIceGrowthRate(I,J)      = 0. _d 0
                  ELSE
c     The growth rate (m/s) beneath ice due to
c     heat flux divergence at the ice base associated
c     with upward conductive heat fluxes through the ice 
c     (F_io_net)
                     IceGrowthRateUnderExistingIce(I,J)=F_io_net(I,J)*QI

c     The potential snow melt rate (m/s) due to surface
c     heat flux convergence.  Note, snow melt requires a
c     negative F_ia_net (implying convergence) for 
c     a nonzero PSMRFW.
                     PotSnowMeltRateFromSurf(I,J)= - F_ia_net(I,J)*QS

c     This is the depth of snow (m) that would be melted 
c     over one time step.
                     PotSnowMeltFromSurf(I,J) =
     &                  PotSnowMeltRateFromSurf(I,J)* SEAICE_deltaTtherm

c     If more can be melted than exists, the melt rate
c     is reduced.  

                     IF (PotSnowMeltFromSurf(I,J) .GE. 
     &                  HSNOW_ACTUAL(I,J)) THEN

c     Snow depth to be melted and the snow melt rate.
                        SnowMeltFromSurface(I,J) = HSNOW_ACTUAL(I,J)

                        SnowMeltRateFromSurface(I,J) = 
     &                     SnowMeltFromSurface(I,J)/ SEAICE_deltaTtherm

c     SurfHeatFluxConvergToSnowMelt is the part of the 
c     total heat flux convergence which melts snow.
                        SurfHeatFluxConvergToSnowMelt(I,J) =
     &                     -HSNOW_ACTUAL(I,J)/QS/SEAICE_deltaTtherm
                     ELSE
c     Snow remains after melt.  
                        SnowMeltFromSurface(I,J)=
     &                     PotSnowMeltFromSurf(I,J)

                        SnowMeltRateFromSurface(I,J) = 
     &                     PotSnowMeltRateFromSurf(I,J)

                        SurfHeatFluxConvergToSnowMelt(I,J) =
     &                     F_ia_net(I,J)
                     ENDIF

c     Reduce the heat flux convergence available to melt 
c     ice by the amount used to melt snow.
                     F_ia_net(I,J) = 
     &                  F_ia_net(I,J)-SurfHeatFluxConvergToSnowMelt(I,J)

                     IceGrowthRateFromSurface(I,J) = F_ia_net(I,J)*QI

c     The total growth rate (m/s) of the existing ice - 
c     the rate of new ice accumulation less the rate of 
c     surface melt.
                     NetExistingIceGrowthRate(I,J) = 
     &                  IceGrowthRateUnderExistingIce(I,J) +
     &                  IceGrowthRateFromSurface(I,J)
                  ENDIF

               ENDDO            !/ i
            ENDDO               !/ j


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Retrieve upper ocean temp and store temporarily in TMIX.
            DO J=1,sNy
               DO I=1,sNx

C     Set the upper ocean temperature in Kelvin
                  TMIX(I,J,bi,bj)= theta(I,J,kSurface,bi,bj) + TMELT

               ENDDO
            ENDDO
            

C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Retrieve the air-sea heat and shortwave radiative fluxes
            CALL SEAICE_BUDGET_OCEAN(
     I         UG, 
     U         TMIX, 
     O         F_ao, QSWO, 
     I         bi, bj, myTime, myIter, myThid )

            
#ifdef SEAICE_DEBUG
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Debugging
            print '(A,i6,4(1x,1PE24.15))',
     &         'myIter, Fao, QSWO ', myIter,
     &         F_ao(SEAICE_debugPointX, SEAICE_debugPointY),
     &         QSWO(SEAICE_debugPointX, SEAICE_debugPointY),
     &         TMIX(SEAICE_debugPointX, SEAICE_debugPointY,bi,bj)
#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Calculate the ice growth rate from open water heat fluxes:
c     Turbulent fluxes (F_ao) and shortwave energy convergence in
c     the uppermost ocean grid cell contribute.
c     
c     SWFRACB - the fraction of incoming sw radiation absorbed in 
c     the uppermost ocean grid cell 
            DO J=1,sNy
               DO I=1,sNx

c     The contribution of shortwave heating is 
c     not included without #define SHORTWAVE_HEATING

#ifdef SHORTWAVE_HEATING
                  QSWO_BELOW_FIRST_LAYER(i,j)= QSWO(I,J)*SWFRACB
                  QSWO_IN_FIRST_LAYER(I,J)   = QSWO(I,J)*(ONE - SWFRACB)
#else 
                  QSWO_BELOW_FIRST_LAYER(i,j)= 0. _d 0
                  QSWO_IN_FIRST_LAYER(I,J)   = 0. _d 0
#endif

                  IceGrowthRateOpenWater(I,J)= QI*
     &               (F_ao(I,J) - QSWO(I,J) + QSWO_IN_FIRST_LAYER(I,J))

               ENDDO
            ENDDO        
            

#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE theta(:,:,:,bi,bj)= comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Calcuate the ice growth rate from turbulent heat fluxes from
c     the ocean.

            DO J=1,sNy
               DO I=1,sNx

#ifdef SEAICE_VARIABLE_FREEZING_POINT
C     Calculate the seawater freezing point
                  TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 
     &               0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
                  
c     Bound the ocean temperature to be at or above the freezing point.
                  surf_theta = max(theta(I,J,kSurface,bi,bj), TBC)


c     This model has two alternative schemes to calculate turbulent ocean-ice
c     heat fluxes.  Each is described in turn
c     
c     ==== METHOD 1 ====
c     
c     The first uses an empircal relationship between the 
c     'far-field' ocean temperature and ocean to sea ice turbulent heat fluxes 
c     given by McPhee:
c     
c     Flux  = rho_sw * cp_sw * <w'T'> 
c     <w'T'>= S_t *  u_star * (T_o - T_f)
c     
c     Where,
c     rho_sw : seawater density (kg m^-3)
c     cp_sw  : seawater heat capcity (J kg^-1 K^-1)
c     S_t    : Stanton Number ~ 0.0056 (dimensionless)
c     u_star : friction velocity beneath ice ~ 0.015 m s^-1
c     T_o, T_f : The 'far-field' ocean temperature and ice
c     freezing point (deg C)
c     
c     Using typical values, ice advected over waters warmer by 1 degree
c     will be subjected to a basal heat flux of ~ 345 W m^-2.  
c     
c     In the model, the criteria for ice growth is that the air-sea
c     heat loss must exceed the potential ocean-ice heat flux (i.e., more 
c     potential ice production over one time step than melt).  
c     Using the above parameterization, ice will form in waters which 
c     are much warmer than its salinity-determined freezing point
c     using typical wintertime conditions in, say, the North Atlantic.
c     
c     Therefore, when no ice is present, an additional factor is applied
c     to the above <w'T'> (mixedLayerTurbulenceFactor) to represent the idea that 
c     turbulent mixing at and near the surface of an ice-free ocean
c     is much greater than mixing beneath an ice-covered one.
c     
c     A factor of 12.5 is chosen for mixedLayerTurbulenceFactor.  Consequently,
c     for each 0.1 degree above freezing of the upper ocean grid cell,
c     the air-sea heat losses must exceed ~ 515 W m^-2 before ice forms.
c     
c     Once ice gains a foothold in a grid cell, the McPhee parameterization
c     is immediately invoked.
c     
c     ==== METHOD 2 ====
c     
c     The second scheme (the original and now outdated version)
c     subsumes (S_t * u_star) into a single variable (SEAICE_gamma_t)
c     in the following way:
c     
c     Flux  = rho_sw * cp_sw * GAMMA 
c     GAMMA = dRf(1)/SEAICE_gamma_t * (T_o - T_f)
c     
c     Where,
c     dRf(1)  : depth of surface level grid cell (originally 
c     assumed to be 10 m)
c     SEAICE_gamma_t : an empirical parameter (~ 3 days in seconds)
c     
c     Using typical values, ice advected into a grid cell 
c     that is warmer than the sea ice freezing point by 1 degree
c     will be subjected to a basal heat flux of ~ 160 W m^-2.   Therefore,
c     as written, one should choose a SEAICE_gamma_t = 1.4 days in seconds
c     to have flux magnitudes match those of the McPhee parameterization.
c     
c     The original scheme is accessed by defining  #SEAICE_USE_ORIGINAL_HEAT FLUX

#ifndef SEAICE_USE_ORIGINAL_HEAT_FLUX

c     If ice is present, MixedLayerTurbulenceFactor = 1.0, else 12.50
                  IF (AREANm1(I,J,bi,bj) .GT. 0. _d 0) THEN
                     MixedLayerTurbulenceFactor = ONE
                  ELSE
                     MixedLayerTurbulenceFactor = 12.5 _d 0
                  ENDIF

                  F_mi(I,J) = -STANTON_NUMBER * USTAR_BASE * RHOSW *
     &               CPW *(surf_theta - TBC)*MixedLayerTurbulenceFactor
#else

c     no turbulence factor using original_heat_flux scheme
                  F_mi(I,J) = -GAMMA*RHOSW*CPW *(surf_theta - TBC)

#endif /* SEAICE_USE_ORIGINAL_HEAT_FLUX */

                  IceGrowthRateMixedLayer(I,J) = F_mi(I,J)*QI;

               ENDDO            !/ i
            ENDDO               !/ j

#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE S_h(:,:)         = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */



C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Calculate the ice thickness derivative (S_h)

            DO J=1,sNy
               DO I=1,sNx
                  S_h(I,J) = 
     &               NetExistingIceGrowthRate(I,J)*AREANm1(I,J,bi,bj)+
     &               (ONE -AREANm1(I,J,bi,bj))*
     &               IceGrowthRateOpenWater(I,J) +
     &               IceGrowthRateMixedLayer(I,J)

c     Both the accumulation and melt rates are in terms
c     of actual snow thickness.  As with HEFF,
c     multiplying actual snow depth by AREA converts to
c     mean snow thickness.
                  S_hsnow(I,J) =     AREANm1(I,J,bi,bj)* (
     &               SnowAccRateOverIce(I,J) - 
     &               SnowMeltRateFromSurface(I,J)     ) 

               ENDDO
            ENDDO


#ifdef ALLOW_SALT_PLUME
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Calculate the flux of salt to be treated with the salt plume
c     parameterization.
            IF (useSALT_PLUME) THEN
               DO J=1,sNy
                  DO I=1,sNx

c     It is assumed that ice production in leads can generate
c     salt plumes.  The fraction of the salt sent to the plume package
c     from ice produced in leads (defined as the open water
c     fraction) is a nonlinear function of ice concentration, AREA.
c     
c     Specifically, the function is a sigmoid with range and 
c     domain {0,1}.  The function, f(AREA), has a single free parameter,
c     SEAICE_plumeInflectionPoint, the inflection point of the curve.  
c     By construction, the function has the following properties:
c     f(1) \approx 1.0
c     f(SEAICE_plumeInflectionPoint) = 0.5
c     f(0) \approx 0.0 (when SEAICE_plumeInflectionPoint \geq 0.5)
c     f(0) > 0.0 (when SEAICE_plumeInflectionPoint < 0.5)
c     
c     As AREA --> 1, the open water fraction occurs
c     in narrow leads, new ice production become spatially non-uniform,
c     and the assumptions motivating KPP no longer hold.  To treat
c     overturning in a more physically realistic way, the salt produced
c     in the leads should be sent to depth via the plume package.  To assure 
c     only narrow leads generate plumes, choose a SEAICE_plumeInflectionPoint 
c     of > 0.8.

c     Ensure that there is already ice present or that the total ice
c     ice tendency term is positive.  We don't want to release 
c     salt if sea ice is not established in the cell.

                     IF ((AREANm1(I,J,bi,bj) .GT. ZERO) .OR. 
     &                  (S_h(I,J)           .GT. ZERO)) THEN
                        
                        leadPlumeFraction(I,J) = 
     &                     (ONE + EXP( ( SEAICE_plumeInflectionPoint- 
     &                     AREANm1(I,J,bi,bj)
     &                     ) * 5._d 0/
     &                     (ONE - SEAICE_plumeInflectionPoint)
     &                     )
     &                     )**(-ONE) 
                        
c     Only consider positive ice growth rate in leads
c     for the salt plume flux.
                        IceGrowthRateInLeads(I,J) = max( ZERO,
     &                     (ONE - AREANm1(I,J,bi,bj)) * 
     &                     IceGrowthRateOpenWater(I,J))

                        saltPlumeFlux(I,J,bi,bj) = 
     &                     leadPlumeFraction(I,J) *
     &                     HEFFM(I,J,bi,bj)*IceGrowthRateInLeads(I,J)*
     &                     ICE2WATR*rhoConstFresh*
     &                     (salt(I,J,kSurface,bi,bj) - 
     &                     SEAICE_salinity_fixed)
                        
                     ENDIF

                  ENDDO         !/ Use plume 
               ENDDO            !/ j
            ENDIF               !/ i

#endif /* ALLOW_SALT_PLUME */


#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE S_h(:,:)         = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE S_hsnow(:,:)     = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Calculate the ice area derivative (S_a)
            DO J=1,sNy
               DO I=1,sNx

c     In the following, time derivatives of AREA require
c     divisions by HEFF. To prevent sensitivity blow up when
c     HEFF --> 0, we divide by an approximation to HEFF, HEFF*:
c     
c     HEFF* = sqrt(HEFF^2 + heff_min^2)
c     Where heff_min is implicitly defined as 0.1 m. 
c     
c     HEFF* is defined as a hyperbola with slope = 1 and 
c     y-intercept of heff_min.  The gradient of 1/HEFF* is 
c     well-behaved (correct sign) and deviates from 1/HEFF 
c     significantly only when HEFF --> 0.

                  IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN
                     HEFF_MIN = SQRT( 0.01 _d 0 + 
     &                  HEFFNm1(I,J,bi,bj) * HEFFNm1(I,J,bi,bj) )
                  ELSE
c     Just in case somehow we miss a case, we want to divide
c     by a sensible number, here 10 cm.
                     HEFF_MIN = max(HEFFNm1(I,J,bi,bj), 0.1 _d 0)
                  ENDIF

c     Caculate the ice area growth rate from the open water fluxes.
c     First, determine whether the open water growth rate is positive or
c     negative.  If positive, make sure that ice is present or that the 
c     net ice thickness growth rate is positive before extending 
c     the ice cover across the grid cell.

                  IF (IceGrowthRateOpenWater(I,J) .GT. ZERO) THEN

c     Determine which hemisphere for hemisphere-dependent so-called
c     "lead closing variable", HO

                     IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
                        S_a_from_IGROW(I,J) = (ONE-AREANm1(I,J,bi,bj))*
     &                     IceGrowthRateOpenWater(I,J)/HO_south
                     ELSE
                        S_a_from_IGROW(I,J) = (ONE-AREANm1(I,J,bi,bj))*
     &                     IceGrowthRateOpenWater(I,J)/HO
                     ENDIF

c     If there is already ice, add to S_a
                     IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN
                        S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)

c     otherwise, add to S_a only if the net ice growth rate
c     is positive
                     ELSE IF (S_h(I,J) .GT. ZERO) THEN
                        S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)
                     ENDIF

                  ELSE IF ( AREANm1(I,J,bi,bj) .GT. ZERO) THEN
c     The open water growth rate is negative - contract the ice cover.

                     S_a(I,J) = S_a(I,J) +
     &                  AREANm1(I,J,bi,bj)/(2. _d 0*HEFF_MIN)*
     &                  IceGrowthRateOpenWater(I,J)*
     &                  (ONE - AREANm1(I,J,bi,bj))

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0. _d 0
                  ENDIF

C     Contract ice area if IceGrowthRateMixedLayer is negative
c     and ice is present
                  IF ( (IceGrowthRateMixedLayer(I,J) .LE. ZERO) .AND. 
     &               (AREANm1(I,J,bi,bj) .GT. ZERO) ) THEN

                     S_a(I,J) = S_a(I,J) +
     &                  AREANm1(I,J,bi,bj)/(2. _d 0 *HEFF_MIN)*
     &                  IceGrowthRateMixedLayer(I,J)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0. _d 0
                  ENDIF

C     Contract ice area if NetExistingIceGrowthRate is negative
c     and ice is present
                  IF ( (NetExistingIceGrowthRate(I,J) .LE. ZERO) .AND. 
     &               (AREANm1(I,J,bi,bj) .GT. ZERO)) THEN

                     S_a(I,J) = S_a(I,J) +
     &                  AREANm1(I,J,bi,bj)/(2. _d 0 * HEFF_MIN)*
     &                  NetExistingIceGrowthRate(I,J)*AREANm1(I,J,bi,bj)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0. _d 0
                  ENDIF
                  
               ENDDO            !/ i
            ENDDO               !/ j
            

#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE S_a(:,:)          = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE S_h(:,:)          = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE f_ao(:,:)         = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE qswi(:,:)         = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE qswo(:,:)         = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
C     ADJ STORE area(:,:,bi,bj)   = comlev1_bibj, 
C     ADJ &                         key = iicekey, byte = isbyte
#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Update AREA, HEFF, and HSNOW using the time derivativees
c     calculated above.
            DO J=1,sNy
               DO I=1,sNx

                  AREA(I,J,bi,bj) = AREANm1(I,J,bi,bj) + 
     &               SEAICE_deltaTtherm * S_a(I,J)

                  HEFF(I,J,bi,bj) = HEFFNm1(I,J,bi,bj) +
     &               SEAICE_deltaTTherm * S_h(I,J)

                  HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) +
     &               SEAICE_deltaTTherm * S_hsnow(I,J)

               ENDDO
            ENDDO
            
            
#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Sanity check on HEFF, HSNOW, and AREA:  After updating
c     the ice state with the time derivatives it is possible that
c     an unphysical ice state occurs (e.g., zero/negative AREA 
c     with positive HEFF and/or HSNOW).

c     Note: the zeroing out negative HEFF and/or HSNOW here will
c     not violate mass/energy conservation.  Energy and mass 
c     fluxes are calculated based on the actual change in ice
c     and snow volume between time levels N-1 and N.
            DO J=1,sNy
               DO I=1,sNx

                  HEFF(I,J,bi,bj)  = HEFFM(I,J,bi,bj) *
     &               MAX(ZERO,HEFF(I,J,bi,bj)) 

                  HSNOW(I,J,bi,bj) = HEFFM(I,J,bi,bj) * 
     &               MAX(ZERO,HSNOW(I,J,bi,bj)) 

                  AREA(I,J,bi,bj) = MIN(1.0 _d 0, AREA(I,J,bi,bj)

                  AREA(I,J,bi,bj)  = HEFFM(I,J,bi,bj) *
     &               MAX(ZERO,AREA(I,J,bi,bj)) 

c     If either HEFF or AREA are zero, make all other ice
c     state variables zero.
                  IF ((HEFF(I,J,bi,bj) .EQ. ZERO) .OR. 
     &               (AREA(I,J,bi,bj) .EQ. ZERO)) THEN 

                     AREA(I,J,bi,bj)  =  0. _d 0
                     HSNOW(I,J,bi,bj) =  0. _d 0
                     HEFF(I,J,bi,bj)  =  0. _d 0

                     HICE_ACTUAL(I,J)   = 0. _d 0
                     HSNOW_ACTUAL(I,J)  = 0. _d 0

                  ELSE          !/ Heff and Area > 0

c     Caclulate the actual (not mean) thicknesses of the 
c     ice and any snow in the grid cell 
                     HICE_ACTUAL(I,J)  = 
     &                  HEFF(I,J,bi,bj)/AREA(I,J,bi,bj)

                     HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/
     &                  AREA(I,J,bi,bj)

                  ENDIF 

               ENDDO            !/ I
            ENDDO               !/ J


#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
            DO J=1,sNy
               DO I=1,sNx


#ifdef SEAICE_DEBUG
c     The expected amount of new mean ice volume
                  ExpectedIceVolumeChange(I,J)  = S_h(I,J) *
     &               SEAICE_deltaTtherm

c     The expected amount of new mean snow volume
                  ExpectedSnowVolumeChange(I,J) = S_hsnow(I,J)*
     &               SEAICE_deltaTtherm

#endif

c     The effective shortwave radiation (after modification
c     by ice cover).
#ifdef SHORTWAVE_HEATING
                  QSW(I,J,bi,bj)  =
     &               QSWI(I,J)  * (      AREANm1(I,J,bi,bj)) +
     &               QSWO(I,J)  * (ONE - AREANm1(I,J,bi,bj))
#else
                  QSW(I,J,bi,bj) = 0. _d 0
#endif

c     The change in mean ice thickness over the time step. 
c     Note, this quantity may not equal S_h*SEAICE_deltaTtherm as
c     AREA or HEFF may end up zero or negative which requires that both 
c     be reset to ZERO.
                  ActualIceVolumeChange(I,J) = 
     &               HEFF(I,J,bi,bj) - HEFFNm1(I,J,bi,bj)

c     The depth of lost mean snow thickness.
                  ActualNewTotalSnowMelt(I,J) = 
     &               HSNOW_ORIG(I,J) +
     &               SnowAccOverIce(I,J) -
     &               HSNOW(I,J,bi,bj)

c     The energy required to melt or form the new ice.
                  EnergyInNewIceVolume(I,J) = 
     &               ActualIceVolumeChange(I,J)/QI

c     The energy divergence (J/m^2) out of the ocean.  This
c     includes the fluxes to the base of the seaice (F_io_net)
c     and shortwave radiation penetrating through the ice (QSWI)
c     weighted by ice-covered fraction, and the air-sea
c     heat fluxes weighted by the open water fraction (F_ao).
                  NetEnergyOutOfOcean(I,J) = SEAICE_deltaTtherm *
     &               (AREANm1(I,J,bi,bj) * (F_io_net(I,J) + QSWI(I,J))
     &               +     (ONE - AREANm1(I,J,bi,bj)) *  F_ao(I,J))

c     The net energy divergence from the snow/ice surface.
c     Note, when TICE < TMELT, F_ia_net = 0 (i.e., an energy balance
c     condition is achieved at the snow/ice surface.
                  NetEnergyOutOfIce(I,J) = SEAICE_deltaTTherm *
     &               AREANm1(I,J,bi,bj) * F_ia_net(I,J)

c     The energy divergence (J/m^2) from the ocean which is 
c     not offset by the latent energy of fusion of any newly created
c     or melted ice.  This residual energy loss will reduce the 
c     seawater temperature. 
                  ResidualEnergyOutOfOcean(I,J) = 
     &               NetEnergyOutOfIce(I,J) + 
     &               NetEnergyOutOfOcean(I,J) -
     &               EnergyInNewIceVolume(I,J)

C     The energy flux (W/m^2) associated with the above residual 
c     energy divgence.  
                  QNET(I,J,bi,bj) =
     &               ResidualEnergyOutOfOcean(I,J) / SEAICE_deltaTtherm


C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     The change of freshwater content  per unit area over the entire
c     cell, not just over the ice covered bits.  This term is only used
c     to calculate freshwater fluxes for the purpose of changing the
c     salinity of the liquid cell. 

c                 If the seawater salinity is less than SEAICE_salinity_fixed
c                 or zero, assume that ice is as saline as the seawater.
c                 salinity
                  IF  ((salt(I,J,kSurface,bi,bj) .EQ. ZERO) .OR. 
     &                 (salt(I,J,kSurface,bi,bj) .LE. 
     &                   SEAICE_salinity_fixed)) THEN

                     FreshwaterContribFromIce(I,J) = ZERO 
                 
c                 If seawater salinity is greater than the specified
c                 ice salinity, the freshwater contribution from ice is
c                 the volume of freshwater required to mix with
c                 seawater to yield sea ice having salinity
c                 SEAICE_salinity_fixed.  Another way of looking at this
c                 formulation is to assume that the sea ice is a mixture 
c                 of a volume of freshwater and volume of water with 
c                 salinity "salt".  When ice grows (or melts), only the 
c                 extraction (release) of the freshwater volume 
c                 alters the seawater salinity.
                  ELSEIF ( salt(I,J,kSurface,bi,bj) .GT. 
     &                     SEAICE_salinity_fixed   )  THEN 

                       FreshwaterContribFromIce(I,J) =
     &                   - ActualIceVolumeChange(I,J)*
     &                   SEAICE_rhoICE/ RHOFW* (ONE - 
     &                   SEAICE_salinity_fixed/salt(I,J,kSurface,bi,bj))

                  ENDIF

c     The freshwater contribution from snow comes from melt.
                  FreshwaterContribFromSnowMelt(I,J) =
     &               ActualNewTotalSnowMelt(I,J)*SEAICE_rhoSnow/RHOFW

                  EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &               ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &               * ( ONE - AREANm1(I,J,bi,bj) )
     &               - PrecipRateOverIceSurfaceToSea(I,J)*
     &               AREANm1(I,J,bi,bj)
#ifdef ALLOW_RUNOFF
     &               - RUNOFF(I,J,bi,bj)
#endif
     &               - (FreshwaterContribFromIce(I,J) +
     &               FreshwaterContribFromSnowMelt(I,J))/
     &               SEAICE_deltaTtherm )*rhoConstFresh

               ENDDO            !/ i
            ENDDO               !/ j


#ifdef SEAICE_DEBUG
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Debugging calculations

            DO J=1,sNy
               DO I=1,sNx

C     Calcuate the shortwave flux absorbed in the upper ocean
c     grid cell
#ifdef SHORTWAVE_HEATING
                  QSW_absorb_in_first_layer(I,J) = QSW(I,J,bi,bj)*
     &               (ONE - SWFRACB)
#else 

                  QSW_absorb_in_first_layer(I,J) = 0. _d 0
#endif

C     Calcuate the sw flux beneath the upper ocean grid cell
                  QSW_absorb_below_first_layer(I,J) = 
     &               QSW(I,J,bi,bj) -  QSW_absorb_in_first_layer(I,J)

                  PredTempChangeFromQSW(I,J) =
     &               - QSW_absorb_in_first_layer(I,J)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromOA_MQNET(I,J) = 
     &               -(QNET(I,J,bi,bj)-QSWO(I,J)) * 
     &               (ONE-AREANm1(I,J,bi,bj)) * FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IO_NET(I,J) = - F_io_net(I,J) *
     &               AREANm1(I,J,bi,bj)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IA_NET(I,J) = - F_ia_net(I,J) *
     &               AREANm1(I,J,bi,bj)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromNewIceVol(I,J) = 
     &               EnergyInNewIceVolume(I,J)*ENERGY_TO_DELTA_TEMP

                  PredTempChange(I,J) = 
     &               PredTempChangeFromQSW(I,J) +
     &               PredTempChangeFromOA_MQNET(I,J) +
     &               PredTempChangeFromF_IO_NET(I,J) +
     &               PredTempChangeFromF_IA_NET(I,J) +
     &               PredTempChangeFromNewIceVol(I,J)

               ENDDO
            ENDDO
#endif /* seaice debug */   


#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            

#ifdef ALLOW_SEAICE_FLOODING
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Convert flooded snow to ice
            IF(SEAICEuseFlooding) THEN

               DO J = 1,sNy
                  DO I = 1,sNx
                     EnergyToMeltSnowAndIce(I,J) = 
     &                  HEFF(I,J,bi,bj)/QI +
     &                  HSNOW(I,J,bi,bj)/QS
                     
                     deltaHS = FL_C2*( HSNOW_ACTUAL(I,J) -
     &                  HICE_ACTUAL(I,J)*FL_C3 )
                     
                     IF (deltaHS .GT. ZERO) THEN
                        deltaHI = FL_C4*deltaHS
                        
                        HICE_ACTUAL(I,J) = HICE_ACTUAL(I,J)  
     &                     + deltaHI

                        HSNOW_ACTUAL(I,J)= HSNOW_ACTUAL(I,J) 
     &                     - deltaHS

                        HEFF(I,J,bi,bj)= HICE_ACTUAL(I,J) *
     &                     AREA(I,J,bi,bj)

                        HSNOW(I,J,bi,bj) = HSNOW_ACTUAL(I,J)*
     &                     AREA(I,J,bi,bj)
                        
                        EnergyToMeltSnowAndIce2(I,J) = 
     &                     HEFF(I,J,bi,bj)/QI +
     &                     HSNOW(I,J,bi,bj)/QS
                        
#ifdef SEAICE_DEBUG

                        IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &                     (J .EQ. SEAICE_debugPointY) ) THEN
                           print '(A,4(1x,1PE24.15))',
     &                        'Energy to melt snow+ice: pre,post,delta',
     &                        EnergyToMeltSnowAndIce(I,J),  
     &                        EnergyToMeltSnowAndIce2(I,J),
     &                        EnergyToMeltSnowAndIce(I,J) - 
     &                        EnergyToMeltSnowAndIce2(I,J)
                        ENDIF
#endif /* SEAICE DEBUG */

                     ENDIF 

                  ENDDO         !/ I
               ENDDO            !/ J

            ENDIF               !/ SEAICEuseFlooding
#endif /* SEAICE_FLOODING


#ifdef ALLOW_AUTODIFF_TAMC
C     ADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
C     ADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
C     ADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ATMOSPHERIC_LOADING
            IF ( useRealFreshWaterFlux ) THEN
               DO J=1,sNy
                  DO I=1,sNx

                     sIceLoad(i,j,bi,bj) = HEFF(I,J,bi,bj)*
     &                  SEAICE_rhoIce + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow

                  ENDDO
               ENDDO
            ENDIF
#endif


#ifdef SEAICE_DEBUG
C---  +----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     Debugging calculations
            DO j=1,sNy
               DO i=1,sNx

                  IF ( (i .EQ. SEAICE_debugPointX)   .and.  
     &               (j .EQ. SEAICE_debugPointY) ) THEN

                     print *,'ifsig: myTime,myIter: ',myTime,myIter

                     print '(A,2i4)',
     &                  'ifice i j -------------- ',i,j

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice IGR(ML OW ICE)     ',
     &                  IceGrowthRateMixedLayer(i,j),
     &                  IceGrowthRateOpenWater(i,j),
     &                  NetExistingIceGrowthRate(i,j),
     &                  SEAICE_deltaTtherm
                     
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice F(mi ao)           ',
     &                  F_mi(i,j), F_ao(i,j)
                     
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice Fi(a,ant2/1 ont)   ',
     &                  F_ia(i,j),
     &                  F_ia_net_before_snow(i,j), 
     &                  F_ia_net(i,j), 
     &                  F_io_net(i,j)
                     
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice AREA2/1 HEFF2/1    ',
     &                  AREANm1(I,J,bi,bj),
     &                  AREA(i,j,bi,bj),
     &                  HEFFNm1(I,J,bi,bj),
     &                  HEFF(i,j,bi,bj)


#ifdef ALLOW_SALT_PLUME
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice A,IGLEA,LPF,SPF    ',
     &                  AREANm1(I,J,bi,bj),
     &                  IceGrowthRateInLeads(I,J),
     &                  leadPlumeFraction(I,J),
     &                  saltPlumeFlux(i,j,bi,bj)
#endif
                     
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice HSNOW2/1 TMX TBC   ',
     &                  HSNOW_ORIG(I,J),
     &                  HSNOW(I,J,bi,bj),
     &                  TMIX(i,j,bi,bj)- TMELT, 
     &                  TBC

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice TI ATP LWD         ',
     &                  TICE(i,j,bi,bj) - TMELT,
     &                  ATEMP(i,j,bi,bj) -TMELT,
     &                  LWDOWN(i,j,bi,bj)


                     print '(A,4(1x,1PE24.15))',
     &                  'ifice S_a S_h S_hsnow    ',
     &                  S_a(i,j),
     &                  S_h(i,j),
     &                  S_hsnow(i,j)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice IVC(E A ENIN)      ',
     &                  ExpectedIceVolumeChange(i,j),
     &                  ActualIceVolumeChange(i,j),
     &                  EnergyInNewIceVolume(i,j)
                     
                     print '(A,4(1x,1PE24.15))',
     &                  'ifice EF(NOS RE) QNET    ',
     &                  NetEnergyOutOfOcean(i,j),
     &                  ResidualEnergyOutOfOcean(i,j),
     &                  QNET(I,J,bi,bj)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice QSW QSWO QSWI      ',
     &                  QSW(i,j,bi,bj),
     &                  QSWO(i,j),
     &                  QSWI(i,j)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice SW(BML IML SW)     ',
     &                  QSW_absorb_below_first_layer(i,j),
     &                  QSW_absorb_in_first_layer(i,j),
     &                  SWFRACB

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice ptc(to, qsw, oa)   ',
     &                  PredTempChange(i,j),
     &                  PredTempChangeFromQSW (i,j),
     &                  PredTempChangeFromOA_MQNET(i,j)


                     print '(A,4(1x,1PE24.15))',
     &                  'ifice ptc(fion,ian,ia)   ',
     &                  PredTempChangeFromF_IO_NET(i,j),
     &                  PredTempChangeFromF_IA_NET(i,j),
     &                  PredTempChangeFromFIA(i,j)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice ptc(niv)           ',
     &                  PredTempChangeFromNewIceVol(i,j)


                     print '(A,4(1x,1PE24.15))',
     &                  'ifice EmPmR EVP PRE RU   ',
     &                  EmPmR(I,J,bi,bj),
     &                  EVAP(I,J,bi,bj),
     &                  PRECIP(I,J,bi,bj),
     &                  RUNOFF(I,J,bi,bj)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice PRROIS,SAOI        ',
     &                  PrecipRateOverIceSurfaceToSea(I,J),
     &                  SnowAccRateOverIce(I,J),
     &                  SnowAccOverIce(I,J)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice SM(PM PMS S MR)    ',
     &                  PotSnowMeltFromSurf(I,J),
     &                  PotSnowMeltRateFromSurf(I,J),
     &                  SnowMeltFromSurface(I,J),
     &                  SnowMeltRateFromSurface(I,J)

                     print '(A,4(1x,1PE24.15))',
     &                  'ifice TotSnwMlt ExSnVC   ',
     &                  ActualNewTotalSnowMelt(I,J),
     &                  ExpectedSnowVolumeChange(I,J)


                     print '(A,4(1x,1PE24.15))',
     &                  'ifice fw(CFICE, CFSM)    ',
     &                  FreshwaterContribFromIce(I,J),
     &                  FreshwaterContribFromSnowMelt(I,J)

                     print '(A,2i5)',
     &                  'ifice i j -------------- ',i,j

                     print '(A,4(1x,1PE24.15))',
     &                  'end iter, ANm1, HNm1, HSN  ',myIter,
     &                  AREANm1(I,J,bi,bj), HEFFNm1(I,J,bi,bj),
     &                  HSNOW(I,J,bi,bj)

                  ENDIF
               ENDDO
            ENDDO
#endif /* SEAICE_DEBUG */
            
            
         ENDDO                  !/ bi
      ENDDO                     !/ bj
      
      RETURN
      END
