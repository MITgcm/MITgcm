C     $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth_if.F,v 1.10 2010/03/16 19:09:51 gforget Exp $
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

C     constants
      _RL TBC, SDF, ICE2SNOW,TMELT
      _RL STANTON_NUMBER, USTAR_BASE

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

C     The total shortwave heat flux (into open water and through the ice)
C     converging within the uppermost ocean grid cell (W/m^2)
      _RL QSW_absorb_in_first_layer     (1:sNx,1:sNy)

C     ( 1 - QSW_absorb_in_first_layer)  (W/m^2) 
      _RL QSW_absorb_below_first_layer  (1:sNx,1:sNy)

C     The actual ice thickness (i.e., mean ice thickness / ice concentration) (m) 
      _RL HICE_ACTUAL                   (1:sNx,1:sNy)

C     The actual snow thickness (i.e., mean snow thickness / ice concentration) (m) 
      _RL HSNOW_ACTUAL                  (1:sNx,1:sNy)

C     wind speed (m/s)
      _RL UG                            (1:sNx,1:sNy)

      _RL SPEED_SQ
      _RL RHOFW,CPW,LI,QI,QS,RHOSW

C     Helper Variables for snow flooding
      _RL EnergyToMeltSnowAndIce        (1:sNx,1:sNy)
      _RL EnergyToMeltSnowAndIce2       (1:sNx,1:sNy)
      _RL FL_C1,FL_C2,FL_C3,FL_C4,deltaHS,deltaHI


#ifdef SEAICE_USE_ORIGINAL_HEAT_FLUX
      _RL GAMMA
#endif

C     Factor by which we increase the u* (friction velocity) when ice is not 
C     present (dimensionless)
      _RL MixedLayerTurbulenceFactor

C     Sea ice growth rates (m/s)
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
 
c      Variables for debugging (predicted temperature change from various sources)
      _RL PredTempChange                (1:sNx,1:sNy)
      _RL PredTempChangeFromQSW         (1:sNx,1:sNy)
      _RL PredTempChangeFromOA_MQNET    (1:sNx,1:sNy)
      _RL PredTempChangeFromFIA         (1:sNx,1:sNy)
      _RL PredTempChangeFromNewIceVol   (1:sNx,1:sNy)
      _RL PredTempChangeFromF_IA_NET    (1:sNx,1:sNy)
      _RL PredTempChangeFromF_IO_NET    (1:sNx,1:sNy)

      _RL ExpectedIceVolumeChange       (1:sNx,1:sNy)
      _RL ExpectedSnowVolumeChange      (1:sNx,1:sNy)
      _RL ActualNewTotalVolumeChange    (1:sNx,1:sNy)
      _RL ActualNewTotalSnowMelt        (1:sNx,1:sNy)

      _RL EnergyInNewTotalIceVolume     (1:sNx,1:sNy)
      _RL NetEnergyFluxOutOfOcean       (1:sNx,1:sNy)

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
C                  melt, or flooding (m)
      _RL HSNOW_ORIG                    (1:sNx,1:sNy)

C     F_ia  - sea ice/snow surface heat flux with atmosphere (W/m^2)
C       F_ia > 0, heat loss to atmosphere
C       F_ia < 0, atmospheric heat flux convergence (ice/snow surface melt)
      _RL F_ia                          (1:sNx,1:sNy)

C     F_ia_net - the net heat flux divergence at the sea ice/snow surface
C                including sea ice conductive fluxes and atmospheric fluxes (W/m^2)
C       F_ia_net = 0, sea ice/snow surface energy balance condition met
C                     upward conductive fluxes balance surface heat loss
C       F_ia_net < 0, net heat flux convergence at ice/snow surface 
C                     zero conductive fluxes and net atmospheric convergence
      _RL F_ia_net                      (1:sNx,1:sNy)

C     F_ia_net - the net heat flux divergence at the sea ice/snow surface 
C                before snow is melted with any convergence (W/m^2)
C        F_ia_net < 0, some snow, if present, will melt 
      _RL F_ia_net_before_snow          (1:sNx,1:sNy)

C     F_io_net - the net upward conductive heat flux through the ice+snow system
C                realized at the sea ice/snow surface
C        F_io_net > 0, heat conducting upward from ice base --> basal thickening
C        F_io_net = 0, no upward heat conduction 
C                      ice/snow surface temperature > SEAICE_freeze)  
      _RL F_io_net                      (1:sNx,1:sNy)

C     F_ao  - heat flux from atmosphere to ocean (W/m^2)
C        F_ao > 0
      _RL F_ao                          (1:sNx,1:sNy)

C     F_mi - heat flux from ocean to the ice (W/m^2)
      _RL F_mi                          (1:sNx,1:sNy)
 

c     The ocean temperature used for the calculation of turbulent ocean-ice heat fluxes
      _RL surf_theta

C     Helper variables for debugging
      _RL FLUX_TO_DELTA_TEMP,ENERGY_TO_DELTA_TEMP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
         kSurface        = Nr 
      else
         kSurface        = 1
      endif

c    For debugging
      FLUX_TO_DELTA_TEMP = SEAICE_deltaTtherm*
     &            recip_Cp*recip_rhoConst * recip_drF(1)

      ENERGY_TO_DELTA_TEMP = recip_Cp*recip_rhoConst*recip_drF(1)

C     TBC : The freezing point of seawater (deg C)
      TBC          = SEAICE_freeze

C     TMELT : The freezing point of fresh water (deg K)
      TMELT = 273.15 _d 0 

c     A reference seawater density (kg m^-3)
      RHOSW = 1026. _d 0

c     Freshwater density (kg m^-3)
      RHOFW = rhoConstFresh

c     The Stanton number for the McPhee 
c     ocean-ice heat flux parameterization (dimensionless)
      STANTON_NUMBER = 0.0056 _d 0 

c     USTAR_BASE : A typical friction velocity beneath sea 
c                  ice for the McPhee heat flux parameterization (m/s)
      USTAR_BASE = 0.0125 _d 0

C     CPW : Seawater heat capacity   (J m^-3 K^-1)
      CPW = 4010. _d 0

c     Li : Ice latent heat of fusion (J kg^-1)
      Li = 3.34 _d 5 

c     Factors used to to map between energy fluxes and snow/ice melt or growth
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
c     To keep this timescale up to date given advances in
c     the parameterization, GAMMA is specified as follows, 
c     
      GAMMA =  10. _d 0/SEAICE_gamma_t

c     n.b., changed from drF(1)/SEAICE_gamma_t to make 
c     a more robust form in setups with a higher vertical resolution.
#endif

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
     &           + act3*max1*max2
     &           + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */
C     
C     initialise a few fields
C     
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_SIMULATE_CONVERGENCE_IN_1D_MODEL
c           simulate total convergence from dynamics
            DO J=1,sNy
               DO I=1,sNx
                  IF (HEFF(I,J,bi,bj) .GT. ZERO) THEN
                     AREA(I,J,bi,bj) = ONE
                  ENDIF
               ENDDO
            ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
c
            DO J=1,sNy
               DO I=1,sNx
c                 Bound area, heff, and hsnow 
                  AREA(I,J,bi,bj) = MIN(ONE,AREA(I,J,bi,bj))
                  AREA(I,J,bi,bj) = MAX(ZERO,AREA(I,J,bi,bj))
                  HEFF(I,J,bi,bj) = MAX(ZERO, HEFF(I,J,bi,bj))
                  HSNOW(I,J,bi,bj)  = MAX(ZERO, HSNOW(I,J,bi,bj))

c                 Sanity checks 
                  IF (HEFF(I,J,bi,bj) .LE. ZERO .OR.
     &                AREA(I,J,bi,bj) .LE. ZERO) THEN
                    
                    AREA(I,J,bi,bj)       = 0. _d 0
                    HEFF(I,J,bi,bj)       = 0. _d 0
                    HSNOW(I,J,bi,bj)      = 0. _d 0
                  ENDIF
               ENDDO
            ENDDO


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

                  PredTempChange                  (I,J) = 0. _d 0
                  PredTempChangeFromQSW           (I,J) = 0. _d 0
                  PredTempChangeFromOA_MQNET      (I,J) = 0. _d 0
                  PredTempChangeFromFIA           (I,J) = 0. _d 0
                  PredTempChangeFromF_IA_NET      (I,J) = 0. _d 0
                  PredTempChangeFromF_IO_NET      (I,J) = 0. _d 0
                  PredTempChangeFromNewIceVol     (I,J) = 0. _d 0

                  IceGrowthRateOpenWater          (I,J) = 0. _d 0
                  IceGrowthRateMixedLayer         (I,J) = 0. _d 0

#ifdef ALLOW_SALT_PLUME
                  IceGrowthRateInLeads            (I,J) = 0. _d 0
                  leadPlumeFraction               (I,J) = 0. _d 0
                  saltPlumeFlux             (I,J,bi,bj) = 0. _d 0
#endif

                  ExpectedIceVolumeChange         (I,J) = 0. _d 0
                  ExpectedSnowVolumeChange        (I,J) = 0. _d 0
                  ActualNewTotalVolumeChange      (I,J) = 0. _d 0
                  ActualNewTotalSnowMelt          (I,J) = 0. _d 0

                  EnergyInNewTotalIceVolume       (I,J) = 0. _d 0
                  NetEnergyFluxOutOfOcean         (I,J) = 0. _d 0
                  ResidualEnergyOutOfOcean          (I,J) = 0. _d 0
                  QSW_absorb_in_first_layer       (I,J) = 0. _d 0
                  QSW_absorb_below_first_layer    (I,J) = 0. _d 0

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

c the post sea ice advection and diffusion ice state are in time level 1.  
c move these to the time level 2 before thermo.  after this routine 
c the updated ice state will be in time level 1 again. (except for snow 
c which does not have 3 time levels for some reason) 
                  HEFFNm1(I,J,bi,bj) = HEFF(I,J,bi,bj) 
                  AREANm1(I,J,bi,bj) = AREA(I,J,bi,bj) 

#ifdef SEAICE_SIMULATE_DIVERGENCE_IN_1D_MODEL
c Sometimes it's nice to run a 1-D setup but how then to simulate
c the effect of mechanical divergence?.  To simulate the effect of 
c having regular lead openings, each time step remove a fraction
c of the ice area while maintaining the actual thickness of ice
c and snow.

                  IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN
                     HICE_ACTUAL(I,J)  = 
     &                  HEFFNm1(I,J,bi,bj)/AREANm1(I,J,bi,bj)

                     HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/
     &                  AREANm1(I,J,bi,bj)

c                    Removes about 1.2% of the concentration in 48 hours.
                     AREANm1(I,J,bi,bj) = AREANm1(I,J,bi,bj)*0.99975 _d 0

c                    Maintain the same snow and ice thickness
                     HEFFNm1(I,J,bi,bj) = 
     &                     HICE_ACTUAL(I,J)*AREANm1(I,J,bi,bj)

                     HSNOW(I,J,bi,bj) = 
     &                     HSNOW_ACTUAL(I,J)*AREANm1(I,J,bi,bj)

                   ENDIF
#endif            

               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE tice(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE precip(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            DO J=1,sNy
               DO I=1,sNx
C WE HAVE TO BE CAREFUL HERE SINCE ADVECTION/DIFFUSION COULD HAVE 
C MAKE EITHER (BUT NOT BOTH) HEFF OR AREA ZERO OR NEGATIVE 
C HSNOW COULD ALSO BECOME NEGATIVE 
                  HEFFNm1(I,J,bi,bj) = MAX(ZERO,HEFFNm1(I,J,bi,bj)) 
                  HSNOW(I,J,bi,bj)   = MAX(ZERO,HSNOW(I,J,bi,bj)  ) 
                  AREANm1(I,J,bi,bj) = MAX(ZERO,AREANm1(I,J,bi,bj)) 
cif this is hack to prevent negative precip.  somehow negative precips  
cif escapes my exf_checkrange hack
cph-checkthis
                  IF (PRECIP(I,J,bi,bj) .LT. ZERO) THEN 
                     PRECIP(I,J,bi,bj) = ZERO 
                  ENDIF 
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE precip(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif
            DO J=1,sNy
               DO I=1,sNx

                  IF (HEFFNm1(I,J,bi,bj) .LE. ZERO) THEN 
                      AREANm1(I,J,bi,bj) = 0. _d 0
                      HSNOW(I,J,bi,bj)   = 0. _d 0
                  ENDIF 
 
                  IF (AREANm1(I,J,bi,bj) .LE. ZERO) THEN 
                     HEFFNm1(I,J,bi,bj)  = 0. _d 0
                     HSNOW(I,J,bi,bj)    = 0. _d 0
                  ENDIF 
 
C PROCEED ONLY IF WE ARE CERTAIN TO HAVE ICE (AREA > 0) 

                  IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN
                     HICE_ACTUAL(I,J)  = 
     &                  HEFFNm1(I,J,bi,bj)/AREANm1(I,J,bi,bj)

                     HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/
     &                  AREANm1(I,J,bi,bj)

c                   ACCUMULATE SNOW
c                   Is the ice/surface below freezing or at the freezing 
c                   point (melting).  If it is freezing the precip is 
c                   felt as snow and will accumulate over the ice. Else,
c                   precip makes its way, like all things in time, to the sea.
                    IF (TICE(I,J,bi,bj) .LT. TMELT) THEN
c                     Snow falls onto freezing surface remaining as snow
                      SnowAccRateOverIce(I,J) = 
     &                  PRECIP(I,J,bi,bj)*RHOFW/SEAICE_rhoSnow

c                     None of the precipitation falls into the sea 
                      PrecipRateOverIceSurfaceToSea(I,J) = 0. _d 0
  
                    ELSE 
c                     The snow melts on impact is is considered 
c                     nothing more than rain.  Since meltponds are
c                     not explicitly represented,this rain runs
c                     immediately into the sea

                      SnowAccRateOverIce(I,J) = 0. _d 0
C                     The rate of rainfall over melting ice.
                      PrecipRateOverIceSurfaceToSea(I,J)=
     &                  PRECIP(I,J,bi,bj)
                   ENDIF

c                  In m of mean snow thickness.
                   SnowAccOverIce(I,J) = 
     &                  SnowAccRateOverIce(I,J)
     &                  *SEAICE_deltaTtherm*AreaNm1(I,J,bi,bj)

                ELSE
                   HEFFNm1(I,J,bi,bj) = 0. _d 0
                   HICE_ACTUAL(I,J)   = 0. _d 0
                   HSNOW_ACTUAL(I,J)  = 0. _d 0
                   HSNOW(I,J,bi,bj)   = 0. _d 0
                ENDIF
                HSNOW_ORIG(I,J) = HSNOW(I,J,bi,bj)
             ENDDO
          ENDDO
            
C     FIND ATM. WIND SPEED
        DO J=1,sNy
         DO I=1,sNx
C         copy the wind speed computed in exf_wind.F to UG
          UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics
cphCADJ STORE uwind  = comlev1, key = ikey_dynamics
cphCADJ STORE vwind  = comlev1, key = ikey_dynamics
CADJ STORE tice   = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

C           SET LAYER TEMPERATURE IN KELVIN
            DO J=1,sNy
               DO I=1,sNx
                  TMIX(I,J,bi,bj)=
     &                 theta(I,J,kSurface,bi,bj) + TMELT
               ENDDO
            ENDDO

C           CALCULATE THE THERMODYNAMIC FLUXES WITHIN THE SNOW/ICE SYSTEM
            CALL SEAICE_BUDGET_ICE_IF(
     I           UG, HICE_ACTUAL, HSNOW_ACTUAL, 
     U           TICE, 
     O           F_io_net,F_ia_net,F_ia, QSWI, 
     I           bi, bj, myThid)

C Sometimes it is nice to have a setup without ice-atmosphere heat
C fluxes.  This flag turns those fluxes to zero but leaves the 
C Ice ocean fluxes intact.  Thus, the first oceanic cell can transfer
C heat to the ice leading to melting in F_ml and it can release 
C heat to the atmosphere through leads and open area thus growing it in
C F_ao

#ifdef FORBID_ICE_SURFACE_ATMOSPHERE_HEAT_FLUXES
            DO J=1,sNy
               DO I=1,sNx
                  F_ia_net (I,J)  = 0. _d 0
                  F_ia (I,J)      = 0. _d 0
                  F_io_net(I,J)   = 0. _d 0
                  QSWI(I,J)       = 0. _d 0
               ENDDO
            ENDDO
#endif

            DO J=1,sNy
               DO I=1,sNx

#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &              (J .EQ. SEAICE_debugPointY) ) THEN

                 print *,'sig: I,J,F_ia,F_ia_net',I,J,F_ia(I,J),
     &              F_ia_net(I,J)

               ENDIF
#endif

c                 If there is heat flux convergence at the snow surface,
c                 use that energy to melt snow before melting ice.  It's
c                 possible that some snow will remain after melting, 
c                 which will drive F_ia_net to zero, or that all of the 
c                 snow will be melted, leaving a nonzero F_ia_net to melt
c                 some ice.  
                  F_ia_net_before_snow(I,J) = F_ia_net(I,J)

c                 Only continue if there is snow and ice in the cell
                  IF (AreaNm1(I,J,bi,bj)*HEFFNm1(I,J,bi,bj)
     &                .LE. ZERO                            ) THEN
                     IceGrowthRateUnderExistingIce(I,J) = 0. _d 0
                     IceGrowthRateFromSurface(I,J)      = 0. _d 0
                     NetExistingIceGrowthRate(I,J)      = 0. _d 0
                  ELSE
c                    The growth rate (m/s) beneath existing ice is given by the upward
c                    ocean-ice conductive flux, F_io_net, and QI.
                     IceGrowthRateUnderExistingIce(I,J)=F_io_net(I,J)*QI

c                    The rate at which snow is melted (m/s) because of surface 
c                    heat flux convergence.  Note, during snow melt, F_ia_net must 
c                    be negative (implying convergence) to make PSMRFW is positive
                     PotSnowMeltRateFromSurf(I,J)= - F_ia_net(I,J)*QS

c                    This is the depth of snow (m) that would be melted over one time step.
                     PotSnowMeltFromSurf(I,J) =
     &                  PotSnowMeltRateFromSurf(I,J)* SEAICE_deltaTtherm

c                    If we can melt MORE than is actually there, then the melt 
c                    rate is reduced so that only that which is there 
c                    is melted during the time step.  In this case, not all of the 
c                    heat flux convergence at the surface is used to melt snow.
c                    Any remaining energy will melt ice.  

c                    SurfHeatFluxConvergToSnowMelt is the part of the total heat 
c                    flux convergence which melts snow.

                     IF (PotSnowMeltFromSurf(I,J) .GE. 
     &                   HSNOW_ACTUAL(I,J)) THEN

c                      Snow melt and melt rate (actual not mean snow thickness)
                       SnowMeltFromSurface(I,J)     = HSNOW_ACTUAL(I,J)

                       SnowMeltRateFromSurface(I,J) = 
     &                   SnowMeltFromSurface(I,J)/ SEAICE_deltaTtherm

                       SurfHeatFluxConvergToSnowMelt(I,J) =
     &                   -HSNOW_ACTUAL(I,J)/QS/SEAICE_deltaTtherm
                     ELSE
c                      In this case there will be snow remaining after melting.
c                      All of the surface heat convergence will be redirected to 
c                      this effort.
                       SnowMeltFromSurface(I,J)=PotSnowMeltFromSurf(I,J)

                       SnowMeltRateFromSurface(I,J) = 
     &                    PotSnowMeltRateFromSurf(I,J)

                       SurfHeatFluxConvergToSnowMelt(I,J) =F_ia_net(I,J)
                     ENDIF

c                    Reduce the heat flux convergence available to melt surface
c                    ice by the amount used to melt snow
                     F_ia_net(I,J) = 
     &                  F_ia_net(I,J)-SurfHeatFluxConvergToSnowMelt(I,J)

                     IceGrowthRateFromSurface(I,J) = F_ia_net(I,J)*QI

c                    The total growth rate (m/s) of the existing ice - the rate of
c                    new ice accretion at the base less the rate due to surface melt
                     NetExistingIceGrowthRate(I,J) = 
     &                 IceGrowthRateUnderExistingIce(I,J) +
     &                 IceGrowthRateFromSurface(I,J)
                  ENDIF
               ENDDO
            ENDDO

c           Retrieve the air-sea heat and shortwave radiative fluxes
            CALL SEAICE_BUDGET_OCEAN(
     I           UG, 
     U           TMIX, 
     O           F_ao, QSWO, 
     I           bi, bj, myTime, myIter, myThid )
 
#ifdef SEAICE_DEBUG
        print *,'myiter', myIter
        print '(A,2i4,2(1x,1P2E15.3))',
     &       'ifice sigr, dbgx,dby, (netHF, SWHeatFlux)',
     &        SEAICE_debugPointX,   SEAICE_debugPointY,
     &        F_ao(SEAICE_debugPointX, SEAICE_debugPointY),
     &        QSWO(SEAICE_debugPointX, SEAICE_debugPointY)
#endif


c--   Not all of the sw radiation is absorbed in the uppermost ocean grid cell layer.
c     Only that fraction which converges in the uppermost ocean grid cell is used to
c     melt ice.
c           SWFRACB - the fraction of incoming sw radiation absorbed in the 
c                     uppermost ocean grid cell (calculated in seaice_init_vari.F)
            DO J=1,sNy
               DO I=1,sNx

c The contribution of shortwave heating is 
c not included without #define SHORTWAVE_HEATING
#ifdef SHORTWAVE_HEATING
                  QSWO_BELOW_FIRST_LAYER(i,j)= QSWO(I,J)*SWFRACB
                  QSWO_IN_FIRST_LAYER(I,J)   = QSWO(I,J)*(ONE - SWFRACB)
#else 
                  QSWO_BELOW_FIRST_LAYER(i,j)= 0. _d 0
                  QSWO_IN_FIRST_LAYER(I,J)   = 0. _d 0
#endif
                  IceGrowthRateOpenWater(I,J)= QI*
     &              (F_ao(I,J) - QSWO(I,J) + QSWO_IN_FIRST_LAYER(I,J))

             ENDDO
            ENDDO        
            

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)= comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C--   Calcuate the heat fluxes from the ocean to the sea ice
            DO J=1,sNy
               DO I=1,sNx

C     Calculate the seawater freezing point
#ifdef SEAICE_VARIABLE_FREEZING_POINT
                  TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 
     &                 0.0901 _d 0
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
c          Where,
c             rho_sw : seawater density (kg m^-3)
c             cp_sw  : seawater heat capcity (J kg^-1 K^-1)
c             S_t    : Stanton Number ~ 0.0056 (dimensionless)
c             u_star : friction velocity beneath ice ~ 0.015 m s^-1
c             T_o, T_f : The 'far-field' ocean temperature and ice
c                        freezing point (deg C)
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
c      in the following way:
c
c     Flux  = rho_sw * cp_sw * GAMMA 
c     GAMMA = dRf(1)/SEAICE_gamma_t * (T_o - T_f)
c    
c           Where,
c               dRf(1)  : depth of surface level grid cell (originally 
c                         assumed to be 10 m)
c               SEAICE_gamma_t : an empirical parameter (~ 3 days in seconds)
c
c     Using typical values, ice advected into a grid cell 
c     that is warmer than the sea ice freezing point by 1 degree
c     will be subjected to a basal heat flux of ~ 160 W m^-2.   Therefore,
c     as written, one should choose a SEAICE_gamma_t = 1.4 days in seconds
c     to have flux magnitudes match those of the McPhee parameterization.
c    
c     The original scheme is accessed by defining  #SEAICE_USE_ORIGINAL_HEAT FLUX

#ifndef SEAICE_USE_ORIGINAL_HEAT_FLUX
c                 If ice is present, MixedLayerTurbulenceFactor = 1.0, else 12.50
                  IF (AREANm1(I,J,bi,bj) .GT. 0. _d 0) THEN
                     MixedLayerTurbulenceFactor = ONE
                  ELSE
                     MixedLayerTurbulenceFactor = 12.5 _d 0
                  ENDIF

                  F_mi(I,J) = -STANTON_NUMBER * USTAR_BASE * RHOSW *
     &                CPW *(surf_theta - TBC)*MixedLayerTurbulenceFactor
#else
c                 no turbulence factor using original_heat_flux scheme
                  F_mi(I,J) = -GAMMA*RHOSW*CPW *(surf_theta - TBC)

#endif /* SEAICE_USE_ORIGINAL_HEAT_FLUX */

                  IceGrowthRateMixedLayer(I,J) = F_mi(I,J)*QI;
               ENDDO
            ENDDO 

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE S_h(:,:)         = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */



C           CALCULATE THICKNESS DERIVATIVE (S_h)
            DO J=1,sNy
               DO I=1,sNx
                  S_h(I,J) = 
     &                 NetExistingIceGrowthRate(I,J)*AREANm1(I,J,bi,bj)+
     &                 (ONE -AREANm1(I,J,bi,bj))*
     &                 IceGrowthRateOpenWater(I,J) +
     &                 IceGrowthRateMixedLayer(I,J)

c                  Both the accumulation and melt rates are in terms
c                  of actual snow thickness.  As with ice, multiplying
c                  with area converts to mean snow thickness.
                   S_hsnow(I,J) =     AREANm1(I,J,bi,bj)* (
     &                  SnowAccRateOverIce(I,J) - 
     &                  SnowMeltRateFromSurface(I,J)     ) 
               ENDDO
            ENDDO

#ifdef ALLOW_SALT_PLUME
C       Now that we know the thickness tendency terms, we can calculate the saltPlumeFlux
        DO J=1,sNy
         DO I=1,sNx

c            It is assumed that ice production in leads can generate
c            salt plumes.  The fraction of the salt sent to the plume package
c            from ice produced in leads (defined as the open water
c            fraction) is a nonlinear function of ice concentration, AREA.
c
c            Specifically, function is a logistic curve (sigmoid) with a range and 
c            domain {0,1}.  The function, f(AREA), has a single free parameter,
c            SEAICE_plumeInflectionPoint, the inflection point of the curve.  
c            By construction, the function has the following properties:
c            f(1) \approx 1.0
c            f(SEAICE_plumeInflectionPoint) = 0.5
c            f(0) \approx 0.0 (when SEAICE_plumeInflectionPoint \geq 0.5)
c            f(0) > 0.0 (when SEAICE_plumeInflectionPoint < 0.5)
c
c            As AREA --> 1, the open water fraction occurs
c            in narrow leads, new ice production become spatially non-uniform,
c            and the assumptions motivating KPP no longer hold.  To treat
c            overturning in a more physically realistic way, the salt produced
c            in the leads should be sent to depth via the plume package.  To assure 
c            only narrow leads generate plumes, choose a SEAICE_plumeInflectionPoint 
c            of > 0.8.

c            Ensure that there is already ice present or that the total ice
c            ice tendency term is positive.  We don't want to release 
c            salt if sea ice is not established in the cell.

             IF ((AREANm1(I,J,bi,bj) .GT. ZERO) .OR. 
     &           (S_h(I,J)           .GT. ZERO)) THEN
 
              leadPlumeFraction(I,J) = 
     &         (ONE + EXP( ( SEAICE_plumeInflectionPoint - 
     &                       AREANm1(I,J,bi,bj)
     &                     ) * 5._d 0/
     &                     (ONE - SEAICE_plumeInflectionPoint)
     &                   )
     &         )**(-ONE) 
 
c             Only consider positive ice growth rate in leads for salt production
              IceGrowthRateInLeads(I,J) = max( ZERO,
     &         (ONE - AREANm1(I,J,bi,bj)) * IceGrowthRateOpenWater(I,J))

              saltPlumeFlux(I,J,bi,bj) = leadPlumeFraction(I,J) * 
     &            HEFFM(I,J,bi,bj)*IceGrowthRateInLeads(I,J)*
     &            ICE2WATR*rhoConstFresh*
     &            (salt(I,J,kSurface,bi,bj) - SEAICE_salinity_fixed)
             ELSE 

              saltPlumeFlux(I,J,bi,bj) = ZERO 

             ENDIF

         ENDDO
        ENDDO
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE S_h(:,:)         = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE S_hsnow(:,:)     = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */



c           Caculate dA/dt (S_a)
            DO J=1,sNy
               DO I=1,sNx
                  S_a(I,J) =  0. _d 0 

c                 In the following, time derivatives of AREA require
c                 divisions by HEFF. To prevent sensitivity blow up when
c                 HEFF --> 0, we divide by an approximation to HEFF, HEFF*:
c
c                 HEFF* = sqrt(HEFF^2 + heff_min^2)
c                      Where heff_min is implicitly defined as 0.1 m. 
c
c                 HEFF* is defined as a hyperbola with slope = 1 and 
c                 y-intercept of heff_min.  The gradient of 1/HEFF* is 
c                 well-behaved (correct sign) and deviates from 1/HEFF 
c                 significantly only when HEFF --> 0.

                 IF ( (HEFFNm1(I,J,bi,bj).GT. ZERO) .AND.
     &                (AREANm1(I,J,bi,bj).NE. ZERO)) THEN
                     HEFF_MIN = SQRT( 0.01 _d 0 + 
     &                   HEFFNm1(I,J,bi,bj) * HEFFNm1(I,J,bi,bj) )
                 ELSE
c                        Just in case somehow we miss a case, we want to divide
c                        by a sensible number, here 10 cm.
                         HEFF_MIN = max(HEFFNm1(I,J,bi,bj), 0.1 _d 0)
                 ENDIF

c                 Caculate the ice area growth rate from the open water fluxes.
c                 First, determine whether the open water growth rate is positive or
c                 negative.  If positive, make sure that ice is present or that the 
c                 net ice thickness growth rate is positive before extending ice cover 

                  IF (IceGrowthRateOpenWater(I,J) .GT. ZERO) THEN

c                    Determine which hemisphere for hemisphere-dependent
c                    "lead closing variable", HO

                     IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
                        S_a_from_IGROW(I,J) = (ONE-AREANm1(I,J,bi,bj))*
     &                     IceGrowthRateOpenWater(I,J)/HO_south
                     ELSE
                        S_a_from_IGROW(I,J) = (ONE-AREANm1(I,J,bi,bj))*
     &                     IceGrowthRateOpenWater(I,J)/HO
                     ENDIF

c                    If there is already ice, add to S_a
                     IF (AREANm1(I,J,bi,bj) .GT. ZERO) THEN
                        S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)
c                    otherwise, add to S_a only if the net ice growth rate
c                    is positive
                     ELSE
                        IF (S_h(I,J) .GT. ZERO) THEN
                           S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)
                        ENDIF
                     ENDIF
                  ELSE  

c                    The open water growth rate is negative - contract the ice cover.
                     IF ( (AREANm1(I,J,bi,bj).GT. ZERO) .AND.
     &                  (HEFFNm1(I,J,bi,bj).GT. ZERO) ) THEN

                       S_a(I,J) = S_a(I,J) 
     &                    + AREANm1(I,J,bi,bj)/(2. _d 0*HEFF_MIN)*
     &                    IceGrowthRateOpenWater(I,J)*
     &                    (ONE - AREANm1(I,J,bi,bj))
                     ELSE
                       S_a(I,J) = S_a(I,J) +  0. _d 0
                     ENDIF
                  ENDIF

C                 Melt ice if the IceGrowthRateMixedLayer is negative
                  IF ( (IceGrowthRateMixedLayer(I,J) .LE. ZERO) .AND. 
     &                 (AREANm1(I,J,bi,bj).GT. ZERO) .AND.
     &                 (HEFFNm1(I,J,bi,bj).NE. ZERO) ) THEN

                     S_a(I,J) = S_a(I,J) 
     &                    + AREANm1(I,J,bi,bj)/(2. _d 0 *HEFF_MIN)*
     &                    IceGrowthRateMixedLayer(I,J)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0. _d 0
                  ENDIF

C                 Melt ice if the NetExistingIceGrowthRate is negative
                  IF ( (NetExistingIceGrowthRate(I,J) .LE. ZERO) .AND. 
     &                 (AREANm1(I,J,bi,bj).GT. ZERO) .AND.
     &                 (HEFFNm1(I,J,bi,bj).NE. ZERO) ) THEN

                     S_a(I,J) = S_a(I,J) 
     &                   + AREANm1(I,J,bi,bj)/(2. _d 0 * HEFF_MIN)*
     &                  NetExistingIceGrowthRate(I,J)*AREANm1(I,J,bi,bj)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0. _d 0
                  ENDIF
                  
               ENDDO
            ENDDO
                 

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE S_a(:,:)          = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE S_h(:,:)          = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE f_ao(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qswi(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qswo(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif

C           Update the area, heff, and hsnow 
            DO J=1,sNy
               DO I=1,sNx
                  AREA(I,J,bi,bj) = AREANm1(I,J,bi,bj) + 
     &                 SEAICE_deltaTtherm * S_a(I,J)
                  HEFF(I,J,bi,bj) = HEFFNm1(I,J,bi,bj) +
     &                 SEAICE_deltaTTherm * S_h(I,J)
                  HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) +
     &                 SEAICE_deltaTTherm * S_hsnow(I,J)
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif

            DO J=1,sNy
               DO I=1,sNx
c                 Bound area, heff, and hsnow 
                  AREA(I,J,bi,bj) = MIN(ONE,AREA(I,J,bi,bj))
                  AREA(I,J,bi,bj) = MAX(ZERO,AREA(I,J,bi,bj))
                  HEFF(I,J,bi,bj) = MAX(ZERO, HEFF(I,J,bi,bj))
                  HSNOW(I,J,bi,bj)  = MAX(ZERO, HSNOW(I,J,bi,bj))

c                 Sanity checks 
                  IF (HEFF(I,J,bi,bj) .LE. ZERO .OR.
     &                AREA(I,J,bi,bj) .LE. ZERO) THEN
                    
                    AREA(I,J,bi,bj)       = 0. _d 0
                    HEFF(I,J,bi,bj)       = 0. _d 0
                    HICE_ACTUAL(I,J)      = 0. _d 0
                    HSNOW(I,J,bi,bj)      = 0. _d 0
                    HSNOW_ACTUAL(I,J)     = 0. _d 0

                  ELSE
c                 Calcuate the actual ice and snow thicknesses
                    HICE_ACTUAL(I,J)  =  HEFF(I,J,bi,bj)/AREA(I,J,bi,bj)
                    HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/AREA(I,J,bi,bj)
                  ENDIF

               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            DO J=1,sNy
               DO I=1,sNx

c                 The amount of new mean ice thickness we expect to grow
                  ExpectedIceVolumeChange(I,J)  = S_h(I,J) *
     &                 SEAICE_deltaTtherm

c                 The amount of new mean snow thickness we expect to grow
                  ExpectedSnowVolumeChange(I,J) = S_hsnow(I,J)*
     &                 SEAICE_deltaTtherm

c                 THE EFFECTIVE SHORTWAVE HEATING RATE
#ifdef SHORTWAVE_HEATING
                  QSW(I,J,bi,bj)  =
     &                 QSWI(I,J)  * (      AREANm1(I,J,bi,bj)) +
     &                 QSWO(I,J)  * (ONE - AREANm1(I,J,bi,bj))
#else
                  QSW(I,J,bi,bj) = 0. _d 0
#endif

c                 The actual ice volume change over the time step
                  ActualNewTotalVolumeChange(I,J) = 
     &                 HEFF(I,J,bi,bj) - HEFFNm1(I,J,bi,bj)

c     The net average snow thickness melt that is actually realized. e.g.
c     hsnow_orig  = 0.25 m (e.g. 1 m of ice over a cell 1/4 covered in snow)
c     hsnow_new   = 0.20 m
c     snow accum  = 0.05 m
c            melt = 0.25 + 0.05 - 0.2 = 0.1 m 

c     since this is in mean snow thickness it might have been  0.4 of actual 
c     snow thickness over the 1/4 of the cell which is ice covered.
                  ActualNewTotalSnowMelt(I,J) = 
     &                 HSNOW_ORIG(I,J) +
     &                 SnowAccOverIce(I,J) -
     &                 HSNOW(I,J,bi,bj)

c                 The energy required to melt or form the new ice volume
                  EnergyInNewTotalIceVolume(I,J) = 
     &                 ActualNewTotalVolumeChange(I,J)/QI

c     This is the net energy flux out of the ice+ocean system
c     Remember -----
c     F_ia_net : Under ice/snow surface freezing conditions, 
c                vertical conductive heat flux convergence (F_c < 0) balances
c                heat flux divergence to atmosphere (F_ia > 0)  
c                Otherwise, F_ia_net = F_ia (pos)
c
c     F_io_net : Under ice/snow surface freezing conditions, F_c < 0.
c                Under ice surface melting conditions, F_c = 0 (no energy flux 
c                from the ice to ocean)
c
c     So if we are freezing, F_io_net = the conductive flux and there 
c     is energy balance at ice surface, F_ia_net =0.  If we are melting,
c     there is a convergence of energy into the ice from above
                  NetEnergyFluxOutOfOcean(I,J) = SEAICE_deltaTtherm *
     &               (AREANm1(I,J,bi,bj) * 
     &               (F_ia_net(I,J) + F_io_net(I,J) + QSWI(I,J))
     &         +     (ONE - AREANm1(I,J,bi,bj)) *  F_ao(I,J))

c     THE QUANTITY OF HEAT WHICH IS THE RESIDUAL TO THE QUANTITY OF 
c     ML temperature.  If the net energy flux is exactly balanced by the 
c     latent energy of fusion in the new ice created then we will not 
c     change the ML temperature at all.

                  ResidualEnergyOutOfOcean(I,J) = 
     &             NetEnergyFluxOutOfOcean(I,J) -
     &             EnergyInNewTotalIceVolume(I,J)

C     NOW FORMULATE QNET, which time LEVEL, ORIG 2.
C     THIS QNET WILL DETERMINE THE TEMPERATURE CHANGE OF THE MIXED LAYER
C     QNET IS A DEPTH AVERAGED HEAT FLUX FOR THE OCEAN COLUMN
C     BECAUSE OF THE 
                  QNET(I,J,bi,bj) =
     &             ResidualEnergyOutOfOcean(I,J) / SEAICE_deltaTtherm


c    Like snow melt, if there is melting, this quantity is positive.
c    The change of freshwater content is per unit area over the entire
c    cell, not just over the ice covered bits.  This term is only used
c    to calculate freshwater fluxes for the purpose of changing the
c    salinity of the liquid cell.  In the case of non-zero ice salinity,
c    the amount of freshwater is reduced by the ratio of ice salinity
c    to water cell salinity.
           IF  ( (salt(I,J,kSurface,bi,bj) .GT. ZERO) .AND.
     &           (salt(I,J,kSurface,bi,bj) .GE. 
     &                         SEAICE_salinity_fixed))  THEN 

                 FreshwaterContribFromIce(I,J) =
     &            - ActualNewTotalVolumeChange(I,J)*SEAICE_rhoICE/RHOFW*
     &            (ONE - SEAICE_salinity_fixed/salt(I,J,kSurface,bi,bj))

           ELSE
C    If the liquid cell has a lower salinity than the specified
c    salinity of sea ice then assume the sea ice is completely fresh
                 FreshwaterContribFromIce(I,J) =
     &             -ActualNewTotalVolumeChange(I,J)*SEAICE_rhoIce/RHOFW
           ENDIF


c    The freshwater contribution from snow comes only in the form of melt
c    unlike ice, which takes freshwater upon growth and yields freshwater
c    upon melt.  This is why the the actual new average snow melt was determined.
c    In m/m^2 over the entire cell.
                  FreshwaterContribFromSnowMelt(I,J) =
     &                 ActualNewTotalSnowMelt(I,J)*SEAICE_rhoSnow/RHOFW

c    This seems to be in m/s, original time level 2 for area
c    Only the precip and evap need to be area weighted.  The runoff
c    and freshwater contribs from ice and snow melt are already mean
c    weighted
                  EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &                 ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &                 * ( ONE - AREANm1(I,J,bi,bj) )
     &                 - PrecipRateOverIceSurfaceToSea(I,J)*
     &                     AREANm1(I,J,bi,bj)
#ifdef ALLOW_RUNOFF
     &                 - RUNOFF(I,J,bi,bj)
#endif
     &                 - (FreshwaterContribFromIce(I,J) +
     &                    FreshwaterContribFromSnowMelt(I,J))/
     &                    SEAICE_deltaTtherm )*rhoConstFresh

C                 DO SOME DEBUGGING CALCULATIONS.  MAKE SURE SUMS ALL ADD UP.
#ifdef SEAICE_DEBUG

C                 Calcuate the shortwave flux absorbed in the upper ocean
c                 grid cell
#ifdef SHORTWAVE_HEATING
                  QSW_absorb_in_first_layer(I,J) = QSW(I,J,bi,bj)*
     &              (ONE - SWFRACB)
#else 

                  QSW_absorb_in_first_layer(I,J) = 0. _d 0
#endif

C                 Calcuate the sw flux beneath the upper ocean grid cell
                  QSW_absorb_below_first_layer(I,J) = 
     &                 QSW(I,J,bi,bj) -  QSW_absorb_in_first_layer(I,J)

                  PredTempChangeFromQSW(I,J) = 
     &             - QSW_absorb_in_first_layer(I,J) * FLUX_TO_DELTA_TEMP

                  PredTempChangeFromOA_MQNET(I,J) = 
     &            -(QNET(I,J,bi,bj)-QSWO(I,J))*(ONE -AREANm1(I,J,bi,bj))
     &             * FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IO_NET(I,J) = 
     &             -F_io_net(I,J)*AREANm1(I,J,bi,bj)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IA_NET(I,J) = 
     &             -F_ia_net(I,J)*AREANm1(I,J,bi,bj)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromNewIceVol(I,J) = 
     &              EnergyInNewTotalIceVolume(I,J)*ENERGY_TO_DELTA_TEMP

                  PredTempChange(I,J) = 
     &              PredTempChangeFromQSW(I,J) +
     &              PredTempChangeFromOA_MQNET(I,J) +
     &              PredTempChangeFromF_IO_NET(I,J) +
     &              PredTempChangeFromF_IA_NET(I,J) +
     &              PredTempChangeFromNewIceVol(I,J)
#endif    

               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            
            DO J=1,sNy
               DO I=1,sNx
                  AREA(I,J,bi,bj) = AREA(I,J,bi,bj)*HEFFM(I,J,bi,bj)
                  HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)*HEFFM(I,J,bi,bj)
                  HSNOW(I,J,bi,bj)  = HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ALLOW_SEAICE_FLOODING
           IF(SEAICEuseFlooding) THEN

            DO J = 1,sNy
               DO I = 1,sNx
                  EnergyToMeltSnowAndIce(I,J) = 
     &                 HEFF(I,J,bi,bj)/QI +
     &                 HSNOW(I,J,bi,bj)/QS
                  
                  deltaHS = FL_C2*( HSNOW_ACTUAL(I,J) -
     &                 HICE_ACTUAL(I,J)*FL_C3 )
                  
                  IF (deltaHS .GT. ZERO) THEN
                     deltaHI = FL_C4*deltaHS
                     
                     HICE_ACTUAL(I,J) = HICE_ACTUAL(I,J)  
     &                    + deltaHI

                     HSNOW_ACTUAL(I,J)= HSNOW_ACTUAL(I,J) 
     &                    - deltaHS

                     HEFF(I,J,bi,bj)= HICE_ACTUAL(I,J) *
     &                    AREA(I,J,bi,bj)

                     HSNOW(I,J,bi,bj) = HSNOW_ACTUAL(I,J)*
     &                    AREA(I,J,bi,bj)
                     
                     EnergyToMeltSnowAndIce2(I,J) = 
     &                    HEFF(I,J,bi,bj)/QI +
     &                    HSNOW(I,J,bi,bj)/QS
                     
#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &              (J .EQ. SEAICE_debugPointY) ) THEN

                     print *,'Energy to melt snow+ice: pre,post,delta', 
     &                    EnergyToMeltSnowAndIce(I,J),  
     &                    EnergyToMeltSnowAndIce2(I,J),
     &                    EnergyToMeltSnowAndIce(I,J) - 
     &                    EnergyToMeltSnowAndIce2(I,J)
               ENDIF
c SEAICE DEBUG
#endif
c there is any flooding to be had                 
                  ENDIF
               ENDDO
            ENDDO

c SEAICEuseFlooding
           ENDIF 
c ALLOW_SEAICE_FLOODING
#endif 

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ATMOSPHERIC_LOADING
            IF ( useRealFreshWaterFlux ) THEN
               DO J=1,sNy
                  DO I=1,sNx
                     sIceLoad(i,j,bi,bj) = HEFF(I,J,bi,bj)*
     &                 SEAICE_rhoIce + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
                  ENDDO
               ENDDO
            ENDIF
#endif

#ifdef SEAICE_DEBUG
            DO j=1,sNy
               DO i=1,sNx

               IF ( (i .EQ. SEAICE_debugPointX)   .and.  
     &              (j .EQ. SEAICE_debugPointY) ) THEN

                  print *,'ifsig: myTime,myIter:',myTime,myIter

                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j --------------  ',i,j

                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j IGR(ML OW ICE)  ',i,j,
     &                 IceGrowthRateMixedLayer(i,j),
     &                 IceGrowthRateOpenWater(i,j),
     &                 NetExistingIceGrowthRate(i,j),
     &                 SEAICE_deltaTtherm
                  
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j F(mi ao)        ',
     &                 i,j,F_mi(i,j), F_ao(i,j)
                  
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j Fi(a,ant2/1 ont)',
     &                 i,j,F_ia(i,j),
     &                 F_ia_net_before_snow(i,j), 
     &                 F_ia_net(i,j), 
     &                 F_io_net(i,j)
                 
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j AREA2/1 HEFF2/1 ',i,j,
     &                 AREANm1(I,J,bi,bj),
     &                 AREA(i,j,bi,bj),
     &                 HEFFNm1(I,J,bi,bj),
     &                 HEFF(i,j,bi,bj)


#ifdef ALLOW_SALT_PLUME
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j A,IGLEA,LPF,SPF ',i,j,
     &                 AREANm1(I,J,bi,bj),
     &                 IceGrowthRateInLeads(I,J),
     &                 leadPlumeFraction(I,J),
     &                 saltPlumeFlux(i,j,bi,bj)
#endif
                  
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j HSNOW2/1 TMX TBC',i,j,
     &                 HSNOW_ORIG(I,J),
     &                 HSNOW(I,J,bi,bj),
     &                 TMIX(i,j,bi,bj)- TMELT, 
     &                 TBC

                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j TI ATP LWD      ',i,j,
     &                 TICE(i,j,bi,bj) - TMELT,
     &                 ATEMP(i,j,bi,bj) -TMELT,
     &                 LWDOWN(i,j,bi,bj)


                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j S_a S_h S_hsnow ',i,j,
     &                 S_a(i,j),
     &                 S_h(i,j),
     &                 S_hsnow(i,j)

                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j IVC(E A ENIN)   ',i,j,
     &                 ExpectedIceVolumeChange(i,j),
     &                 ActualNewTotalVolumeChange(i,j),
     &                 EnergyInNewTotalIceVolume(i,j)
                  
                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j EF(NOS RE) QNET ',i,j,
     &                 NetEnergyFluxOutOfOcean(i,j),
     &                 ResidualEnergyOutOfOcean(i,j),
     &                 QNET(I,J,bi,bj)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j QSW QSWO QSWI   ',i,j,
     &                 QSW(i,j,bi,bj),
     &                 QSWO(i,j),
     &                 QSWI(i,j)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j SW(BML IML SW)  ',i,j,
     &                 QSW_absorb_below_first_layer(i,j),
     &                 QSW_absorb_in_first_layer(i,j),
     &                 SWFRACB

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j ptc(to, qsw, oa)',i,j,
     &                 PredTempChange(i,j),
     &                 PredTempChangeFromQSW (i,j),
     &                 PredTempChangeFromOA_MQNET(i,j)


                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j ptc(fion,ian,ia)',i,j,
     &                 PredTempChangeFromF_IO_NET(i,j),
     &                 PredTempChangeFromF_IA_NET(i,j),
     &                 PredTempChangeFromFIA(i,j)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j ptc(niv)        ',i,j,
     &                 PredTempChangeFromNewIceVol(i,j)


                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j EmPmR EVP PRE RU',i,j,
     &                 EmPmR(I,J,bi,bj),
     &                 EVAP(I,J,bi,bj),
     &                 PRECIP(I,J,bi,bj),
     &                 RUNOFF(I,J,bi,bj)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j PRROIS,SAOI(R .)',i,j,
     &                 PrecipRateOverIceSurfaceToSea(I,J),
     &                 SnowAccRateOverIce(I,J),
     &                 SnowAccOverIce(I,J)

                  print '(A,2i4,4(1x,1P3E15.4))',
     &                 'ifice i j SM(PM PMR . .R) ',i,j,
     &                 PotSnowMeltFromSurf(I,J),
     &                 PotSnowMeltRateFromSurf(I,J),
     &                 SnowMeltFromSurface(I,J),
     &                 SnowMeltRateFromSurface(I,J)

                  print '(A,2i4,4(1x,1P3E15.4))',
     &                 'ifice i j TotSnwMlt ExSnVC',i,j,
     &                 ActualNewTotalSnowMelt(I,J),
     &                 ExpectedSnowVolumeChange(I,J)


                  print '(A,2i4,4(1x,1P3E15.4))',
     &                 'ifice i j fw(CFICE, CFSM) ',i,j,
     &                 FreshwaterContribFromIce(I,J),
     &                 FreshwaterContribFromSnowMelt(I,J)

                  print '(A,2i4,2(1x,1P3E15.4))',
     &                 'ifice i j --------------  ',i,j

               ENDIF
               ENDDO
            ENDDO
#endif /* SEAICE_DEBUG */
            
            
C     end bi,bj loops
         ENDDO
      ENDDO
      
      RETURN
      END
