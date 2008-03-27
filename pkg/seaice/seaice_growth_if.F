C     $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/seaice_growth_if.F,v 1.1 2008/03/27 19:53:14 heimbach Exp $
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
      _RL TBC, salinity_ice, SDF, ICE2SNOW,TMELT

#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft, hFlood
#endif /* ALLOW_SEAICE_FLOODING */

C     QNETI  - net surface heat flux under ice in W/m^2
C     QSWO   - short wave heat flux over ocean in W/m^2
C     QSWI   - short wave heat flux under ice in W/m^2

      _RL QNETI               (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSWO                (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSWI                (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL QSWO_IN_FIRST_LAYER
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSWO_BELOW_FIRST_LAYER
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL QSW_absorb_in_ML    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSW_absorb_below_ML (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     actual ice thickness with upper and lower limit
      _RL HICE_ACTUAL   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C     actual snow thickness
      _RL HSNOW_ACTUAL(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C     wind speed
      _RL UG     (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL SPEED_SQ

C     IAN
      _RL RHOI, RHOFW,CPW,LI,QI,QS,GAMMAT,GAMMA,RHOSW,RHOSN
      _RL FL_C1,FL_C2,FL_C3,FL_C4,deltaHS,deltaHI

      _RL NetExistingIceGrowthRate      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IceGrowthRateUnderExistingIce (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IceGrowthRateFromSurface      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IceGrowthRateOpenWater        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL IceGrowthRateMixedLayer       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL S_a_from_IGROW                (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
 
      _RL PredTempChange
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromQSW
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromOA_MQNET
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromFIA
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromNewIceVol
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromF_IA_NET
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  PredTempChangeFromF_IO_NET
     &      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL ExpectedIceVolumeChange   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ExpectedSnowVolumeChange   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ActualNewTotalVolumeChange(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ActualNewTotalSnowMelt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL EnergyInNewTotalIceVolume (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL NetEnergyFluxOutOfSystem   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL ResidualHeatOutOfSystem    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL SnowAccRateOverIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SmowAccOverIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PrecipRateOverIceSurfaceToSea (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL PotSnowMeltRateFromSurf       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PotSnowMeltFromSurf           (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SnowMeltFromSurface           (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SnowMeltRateFromSurface       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL FreshwaterContribFromSnowMelt (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FreshwaterContribFromIce      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL SurfHeatFluxConvergToSnowMelt (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL EnergyToMeltSnowAndIce        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL EnergyToMeltSnowAndIce2       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     dA/dt = S_a
      _RL S_a (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     dh/dt = S_h
      _RL S_h (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL S_hsnow (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL HSNOW_ORIG (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     F_ia  - heat flux from ice to atmosphere (W/m^2)
C     >0 causes ice growth, <0 causes snow and sea ice melt
      _RL F_ia     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_ia_net (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_ia_net_before_snow (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL F_io_net (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     F_ao  - heat flux from atmosphere to ocean (W/m^2)
      _RL F_ao (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     F_mi - heat flux from mixed layer to ice (W/m^2)
      _RL F_mi (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

c     the theta to use for the calculation of mixed layer-> ice heat fluxes 
      _RL surf_theta

      _RL FLUX_TO_DELTA_TEMP,ENERGY_TO_DELTA_TEMP

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
         kSurface        = Nr 
      else
         kSurface        = 1
      endif

      FLUX_TO_DELTA_TEMP = SEAICE_deltaTtherm*
     &            recip_Cp*recip_rhoConst * recip_drF(1)

      ENERGY_TO_DELTA_TEMP = recip_Cp*recip_rhoConst*recip_drF(1)

C     ICE SALINITY (g/kg)
      salinity_ice = 4.0 

C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze

C     FREEZING POINT OF FRESHWATER 
      TMELT = 273.15

C     IAN

c     Sea ice density (kg m^-3)
      RHOI = 917.0

c     Seawater density (kg m^-3)
      RHOSW = 1026.0

c     Freshwater density (KG M^-3)
      RHOFW = 1000.0

C     Snow density
      RHOSN = 330.0

C     Heat capacity of seawater (J m^-3 K^-1)
      CPW = 4010.0

c     latent heat of fusion for ice (J kg^-1)
      LI = 3.340e5 
c     conversion between Joules and m^3 of ice  (m^3)
      QI = 1/rhoi/Li
      QS = 1/RHOSN/Li

c     FOR FLOODING
      FL_C2 = RHOI/RHOSW
      FL_C2 = RHOSN/RHOSW
      FL_C3 = (RHOSW-RHOI)/RHOSN
      FL_C4 = RHOSN/RHOI

c     Timescale for melting of ice from a warm ML (3 days in seconds)     
c     Damping term for mixed layer heat to melt existing ice
      GAMMA =  dRf(1)/SEAICE_gamma_t

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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)    = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            DO J=1,sNy
               DO I=1,sNx
                  F_ia_net (I,J)      = 0.0 
                  F_ia_net_before_snow(I,J)      = 0.0 
                  F_io_net (I,J)      = 0.0 

                  F_ia (I,J)      = 0.0 
                  F_ao (I,J)      = 0.0 
                  F_mi (I,J)      = 0.0 

                  QNETI(I,J)      = 0.0 
                  QSWO (I,J)      = 0.0 
                  QSWI (I,J)      = 0.0 

                  QSWO_BELOW_FIRST_LAYER (I,J) = 0.0 
                  QSWO_IN_FIRST_LAYER    (I,J) = 0.0 

                  S_a                             (I,J) = 0.0 
                  S_h                             (I,J) = 0.0 

                  IceGrowthRateUnderExistingIce   (I,J) = 0.0 
                  IceGrowthRateFromSurface        (I,J) = 0.0 
                  NetExistingIceGrowthRate        (I,J) = 0.0 
                  S_a_from_IGROW                  (I,J) = 0.0

                  PredTempChange              (I,J) = 0.0
                  PredTempChangeFromQSW       (I,J) = 0.0
                  PredTempChangeFromOA_MQNET  (I,J) = 0.0
                  PredTempChangeFromFIA       (I,J) = 0.0
                  PredTempChangeFromF_IA_NET  (I,J) = 0.0
                  PredTempChangeFromF_IO_NET  (I,J) = 0.0
                  PredTempChangeFromNewIceVol (I,J) = 0.0

                  IceGrowthRateOpenWater          (I,J) = 0.0 
                  IceGrowthRateMixedLayer         (I,J) = 0.0 

                  ExpectedIceVolumeChange         (I,J) = 0.0 
                  ExpectedSnowVolumeChange        (I,J) = 0.0 
                  ActualNewTotalVolumeChange      (I,J) = 0.0 
                  ActualNewTotalSnowMelt          (I,J) = 0.0 

                  EnergyInNewTotalIceVolume       (I,J) = 0.0 
                  NetEnergyFluxOutOfSystem        (I,J) = 0.0 
                  ResidualHeatOutOfSystem         (I,J) = 0.0 
                  QSW_absorb_in_ML                (I,J) = 0.0 
                  QSW_absorb_below_ML             (I,J) = 0.0 

                  SnowAccRateOverIce     (I,J) = 0.0
                  SmowAccOverIce         (I,J) = 0.0
                  PrecipRateOverIceSurfaceToSea   (I,J) = 0.0

                  PotSnowMeltRateFromSurf         (I,J) = 0.0
                  PotSnowMeltFromSurf             (I,J) = 0.0
                  SnowMeltFromSurface             (I,J) = 0.0
                  SnowMeltRateFromSurface         (I,J) = 0.0
                  SurfHeatFluxConvergToSnowMelt   (I,J) = 0.0 

                  FreshwaterContribFromSnowMelt   (I,J) = 0.0
                  FreshwaterContribFromIce        (I,J) = 0.0

c the post sea ice advection and diffusion ice state are in time level 1.  
c move these to the time level 2 before thermo.  after this routine 
c the updated ice state will be in time level 1 again. (except for snow 
c which does not have 3 time levels for some reason) 
                  HEFF(I,J,2,bi,bj) = HEFF(I,J,1,bi,bj) 
                  AREA(I,J,2,bi,bj) = AREA(I,J,1,bi,bj) 

               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
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
                  HEFF(I,J,2,bi,bj)  = MAX(0.0, HEFF(I,J,2,bi,bj)  ) 
                  HSNOW(I,J,bi,bj)   = MAX(0.0, HSNOW(I,J,bi,bj)   ) 
                  AREA(I,J,2,bi,bj)  = MAX(0.0, AREA(I,J,2,bi,bj)  ) 
cif this is hack to prevent negative precip.  somehow negative precips  
cif escapes my exf_checkrange hack
cph-checkthis
                  IF (PRECIP(I,J,bi,bj) .LT. 0.0 _d 0) THEN 
                     PRECIP(I,J,bi,bj) = 0.0 _d 0 
                  ENDIF 
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE precip(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif
            DO J=1,sNy
               DO I=1,sNx

                  IF (HEFF(I,J,2,bi,bj) .EQ. 0.0) THEN 
                      AREA(I,J,2,bi,bj) = 0.0 _d 0 
                      HSNOW(I,J,bi,bj)  = 0.0 _d 0 
                  ENDIF 
 
                  IF (AREA(I,J,2,bi,bj) .EQ. 0.0) THEN 
                     HEFF(I,J,2,bi,bj)  = 0.0 _d 0 
                     HSNOW(I,J,bi,bj)   = 0.0 _d 0 
                  ENDIF 
 
C PROCEED ONLY IF WE ARE CERTAIN TO HAVE ICE (AREA > 0) 

                  IF (AREA(I,J,2,bi,bj) .GT. 0.) THEN
                     HICE_ACTUAL(I,J)  = 
     &                  HEFF(I,J,2,bi,bj)/AREA(I,J,2,bi,bj)

                     HSNOW_ACTUAL(I,J) = HSNOW(I,J,bi,bj)/
     &                  AREA(I,J,2,bi,bj)

c                   ACCUMULATE SNOW
c                   Is the ice/surface below freezing or at the freezing 
c                   point (melting).  If it is freezing the precip is 
c                   felt as snow and will accumulate over the ice. Else,
c                   precip makes its way, like all things in time, to the sea.            
                    IF (TICE(I,J,bi,bj) .LT. TMELT) THEN
c                     Snow falls onto freezing surface remaining as snow
                      SnowAccRateOverIce(I,J) = 
     &                  PRECIP(I,J,bi,bj)*RHOFW/RHOSN

c                     None of the precipitation falls into the sea 
                      PrecipRateOverIceSurfaceToSea(I,J) = 0.0
  
                    ELSE 
c                     The snow melts on impact is is considered 
c                     nothing more than rain.  Since meltponds are
c                     not explicitly represented,this rain runs
c                     immediately into the sea

                      SnowAccRateOverIce(I,J) = 0.0
C                     The rate of rainfall over melting ice.
                      PrecipRateOverIceSurfaceToSea(I,J)=
     &                  PRECIP(I,J,bi,bj)
                   ENDIF

c                  In m of mean snow thickness.
                   SmowAccOverIce(I,J) = 
     &                  SnowAccRateOverIce(I,J)
     &                  *SEAICE_deltaTtherm*Area(I,J,2,bi,bj)

                ELSE
                   HEFF(I,J,2,bi,bj) = 0.0
                   HICE_ACTUAL(I,J)  = 0.0 
                   HSNOW_ACTUAL(I,J) = 0.0
                   HSNOW(I,J,bi,bj)  = 0.0
                ENDIF
                HSNOW_ORIG(I,J) = HSNOW(I,J,bi,bj)
             ENDDO
          ENDDO
            
C     FIND ATM. WIND SPEED
            DO J=1,sNy
               DO I=1,sNx
#ifdef SEAICE_EXTERNAL_FORCING
c     USE EXF PACKAGE
                  UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
#else 
C     CALCULATE IT HERE
                  SPEED_SQ = UWIND(I,J,bi,bj)**2 + VWIND(I,J,bi,bj)**2
                  IF ( SPEED_SQ .LE. SEAICE_EPS_SQ ) THEN
                     UG(I,J)=SEAICE_EPS
                  ELSE
                     UG(I,J)=SQRT(SPEED_SQ)
                  ENDIF
#endif /* SEAICE_EXTERNAL_FORCING */
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics
cphCADJ STORE uwind  = comlev1, key = ikey_dynamics
cphCADJ STORE vwind  = comlev1, key = ikey_dynamics
CADJ STORE tice   = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

C     SET LAYER TEMPERATURE IN KELVIN
            DO J=1,sNy
               DO I=1,sNx
                  TMIX(I,J,bi,bj)=
     &                 theta(I,J,kSurface,bi,bj) + TMELT
               ENDDO
            ENDDO

C     NOW DO ICE

            CALL SEAICE_BUDGET_ICE_IF(
     I           UG, HICE_ACTUAL, HSNOW_ACTUAL, 
     U           TICE, 
     O           F_io_net,F_ia_net,F_ia, QSWI, 
     I           bi, bj)

C Sometimes it's nice to have a setup without ice-atmosphere heat
C fluxes.  This flag turns those fluxes to zero but leaves the 
C Ice ocean fluxes intact.  Thus, the first oceanic cell can transfer
C heat to the ice leading to melting in F_ml and it can release 
C heat to the atmosphere through leads and open area thus growing it in
C F_ao

#ifdef FORBID_ICE_SURFACE_ATMOSPHERE_HEAT_FLUXES
            DO J=1,sNy
               DO I=1,sNx
                  F_ia_net (I,J)  = 0.0 
                  F_ia (I,J)      = 0.0
                  F_io_net(I,J)   = 0.0 
               ENDDO
            ENDDO
#endif

C--   NET HEAT FLUX TO ICE FROM MIXED LAYER (POSITIVE MEANS NET OUT)
            DO J=1,sNy
               DO I=1,sNx

#ifdef SEAICE_DEBUG
               IF ( (I .EQ. SEAICE_debugPointX)   .and.
     &              (J .EQ. SEAICE_debugPointY) ) THEN

                 print *,'sig: I,J,F_ia,F_ia_net',I,J,F_ia(I,J),
     &              F_ia_net(I,J)

               ENDIF
#endif

                  F_ia_net_before_snow(I,J) = F_ia_net(I,J)

                  IF (Area(I,J,2,bi,bj)*HEFF(I,J,2,bi,bj) .LE. 0.) THEN
                     IceGrowthRateUnderExistingIce(I,J) = 0.0
                     IceGrowthRateFromSurface(I,J)      = 0.0
                     NetExistingIceGrowthRate(I,J)      = 0.0 
                  ELSE
c                    The growth rate under existing ice is given by the upward
c                    ocean-ice conductive flux, F_io_net, and QI, which converts
c                    Joules to meters of ice.  This quantity has units of meters
c                    of ice per second.
                     IceGrowthRateUnderExistingIce(I,J)=F_io_net(I,J)*QI

c                    Snow/Ice surface heat convergence is first used to melt 
c                    snow.  If all of this heat convergence went into melting
c                    snow, this is the rate at which it would do it
c                    F_ia_net must be negative, -> PSMRFW is positive for melting
                     PotSnowMeltRateFromSurf(I,J)= - F_ia_net(I,J)*QS

c                    This is the depth of snow that would be melted at this rate
c                    and the seaice delta t. In meters of snow.
                     PotSnowMeltFromSurf(I,J) =
     &                  PotSnowMeltRateFromSurf(I,J)* SEAICE_deltaTtherm

c                    If we can melt MORE than is actually there, then we will 
c                    reduce the melt rate so that only that which is there 
c                    is melted in one time step.  In this case not all of the 
c                    heat flux convergence at the surface is used to melt snow,
c                    The leftover energy is going to melt ice.  
c                    SurfHeatFluxConvergToSnowMelt is the part of the total heat 
c                    flux convergence going to melt snow. 

                     IF (PotSnowMeltFromSurf(I,J) .GE. 
     &                 HSNOW_ACTUAL(I,J)) THEN
c                      Snow melt and melt rate in actual snow thickness.
                       SnowMeltFromSurface(I,J)     = HSNOW_ACTUAL(I,J)

                       SnowMeltRateFromSurface(I,J) = 
     &                   SnowMeltFromSurface(I,J)/ SEAICE_deltaTtherm

c                      Since F_ia_net is focused only over ice, its reduction 
c                      requires knowing how much snow is actually melted
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

                     NetExistingIceGrowthRate(I,J) = 
     &                 IceGrowthRateUnderExistingIce(I,J) +
     &                 IceGrowthRateFromSurface(I,J)
                  ENDIF
               ENDDO
            ENDDO

c     HERE WE WILL MELT SNOW AND ADJUST NET EXISTING ICE GROWTH RATE 
C     TO REFLECT REDUCTION IN SEA ICE MELT.

C     NOW DETERMINE GROWTH RATES
C     FIRST DO OPEN WATER
            CALL SEAICE_BUDGET_OCEAN_IF(
     I           UG, 
     U           TMIX, 
     O           F_ao, QSWO, 
     I           bi, bj)
 
#ifdef SEAICE_DEBUG
        print *,'myiter', myIter
        print '(A,2i4,2(1x,1P2E15.3))',
     &       'ifice sigr, dbgx,dby, (netHF, SWHeatFlux)',
     &        SEAICE_debugPointX,   SEAICE_debugPointY,
     &        F_ao(SEAICE_debugPointX, SEAICE_debugPointY),
     &        QSWO(SEAICE_debugPointX, SEAICE_debugPointY)
#endif


C--   NET HEAT FLUX TO ICE FROM MIXED LAYER (POSITIVE MEANS NET OUT)
c--   not all of the sw radiation is absorbed in the first layer, only that
c     which is absorbed melts ice.   SWFRACB is calculated in seaice_init_vari.F 
            DO J=1,sNy
               DO I=1,sNx

c     The contribution of shortwave heating is 
c     not included without SHORTWAVE_HEATING
#ifdef SHORTWAVE_HEATING
                  QSWO_BELOW_FIRST_LAYER(i,j)= QSWO(I,J)*SWFRACB
                  QSWO_IN_FIRST_LAYER(I,J)   = QSWO(I,J)*(1.0 - SWFRACB)
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
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C--   NET HEAT FLUX TO ICE FROM MIXED LAYER (POSITIVE MEANS FLUX INTO ICE
C     AND MELTING)
            DO J=1,sNy
               DO I=1,sNx

C     FIND THE FREEZING POINT OF SEAWATER IN THIS CELL
#ifdef SEAICE_VARIABLE_FREEZING_POINT
                  TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 
     &                 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
                  
c     example: theta(i,j,ksurf)  = 0, tbc = -2, 
c     fmi = -gamm*rhocpw * (0-(-2)) = - 2 * gamm * rhocpw, 
c     a NEGATIVE number.  Heat flux INTO ice.

c     It is fantastic that the model frequently generates thetas less 
c     then the freezing point.  Just fantastic.  When this happens, 
c     throw your hands up into the air, shut off the mixed layer 
c     heat flux, and hope for the best.
                  surf_theta = max(theta(I,J,kSurface,bi,bj), TBC)
                  
                  F_mi(I,J) = -GAMMA*RHOSW*CPW *(surf_theta - TBC)

                  IceGrowthRateMixedLayer(I,J) = F_mi(I,J)*QI;
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE S_h(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C     CALCULATE THICKNESS DERIVATIVE (S_h)
            DO J=1,sNy
               DO I=1,sNx
                  S_h(I,J) = 
     &                 NetExistingIceGrowthRate(I,J)*AREA(I,J,2,bi,bj)+
     &                 (1. -AREA(I,J,2,bi,bj))*
     &                 IceGrowthRateOpenWater(I,J) +
     &                 IceGrowthRateMixedLayer(I,J)

c                  Both the accumulation and melt rates are in terms
c                  of actual snow thickness.  As with ice, multiplying
c                  with area converts to mean snow thickness.
                   S_hsnow(I,J) =     AREA(I,J,2,bi,bj)* (
     &                  SnowAccRateOverIce(I,J) - 
     &                  SnowMeltRateFromSurface(I,J)     ) 
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE S_h(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE S_hsnow(:,:)      = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            DO J=1,sNy
               DO I=1,sNx
                  S_a(I,J) =  0.0 
C     IF THE OPEN WATER GROWTH RATE IS POSITIVE 
C     THEN EXTEND ICE AREAL COVER, S_a > 0

C     TWO CASES, IF THERE IS ALREADY ICE PRESENT THEN EXTEND THE AREA USING THE 
C     OPEN WATER GROWTH RATE.  IF THERE IS NO ICE PRESENT DO NOT EXTEND THE ICE
C     UNTIL THE NET ICE THICKNESS RATE IS POSITIVE.  I.E. IF THE MIXED LAYER 
C     HEAT FLUX INTO THE NEW ICE IS ENOUGH TO IMMEDIATELY MELT IT, DO NOT GROW
C     IT.
                  IF (IceGrowthRateOpenWater(I,J) .GT. 0) THEN
                     S_a_from_IGROW(I,J) = (ONE - AREA(I,J,2,bi,bj))*
     &                      IceGrowthRateOpenWater(I,J)/HO

                     IF (AREA(I,J,2,bi,bj) .GT. 0.) THEN
                        S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)
                     ELSE
                        IF (S_h(I,J) .GT. 0) THEN
                           S_a(I,J) = S_a(I,J) + S_a_from_IGROW(I,J)
                        ENDIF
                     ENDIF
                  ENDIF

C     REDUCE THE ICE COVER IF ICE IS PRESENT
                  IF ( (S_h(I,J) .LT. 0.) .AND. 
     &                 (AREA(I,J,2,bi,bj).GT. 0.) .AND.
     &                 (HEFF(I,J,2,bi,bj).NE. 0.) ) THEN

                     S_a(I,J) = S_a(I,J) 
     &                    + AREA(I,J,2,bi,bj)/(2.0*HEFF(I,J,2,bi,bj))*
     &                    IceGrowthRateOpenWater(I,J)*
     &                    (1-AREA(I,J,2,bi,bj))
                  ELSE
                     S_a(I,J) = S_a(I,J) +  0.0
                  ENDIF

C     REDUCE THE ICE COVER IF ICE IS PRESENT
                  IF ( (IceGrowthRateMixedLayer(I,J) .LE. 0.) .AND. 
     &                 (AREA(I,J,2,bi,bj).GT. 0.) .AND.
     &                 (HEFF(I,J,2,bi,bj).NE. 0.) ) THEN

                     S_a(I,J) = S_a(I,J) 
     &                    + AREA(I,J,2,bi,bj)/(2.0*HEFF(I,J,2,bi,bj))*
     &                    IceGrowthRateMixedLayer(I,J)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0.0
                  ENDIF

C     REDUCE THE ICE COVER IF ICE IS PRESENT
                  IF ( (NetExistingIceGrowthRate(I,J) .LE. 0.) .AND. 
     &                 (AREA(I,J,2,bi,bj).GT. 0.) .AND.
     &                 (HEFF(I,J,2,bi,bj).NE. 0.) ) THEN

                     S_a(I,J) = S_a(I,J) 
     &                   + AREA(I,J,2,bi,bj)/(2.0*HEFF(I,J,2,bi,bj))*
     &                   NetExistingIceGrowthRate(I,J)*AREA(I,J,2,bi,bj)

                  ELSE
                     S_a(I,J) = S_a(I,J) +  0.0
                  ENDIF
                  
               ENDDO
            ENDDO
                 

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif

C     ACTUALLY CHANGE THE AREA AND THICKNESS
            DO J=1,sNy
               DO I=1,sNx
                  AREA(I,J,1,bi,bj) = AREA(I,J,2,bi,bj) + 
     &                 SEAICE_deltaTtherm * S_a(I,J)
                  HEFF(I,J,1,bi,bj) = HEFF(I,J,2,bi,bj) +
     &                 SEAICE_deltaTTherm * S_h(I,J)
                  HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) +
     &                 SEAICE_deltaTTherm * S_hsnow(I,J)
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif

            DO J=1,sNy
               DO I=1,sNx
C     SET LIMIT ON AREA etc.
                  AREA(I,J,1,bi,bj) = MIN(1.,AREA(I,J,1,bi,bj))
                  AREA(I,J,1,bi,bj) = MAX(0.,AREA(I,J,1,bi,bj))
                  HEFF(I,J,1,bi,bj) = MAX(0.0, HEFF(I,J,1,bi,bj))
                  HSNOW(I,J,bi,bj)  = MAX(0.0, HSNOW(I,J,bi,bj))
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif

            DO J=1,sNy
               DO I=1,sNx
                  IF (AREA(I,J,1,bi,bj) .GT. 0.0) THEN
                      HICE_ACTUAL(I,J) = 
     &                   HEFF(I,J,1,bi,bj)/AREA(I,J,1,bi,bj)
                      HSNOW_ACTUAL(I,J) = 
     &                    HSNOW(I,J,bi,bj)/AREA(I,J,1,bi,bj)
                  ELSE
                      HICE_ACTUAL(I,J) = 0.0
                      HSNOW_ACTUAL(I,J) = 0.0
                  ENDIF
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

c     constrain area is no thickness and vice versa.
            DO J=1,sNy
               DO I=1,sNx
                  IF (HEFF(I,J,1,bi,bj)  .LE. 0.0 .OR.
     &                 AREA(I,J,1,bi,bj) .LE. 0.0) THEN
                     
                     AREA(I,J,1,bi,bj)       = 0.0
                     HEFF(I,J,1,bi,bj)       = 0.0
                     HICE_ACTUAL(I,J)        = 0.0
                     HSNOW(I,J,bi,bj)        = 0.0
                     HSNOW_ACTUAL(I,J)       = 0.0
                  ENDIF
               ENDDO
            ENDDO
            
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            DO J=1,sNy
               DO I=1,sNx

c     The amount of new mean thickness we expect to grow
                  ExpectedIceVolumeChange(I,J)  = S_h(I,J) *
     &                 SEAICE_deltaTtherm

                  ExpectedSnowVolumeChange(I,J) = S_hsnow(I,J)*
     &                 SEAICE_deltaTtherm

c     THE EFFECTIVE SHORTWAVE HEATING RATE
#ifdef SHORTWAVE_HEATING
                  QSW(I,J,bi,bj)  =
     &                 QSWI(I,J)  * (     AREA(I,J,2,bi,bj)) +
     &                 QSWO(I,J)  * (1. - AREA(I,J,2,bi,bj))
#else
                  QSW(I,J,bi,bj) = 0. _d 0
#endif

                  ActualNewTotalVolumeChange(I,J) = 
     &                 HEFF(I,J,1,bi,bj) - HEFF(I,J,2,bi,bj)

c     The net average snow thickness melt that is actually realized. e.g.
c     hsnow_orig  = 0.25 m (e.g. 1 m of ice over a cell 1/4 covered in snow)
c     hsnow_new   = 0.20 m
c     snow accum  = 0.05 m
c            melt = 0.25 + 0.05 - 0.2 = 0.1 m 

c     since this is in mean snow thickness it might have been  0.4 of actual 
c     snow thickness over the 1/4 of the cell which is ice covered.
                  ActualNewTotalSnowMelt(I,J) = 
     &                 HSNOW_ORIG(I,J) +
     &                 SmowAccOverIce(I,J) -
     &                 HSNOW(I,J,bi,bj)

c     The latent heat of fusion of the new ice
                  EnergyInNewTotalIceVolume(I,J) = 
     &                 ActualNewTotalVolumeChange(I,J)/QI

c     This is the net energy flux out of the ice+ocean system
c     Remember -----
c     F_ia_net : 0 if under freezing conditions (F_c < 0)
c                The sum of the non-conductive surfice ice fluxes otherwise
c
c     F_io_net : The conductive fluxes under freezing conditions (F_c < 0)
c                0 under melting conditions (no energy flux from ice to
c                ocean)
c
c     So if we are freezing, F_io_net is the conductive flux and there 
c     is energy balance at ice surface, F_ia_net =0.  If we are melting
c     There is a convergence of energy into the ice from above
                  NetEnergyFluxOutOfSystem(I,J) = SEAICE_deltaTtherm *
     &               (AREA(I,J,2,bi,bj) * 
     &               (F_ia_net(I,J) + F_io_net(I,J) + QSWI(I,J))
     &         +     (1.0 - AREA(I,J,2,bi,bj)) * 
     &                F_ao(I,J))

c     THE QUANTITY OF HEAT WHICH IS THE RESIDUAL TO THE QUANTITY OF 
c     ML temperature.  If the net energy flux is exactly balanced by the 
c     latent energy of fusion in the new ice created then we won't 
c     change the ML temperature at all.

                  ResidualHeatOutOfSystem(I,J) = 
     &             NetEnergyFluxOutOfSystem(I,J) -
     &             EnergyInNewTotalIceVolume(I,J)

C     NOW FORMULATE QNET, which time LEVEL, ORIG 2.
C     THIS QNET WILL DETERMINE THE TEMPERATURE CHANGE OF THE MIXED LAYER
C     QNET IS A DEPTH AVERAGED HEAT FLUX FOR THE OCEAN COLUMN
C     BECAUSE OF THE 
                  QNET(I,J,bi,bj) =
     &             ResidualHeatOutOfSystem(I,J) / SEAICE_deltaTtherm


c    Like snow melt, if there is melting, this quantity is positive.
c    The change of freshwater content is per unit area over the entire 
c    cell, not just over the ice covered bits.
                  FreshwaterContribFromIce(I,J) = 
     &                -ActualNewTotalVolumeChange(I,J)*RHOI/RHOFW

c    The freshwater contribution from snow comes only in the form of melt
c    unlike ice, which takes freshwater upon growth and yields freshwater
c    upon melt.  This is why the the actual new average snow melt was determined.
c    In m/m^2 over the entire cell.
                  FreshwaterContribFromSnowMelt(I,J) =
     &                 ActualNewTotalSnowMelt(I,J)*RHOSN/RHOFW

c    This seems to be in m/s, original time level 2 for area
c    Only the precip and evap need to be area weighted.  The runoff
c    and freshwater contribs from ice and snow melt are already mean
c    weighted
                  EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &                 ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &                 * ( ONE - AREA(I,J,2,bi,bj) )
     &                 - PrecipRateOverIceSurfaceToSea(I,J)*
     &                     AREA(I,J,2,bi,bj)
#ifdef ALLOW_RUNOFF
     &                 - RUNOFF(I,J,bi,bj)
#endif
     &                 - (FreshwaterContribFromIce(I,J) +
     &                    FreshwaterContribFromSnowMelt(I,J))/
     &                    SEAICE_deltaTtherm) 



C     DO SOME DEBUGGING CALCULATIONS.  MAKE SURE SUMS ALL ADD UP.
#ifdef SEAICE_DEBUG

C     THE SHORTWAVE ENERGY FLUX ABSORBED IN THE SURFACE LAYER
#ifdef SHORTWAVE_HEATING
                  QSW_absorb_in_ML(I,J) = QSW(I,J,bi,bj)*
     &              (1.0 - SWFRACB)
#else 

                  QSW_absorb_in_ML(I,J) = 0. _d 0
#endif

C     THE SHORTWAVE ENERGY FLUX PENETRATING BELOW THE SURFACE LAYER
                  QSW_absorb_below_ML(I,J) = 
     &                 QSW(I,J,bi,bj) -  QSW_absorb_in_ML(I,J);

                  PredTempChangeFromQSW(I,J) = 
     &             - QSW_absorb_in_ML(I,J) * FLUX_TO_DELTA_TEMP

                  PredTempChangeFromOA_MQNET(I,J) = 
     &            -(QNET(I,J,bi,bj) - QSWO(I,J))*(1. -AREA(I,J,2,bi,bj))
     &             * FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IO_NET(I,J) = 
     &             -F_io_net(I,J)*AREA(I,J,2,bi,bj)*FLUX_TO_DELTA_TEMP

                  PredTempChangeFromF_IA_NET(I,J) = 
     &             -F_ia_net(I,J)*AREA(I,J,2,bi,bj)*FLUX_TO_DELTA_TEMP

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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            
            DO J=1,sNy
               DO I=1,sNx
                  AREA(I,J,1,bi,bj) = AREA(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
                  HEFF(I,J,1,bi,bj) = HEFF(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
                  HSNOW(I,J,bi,bj)  = HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
               ENDDO
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ALLOW_SEAICE_FLOODING
           IF(SEAICEuseFlooding) THEN

            DO J = 1,sNy
               DO I = 1,sNx
                  EnergyToMeltSnowAndIce(I,J) = 
     &                 HEFF(I,J,1,bi,bj)/QI +
     &                 HSNOW(I,J,bi,bj)/QS
                  
                  deltaHS = FL_C2*( HSNOW_ACTUAL(I,J) -
     &                 HICE_ACTUAL(I,J)*FL_C3 )
                  
                  IF (deltaHS .GT. 0.0) THEN
                     deltaHI = FL_C4*deltaHS
                     
                     HICE_ACTUAL(I,J) = HICE_ACTUAL(I,J)  
     &                    + deltaHI

                     HSNOW_ACTUAL(I,J)= HSNOW_ACTUAL(I,J) 
     &                    - deltaHS

                     HEFF(I,J,1,bi,bj)= HICE_ACTUAL(I,J) *
     &                    AREA(I,J,1,bi,bj)

                     HSNOW(I,J,bi,bj) = HSNOW_ACTUAL(I,J)*
     &                    AREA(I,J,1,bi,bj)
                     
                     EnergyToMeltSnowAndIce2(I,J) = 
     &                    HEFF(I,J,1,bi,bj)/QI +
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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ATMOSPHERIC_LOADING
            IF ( useRealFreshWaterFlux ) THEN
               DO J=1,sNy
                  DO I=1,sNx
                     sIceLoad(i,j,bi,bj) = HEFF(I,J,1,bi,bj)*
     &                    SEAICE_rhoIce + HSNOW(I,J,bi,bj)* 330. _d 0
                  ENDDO
               ENDDO
            ENDIF
#endif

#ifdef SEAICE_DEBUG
            DO j=1-OLy,sNy+OLy
               DO i=1-OLx,sNx+OLx

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
     &                 AREA(i,j,2,bi,bj),
     &                 AREA(i,j,1,bi,bj),
     &                 HEFF(i,j,2,bi,bj),
     &                 HEFF(i,j,1,bi,bj)
                  
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
     &                 NetEnergyFluxOutOfSystem(i,j),
     &                 ResidualHeatOutOfSystem(i,j),
     &                 QNET(I,J,bi,bj)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j QSW QSWO QSWI   ',i,j,
     &                 QSW(i,j,bi,bj),
     &                 QSWO(i,j),
     &                 QSWI(i,j)

                  print '(A,2i4,3(1x,1P3E15.4))',
     &                 'ifice i j SW(BML IML SW)  ',i,j,
     &                 QSW_absorb_below_ML(i,j),
     &                 QSW_absorb_in_ML(i,j),
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
     &                 SmowAccOverIce(I,J)

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
            
            
C     BI,BJ'S
         ENDDO
      ENDDO
      
      RETURN
      END
