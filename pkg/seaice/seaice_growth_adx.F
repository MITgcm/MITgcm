#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

#undef SEAICE_GROWTH_ADX_STORE_MORE

CBOP
C     !ROUTINE: SEAICE_GROWTH_ADX
C     !INTERFACE:
      SUBROUTINE SEAICE_GROWTH_ADX( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE seaice_growth_adx
C     | o Adjointable seaice thermodynamic code based on
C     |   Ian Fentys code with modifications for nonlinear free
C     |   surface budget closure by A.Bigdeli.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef SEAICE_USE_GROWTH_ADX
#if (defined ALLOW_EXF) && (defined ALLOW_ATM_TEMP)
C     !FUNCTIONS:
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

C     !LOCAL VARIABLES:
C     === Local variables ===
C
C unit/sign convention:

C HEFF is effective Hice thickness (m3/m2)
C HSNOW is Heffective snow thickness (m3/m2)
C HSALT is Heffective salt content (g/m2)
C AREA is the seaice cover fraction (0<=AREA<=1)

C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface
C     IT :: ice thickness category index (MULTICATEGORIES and ITD code)
      INTEGER IT
C     msgBuf      :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
C     constants
      _RL tempFrz, ICE2SNOW, SNOW2ICE, surf_theta
      _RL QI, QS
      _RL lhSublim

C     Regularization values squared
      _RL area_reg_sq, hice_reg_sq

C     pathological cases thresholds
      _RL heffTooHeavy

C     Helper variables: reciprocal of some constants
      _RL recip_multDim
      _RL recip_deltaTtherm
      _RL recip_rhoIce
      _RL recip_QI
      _RL recip_QS
      _RL recip_HO
      _RL recip_HO_south
      _RL rhoFresh2rhoSnow
      _RL rhoSnow2rhoFresh
      _RL rhoIce2rhoFresh

C     facilitate multi-category snow implementation
      _RL pFac, pFacSnow

C     temporary variables available for the various computations
      _RL tmpscal0, tmpscal1, tmpscal2, tmpscal3

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iicekey
#endif

C==   local arrays ==
C--   TmixLoc        :: ocean surface/mixed-layer temperature (in K)
      _RL TmixLoc       (1:sNx,1:sNy)

C     actual ice thickness (with upper and lower limit)
      _RL hiceActual          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hsnowActual         (1:sNx,1:sNy)
C     actual ice thickness (with lower limit only) Reciprocal
      _RL recip_hiceActual    (1:sNx,1:sNy)

C     AREA_PRE :: hold sea-ice fraction field before any seaice-thermo update
      _RL AREApreTH           (1:sNx,1:sNy)
      _RL HEFFpreTH           (1:sNx,1:sNy)
      _RL HSNWpreTH           (1:sNx,1:sNy)

C     wind speed
      _RL UG                  (1:sNx,1:sNy)

C     temporary variables available for the various computations
      _RL ticeInMult          (1:sNx,1:sNy,nITD)
      _RL ticeOutMult         (1:sNx,1:sNy,nITD)
      _RL hiceActualMult      (1:sNx,1:sNy,nITD)
      _RL hsnowActualMult     (1:sNx,1:sNy,nITD)

      _RL F_io_net_mult     (1:sNx,1:sNy,nITD)
      _RL F_ia_net_mult     (1:sNx,1:sNy,nITD)
      _RL F_ia_mult     (1:sNx,1:sNy,nITD)
      _RL QSWI_mult     (1:sNx,1:sNy,nITD)
      _RL FWsublim_mult     (1:sNx,1:sNy,nITD)

#ifdef ALLOW_DIAGNOSTICS
C     Helper variables for diagnostics
c     _RL DIAGarrayA    (1:sNx,1:sNy)
      _RL DIAGarrayB    (1:sNx,1:sNy)
      _RL DIAGarrayC    (1:sNx,1:sNy)
      _RL DIAGarrayD    (1:sNx,1:sNy)
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_SALT_PLUME
c     d(HEFF)/dt from heat fluxes in the open water fraction of the grid cell
      _RL IceGrowthRateInLeads          (1:sNx,1:sNy)

c     The fraction of salt released in leads by new ice production there
c     which is to be sent to the salt plume package
      _RL leadPlumeFraction             (1:sNx,1:sNy)
#endif

      _RL QSWO_BELOW_FIRST_LAYER (1:sNx,1:sNy)
      _RL QSWO_IN_FIRST_LAYER (1:sNx,1:sNy)
      _RL QSWO (1:sNx,1:sNy)
      _RL QSWI (1:sNx,1:sNy)

C     Sea ice growth rates (m/s)
      _RL ActualNewTotalVolumeChange     (1:sNx,1:sNy)
      _RL ActualNewTotalSnowMelt     (1:sNx,1:sNy)

      _RL NetExistingIceGrowthRate      (1:sNx,1:sNy)
      _RL IceGrowthRateUnderExistingIce (1:sNx,1:sNy)
      _RL IceGrowthRateFromSurface      (1:sNx,1:sNy)
      _RL IceGrowthRateOpenWater        (1:sNx,1:sNy)
      _RL IceGrowthRateMixedLayer       (1:sNx,1:sNy)

      _RL EnergyInNewTotalIceVolume     (1:sNx,1:sNy)
      _RL NetEnergyFluxOutOfOcean       (1:sNx,1:sNy)

CAB   Terms for closing the budget
      _RL SItflux     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIaaflux    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIatmFW     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CAB these two terms are placeholders for extra bits from advection,
C   used in budget
      _RL SIheffNeg   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIhsnwNeg   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

CAB
      _RL d_HEFFbyFlood                (1:sNx,1:sNy)

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

C     S_h : d(HEFF)/dt
      _RL S_h                           (1:sNx,1:sNy)

C     S_hsnow : d(HSNOW)/dt
      _RL S_hsnow                       (1:sNx,1:sNy)

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

      _RL FWsublim                      (1:sNx,1:sNy)

C     S_a_from_IGROW : d(AREA)/dt [from ice growth rate from open water fluxes]
      _RL S_a_IGROW                     (1:sNx,1:sNy)

C     S_a_from_IGROW : d(AREA)/dt [from ice growth rate from ocean-ice fluxes]
      _RL S_a_IGRML                     (1:sNx,1:sNy)

C     S_a_from_IGROW : d(AREA)/dt [from ice growth rate from ice-atm fluxes]
      _RL S_a_IGRNE                     (1:sNx,1:sNy)

C     Factor by which we increase the upper ocean friction velocity (u*) when
C     ice is absent in a grid cell  (dimensionless)
      _RL MLTF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C ===================================================================
C =================PART 0: constants and initializations=============
C ===================================================================

      IF ( usingPCoords ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     Cutoff for iceload
      heffTooHeavy=drF(kSurface) / 5. _d 0
      IF ( usingPCoords )
     &     heffTooHeavy = heffTooHeavy * recip_rhoConst * recip_gravity

C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SEAICE_rhoIce/SEAICE_rhoSnow
      SNOW2ICE     = ONE / ICE2SNOW

C  Heat Flux * QI = [W] [m^3/kg] [kg/J] = [m^3/s] or [m/s] per unit area
      QI           = ONE/(SEAICE_rhoIce*SEAICE_lhFusion)
C  Heat Flux * QS = [W] [m^3/kg] [kg/J] = [m^3/s] or [m/s] per unit area
      QS           = ONE/(SEAICE_rhoSnow*SEAICE_lhFusion)

C     ICE LATENT HEAT CONSTANT
      lhSublim = SEAICE_lhEvap + SEAICE_lhFusion

C     avoid unnecessary divisions in loops
      recip_multDim     = SEAICE_multDim
      recip_multDim     = ONE / recip_multDim
C     above/below: double/single precision calculation of recip_multDim
c     recip_multDim     = 1./float(SEAICE_multDim)
      recip_deltaTtherm = ONE / SEAICE_deltaTtherm
      recip_rhoIce      = ONE / SEAICE_rhoIce
      recip_QI          = ONE / QI
      recip_QS          = ONE / QS
      recip_HO          = ONE / HO
      recip_HO_south    = ONE / HO_south
      rhoFresh2rhoSnow  = rhoConstFresh / SEAICE_rhoSnow
      rhoSnow2rhoFresh  = ONE / rhoFresh2rhoSnow
      rhoIce2rhoFresh   = SEAICE_rhoIce / rhoConstFresh

C     regularization constants
      area_reg_sq = SEAICE_area_reg * SEAICE_area_reg
      hice_reg_sq = SEAICE_hice_reg * SEAICE_hice_reg

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        iicekey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE area (:,:,bi,bj) = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE heff (:,:,bi,bj) = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE qnet (:,:,bi,bj) = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE qsw  (:,:,bi,bj) = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE tices(:,:,:,bi,bj)=comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif

C array initializations
C =====================

        DO J=1,sNy
         DO I=1,sNx

C NEW VARIABLE NAMES
          NetExistingIceGrowthRate(I,J) = 0.0 _d 0
          IceGrowthRateOpenWater(I,J)   = 0.0 _d 0
          IceGrowthRateFromSurface(I,J) = 0.0 _d 0
          IceGrowthRateMixedLayer(I,J)  = 0.0 _d 0

          EnergyInNewTotalIceVolume(I,J) = 0.0 _d 0
          NetEnergyFluxOutOfOcean(I,J)   = 0.0 _d 0
          ResidualEnergyOutOfOcean(I,J)  = 0.0 _d 0

          PrecipRateOverIceSurfaceToSea(I,J) = 0.0 _d 0

          DO IT=1,SEAICE_multDim
            ticeInMult(I,J,IT)            = 0.0 _d 0
            ticeOutMult(I,J,IT)           = 0.0 _d 0

            F_io_net_mult(I,J,IT)  = 0.0 _d 0
            F_ia_net_mult(I,J,IT)  = 0.0 _d 0
            F_ia_mult(I,J,IT)      = 0.0 _d 0

            QSWI_mult(I,J,IT)      = 0.0 _d 0
            FWsublim_mult(I,J,IT)  = 0.0 _d 0
          ENDDO

          F_io_net(I,J) = 0.0 _d 0
          F_ia_net(I,J) = 0.0 _d 0
          F_ia(I,J)     = 0.0 _d 0

          QSWI(I,J) = 0.0 _d 0
          FWsublim(I,J) = 0.0 _d 0

          QSWO_BELOW_FIRST_LAYER (I,J) = 0.0 _d 0
          QSWO_IN_FIRST_LAYER (I,J) = 0.0 _d 0
          QSWO(I,J) = 0.0 _d 0

          ActualNewTotalVolumeChange(I,J) = 0.0 _d 0
          ActualNewTotalSnowMelt(I,J)     = 0.0 _d 0

          SnowAccOverIce(I,J) = 0.0 _d 0
          SnowAccRateOverIce(I,J) = 0.0 _d 0

          PotSnowMeltRateFromSurf(I,J)    = 0.0 _d 0
          PotSnowMeltFromSurf(I,J)        = 0.0 _d 0
          SnowMeltRateFromSurface(I,J)    = 0.0 _d 0
          SurfHeatFluxConvergToSnowMelt(I,J) = 0.0 _d 0
          SnowMeltFromSurface(I,J)       = 0.0 _d 0

          FreshwaterContribFromSnowMelt(I,J) = 0.0 _d 0
          FreshwaterContribFromIce(I,J) = 0.0 _d 0

          S_a(I,J)       = 0.0 _d 0
          S_a_IGROW(I,J) = 0.0 _d 0
          S_a_IGRML(I,J) = 0.0 _d 0
          S_a_IGRNE(I,J) = 0.0 _d 0

          S_h(I,J)       = 0.0 _d 0
          S_hsnow(I,J)   = 0.0 _d 0

          d_HEFFbyFlood(I,J) = 0.0 _d 0

#ifdef ALLOW_SALT_PLUME
          IceGrowthRateInLeads            (I,J) = 0. _d 0
          leadPlumeFraction               (I,J) = 0. _d 0
          saltPlumeFlux             (I,J,bi,bj) = 0. _d 0
#endif

         ENDDO
        ENDDO

#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          frWtrAtm(I,J,bi,bj)        = 0.0 _d 0
         ENDDO
        ENDDO
#endif

C =====================================================================
C  PART 1: Store ice and snow state on onset + regularize actual
C               snow and ice thickness
C =====================================================================
CAB
        DO J=1,sNy
         DO I=1,sNx
         SIheffNeg(I,J,bi,bj)=d_HEFFbyNEG(I,J,bi,bj)*SINegFac
         SIhsnwNeg(I,J,bi,bj)=d_HSNWbyNEG(I,J,bi,bj)*SINegFac
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx

          tmpscal0 = HEFF(I,J,bi,bj)
          HEFF(I,J,bi,bj) = MAX(ZERO, tmpscal0)
          tmpscal1 = AREA(I,J,bi,bj)
          AREA(I,J,bi,bj) = MAX(ZERO, tmpscal1)

          IF (HEFF(I,J,bi,bj) .LE. ZERO) then
             AREA(I,J, bi,bj)  = ZERO
             HSNOW(I,J, bi,bj) = ZERO
          ELSEIF (AREA(I,J,bi,bj) .LE. ZERO) then
             HEFF(I,J,bi,bj)  = ZERO
             HSNOW(I,J,bi,bj) = ZERO
          ENDIF

          HEFFpreTH(I,J) = HEFF(I,J,bi,bj)
          HSNWpreTH(I,J) = HSNOW(I,J,bi,bj)
          AREApreTH(I,J) = AREA(I,J,bi,bj)

#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayB(I,J) = AREA(I,J,bi,bj)
          DIAGarrayC(I,J) = HEFF(I,J,bi,bj)
          DIAGarrayD(I,J) = HSNOW(I,J,bi,bj)
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(DIAGarrayB,'SIareaPT',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayC,'SIheffPT',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayD,'SIhsnoPT',0,1,3,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_GROWTH_ADX_STORE_MORE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE AREApreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HEFFpreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HSNWpreTH = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

C     COMPUTE ACTUAL ICE/SNOW THICKNESS; USE MIN/MAX VALUES
C     TO REGULARIZE SEAICE_SOLVE4TEMP/d_AREA COMPUTATIONS

        DO J=1,sNy
         DO I=1,sNx
          IF (HEFFpreTH(I,J) .GT. ZERO) THEN
c          regularize AREA with SEAICE_area_reg
           tmpscal1 = SQRT(AREApreTH(I,J)* AREApreTH(I,J) + area_reg_sq)

c          hiceActual calculated with the regularized AREA
           tmpscal2 = HEFFpreTH(I,J) / tmpscal1

c          regularize hiceActual with SEAICE_hice_reg (add lower bound)
c           hiceActual(I,J) = SQRT(tmpscal2 * tmpscal2 + hice_reg_sq)
           hiceActual(I,J) = MAX(0.05 _d 0, tmpscal2)

c          hsnowActual calculated with the regularized AREA (no lower bound)
c          hsnowActual(I,J) = HSNWpreTH(I,J) / tmpscal1
c          actually I do not think we need to regularize this.
           hsnowActual(I,J) = HSNWpreTH(I,J) / AREApreTH(I,J)

c          regularize the inverse of hiceActual by hice_reg
           recip_hiceActual(I,J)  = AREApreTH(I,J) /
     &         sqrt(HEFFpreTH(I,J)*HEFFpreTH(I,J) + hice_reg_sq)

c         Do not regularize when HEFFpreTH = 0
          ELSE
           hiceActual (I,J)       = ZERO
           hsnowActual(I,J)       = ZERO
           recip_hiceActual(I,J)  = ZERO
          ENDIF

         ENDDO
        ENDDO

C =============================================================================
C Part 2: Precipitation as snow or rain over ice
C =============================================================================

        DO J=1,sNy
         DO I=1,sNx
c        if we have ice and the temperature of the ice is below the freezing point
c        then the precip falls and accumulates as snow
          IF (( AREApreTH(I,J)     .GT. ZERO) .AND.
     &        ( TICES(I,J,1,bi,bj) .LT. celsius2k) ) THEN

c use either prescribed snowfall or PRECIP rate
           IF ( snowPrecipFile .NE. ' ' ) THEN
c    rate of snow accumulation in m/s over ice
c                      y [m/s] \approx 1.0 [kg/m^3] / 0.9 [m^3/kg] * x [m/s]
             SnowAccRateOverIce(I,J) = rhoFresh2rhoSnow *
     &               snowPrecip(i,j,bi,bj)

           ELSE
             SnowAccRateOverIce(I,J) = rhoFresh2rhoSnow *
     &           PRECIP(i,j,bi,bj)

           ENDIF

           PrecipRateOverIceSurfaceToSea(I,J) = ZERO

          ELSE
c            The snow/ice surface is not frozen (wet) so the precipitation
c            remains wet and runs into the ocean
             SnowAccRateOverIce(I,J) = ZERO
             PrecipRateOverIceSurfaceToSea(I,J) = PRECIP(i,j,bi,bj)
          ENDIF

c  actual change in snow thickness due to precipitation in units of
c  mean snow thickness
          SnowAccOverIce(I,J) =
     &     SnowAccRateOverIce(I,J) * SEAICE_deltaTtherm * AREApreTH(I,J)

c I,J
         ENDDO
        ENDDO

C =============================================================================
C FIND WIND SPEED
C =============================================================================

        DO j=1,sNy
         DO i=1,sNx
C ocean surface/mixed layer temperature
          TmixLoc(i,j) = theta(i,j,kSurface,bi,bj) + celsius2K
C wind speed from exf
          UG(I,J) = MAX(SEAICE_EPS, wspeed(I,J,bi,bj))
         ENDDO
        ENDDO
#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_GROWTH_ADX_STORE_MORE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE UG                 = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE SnowAccRateOverIce = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE PrecipRateOverIceSurfaceToSea
CADJ &                        = comlev1_bibj,key=iicekey,byte=isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

C =============================================================================
c           Retrieve the air-sea heat and shortwave radiative fluxes
C =============================================================================

        CALL SEAICE_BUDGET_OCEAN(
     I       UG,
     I       TmixLoc,
     O       F_ao, QSWO,
     I       bi, bj, myTime, myIter, myThid )

#if (!defined SEAICE_EXTERNAL_FLUXES && defined ALLOW_AUTODIFF_TAMC)
C     In this case it might be better save the output of
C     seaice_budget_ocean rather than recompute the bulkformulae.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE f_ao = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE qswo = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif
C =============================================================================
C      Calc air-sea fluxes in the uppermost grid cell
C =============================================================================

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
           QSWO_BELOW_FIRST_LAYER(I,J)= QSWO(I,J)*SWFRACB
           QSWO_IN_FIRST_LAYER(I,J)   = QSWO(I,J)*(ONE - SWFRACB)
#else
           QSWO_BELOW_FIRST_LAYER(I,J)= ZERO
           QSWO_IN_FIRST_LAYER(I,J)   = ZERO
#endif
           IceGrowthRateOpenWater(I,J) = QI *
     &        (F_ao(I,J) - QSWO(I,J) + QSWO_IN_FIRST_LAYER(I,J))

         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE salt(:,:,kSurface,bi,bj)
CADJ &                 = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

C =============================================================================
c  calculate heat fluxes within ice (conduction), F_io, and across the
c  ice/atmosphere interface, F_ia
C =============================================================================

C--   Start loop over multi-categories
        DO IT=1,SEAICE_multDim

         DO J=1,sNy
          DO I=1,sNx
c record prior ice surface temperatures
           ticeInMult(I,J,IT)  = TICES(I,J,IT,bi,bj)
           ticeOutMult(I,J,IT) = TICES(I,J,IT,bi,bj)
           TICES(I,J,IT,bi,bj) = ZERO
          ENDDO
         ENDDO

c set relative thickness of ice categories
         pFac = (2.0 _d 0*IT - 1.0 _d 0)*recip_multDim
         pFacSnow = 1. _d 0

c find actual snow and ice thickness within categories categories
         IF ( SEAICE_useMultDimSnow ) pFacSnow=pFac

         DO J=1,sNy
          DO I=1,sNx
            hiceActualMult(I,J,IT)   = hiceActual(I,J) *pFac
            hsnowActualMult(I,J,IT)  = hsnowActual(I,J)*pFacSnow
          ENDDO
         ENDDO
C     multDim-loop
        ENDDO

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_GROWTH_ADX_STORE_MORE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE hiceActualMult = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE hsnowActualMult= comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

c =======================================================================
c  find calculate heat fluxes within ice (conduction) and across the
c  ice/atmosphere interface for each thickness category
c =======================================================================

        DO IT=1,SEAICE_multDim
         CALL SEAICE_SOLVE4TEMP(
     I        UG, hiceActualMult(1,1,IT), hsnowActualMult(1,1,IT),
#ifdef SEAICE_CAP_SUBLIM
         This error is put here intentionally, because SEAICE_CAP_SUBLIM
         cannot be defined together with SEAICE_USE_GROWTH_ADX
     I        latentHeatFluxMaxMult(1,1,IT),
#endif
     I        ticeInMult(1,1,IT),
     O        ticeOutMult(1,1,IT),
     O        F_io_net_mult(1,1,IT),
     O        F_ia_net_mult(1,1,IT),
     O        F_ia_mult(1,1,IT),
     O        QSWI_mult(1,1,IT),
     O        FWsublim_mult(1,1,IT),
     I        bi, bj, myTime, myIter, myThid )
        ENDDO

c =======================================================================
c  record the ice surface temperature in each category
c  and find the average of fluxes across each category

        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx

C     update TICES
            TICES(I,J,IT,bi,bj) = ticeOutMult(I,J,IT)

            F_io_net(I,J) = F_io_net(I,J) +
     &        F_io_net_mult(I,J,IT)*recip_multDim

            F_ia_net(I,J) = F_ia_net(I,J) +
     &        F_ia_net_mult(I,J,IT)*recip_multDim

            F_ia(I,J)  = F_ia(I,J)     +
     &        F_ia_mult(I,J,IT)*recip_multDim

            QSWI(I,J)  = QSWI(I,J) + QSWI_mult(I,J,IT)*recip_multDim

            FWsublim(I,J) = FWsublim(I,J) +
     &         FWsublim_mult(I,J,IT)*recip_multDim

          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     This store is needed to avoid recomputing seaice_solve4temp in AD-mode,
C     because F_ia_net is modified in the block below.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE F_ia_net  = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
c          If there is heat flux convergence at the snow surface,
c          use that energy to melt snow before melting ice.  It is
c          possible that some snow will remain after melting,
c          which will drive F_ia_net to zero, or that all of the
c          snow will be melted, leaving a nonzero F_ia_net to melt
c          some ice.
          F_ia_net_before_snow(I,J) = F_ia_net(I,J)

c         Only continue if there is snow and ice in the cell
          IF (AREApreTH(I,J) .LE. ZERO) THEN
            IceGrowthRateUnderExistingIce(I,J) = 0. _d 0
            IceGrowthRateFromSurface(I,J)      = 0. _d 0
            NetExistingIceGrowthRate(I,J)      = 0. _d 0
          ELSE
c           The growth rate (m/s) beneath existing ice is given by the upward
c           ocean-ice conductive flux, F_io_net, and QI.
            IceGrowthRateUnderExistingIce(I,J) = F_io_net(I,J)*QI

c           The rate at which snow is melted (m/s) because of surface
c           heat flux convergence.  Note, during snow melt, F_ia_net must
c           be negative (implying convergence) to make PSMRFW is positive
            PotSnowMeltRateFromSurf(I,J) = - F_ia_net(I,J)*QS

c           This is the depth of snow (m) that would be melted in one dt
            PotSnowMeltFromSurf(I,J)  =
     &        PotSnowMeltRateFromSurf(I,J) * SEAICE_deltaTtherm

c           If we can melt MORE than is actually there, then the melt
c           rate is reduced so that only that which is there
c           is melted during the time step.  In this case, not all of the
c           heat flux convergence at the surface is used to melt snow.
c           Any remaining energy will melt ice.

c           SurfHeatFluxConvergToSnowMelt is the part of the total heat
c           flux convergence which melts snow.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CCHECK: HSNOW ACTUAL SHOULD NOT BE REGULARIZED FOR THIS
            IF (PotSnowMeltFromSurf(I,J) .GE. hsnowActual(I,J)) THEN

c           Snow melt and melt rate [m] (actual snow thickness)
              SnowMeltFromSurface(I,J)     = hsnowActual(I,J)

c             SnowMeltRateFromSurface(I,J) =
              SnowMeltRateFromSurface(I,J) =
     &           SnowMeltFromSurface(I,J) * recip_deltaTtherm

              SurfHeatFluxConvergToSnowMelt(I,J) =
     &          - hsnowActual(I,J)*recip_QS*recip_deltaTtherm
           ELSE
c             In this case there will be snow remaining after melting.
c             All of the surface heat convergence will be redirected to
c             this effort.
              SnowMeltFromSurface(I,J) = PotSnowMeltFromSurf(I,J)

              SnowMeltRateFromSurface(I,J) =PotSnowMeltRateFromSurf(I,J)

              SurfHeatFluxConvergToSnowMelt(I,J) = F_ia_net(I,J)

           ENDIF

c          Reduce the heat flux convergence available to melt surface
c          ice by the amount used to melt snow
           F_ia_net(I,J) = F_ia_net(I,J) -
     &         SurfHeatFluxConvergToSnowMelt(I,J)

           IceGrowthRateFromSurface(I,J) = F_ia_net(I,J) * QI

c          The total growth rate (m/s) of the existing ice - the rate of
c          new ice accretion at the base less the rate due to surface melt
           NetExistingIceGrowthRate(I,J) =
     &            IceGrowthRateUnderExistingIce(I,J) +
     &            IceGrowthRateFromSurface(I,J)
          ENDIF

         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     Store F_io/ia_net and QSWI to avoid calling seaice_solve4temp in AD-mode
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE F_io_net      = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE F_ia_net      = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE QSWI          = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE theta(:,:,kSurface,bi,bj)
CADJ &                   = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE salt(:,:,kSurface,bi,bj)
CADJ &                   = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */
c     inflection point
        tmpscal0 = 0.4 _d 0
c     steepness/inflection point
        tmpscal1 = 7.0 _d 0/tmpscal0
c     avoid recomputing constant coefficients
        tmpscal2 = STANTON_NUMBER*USTAR_BASE*rhoConst*HeatCapacity_Cp
C     Calculate the heat fluxes from the ocean to the sea ice
        DO J=1,sNy
         DO I=1,sNx
c
c     Bound the ocean temperature to be at or above the freezing point.
          tempFrz = SEAICE_tempFrz0 +
     &              SEAICE_dTempFrz_dS * salt(I,J,kSurface,bi,bj)

          surf_theta = max(theta(I,J,kSurface,bi,bj), tempFrz)

C     MCPHEE_TAPER_FAC = 12.5 (CONSTANT PARAMETER)
          MLTF = ONE + (MCPHEE_TAPER_FAC - ONE)
     &         / ( ONE + EXP( (AREApreTH(I,J) - tmpscal0)*tmpscal1 ) )

C          IF (AREApreTH(I,J) .GT. ZERO) THEN
CC     If ice is present, MixedLayerTurbulenceFactor = 1.0, else 12.50
C           MLTF = ONE
C          ELSE
C           MLTF = MCPHEE_TAPER_FAC
C          ENDIF

          F_mi(I,J) = - tmpscal2 * (surf_theta - tempFrz) * MLTF

          IceGrowthRateMixedLayer (I,J) = F_mi(I,J) * QI

         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     The first two store directives are also needed to avoid calling
C     seaice_solve4temp in AD-mode.
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE NetExistingIceGrowthRate      = comlev1_bibj,
CADJ &     key = iicekey, byte = isbyte
CADJ STORE IceGrowthRateMixedLayer       = comlev1_bibj,
CADJ &     key = iicekey, byte = isbyte
# ifdef SEAICE_GROWTH_ADX_STORE_MORE
CADJ STORE SnowMeltRateFromSurface       = comlev1_bibj,
CADJ &     key = iicekey, byte = isbyte
CADJ STORE IceGrowthRateUnderExistingIce = comlev1_bibj,
CADJ &     key = iicekey, byte = isbyte
CADJ STORE IceGrowthRateFromSurface      = comlev1_bibj,
CADJ &     key = iicekey, byte = isbyte
# endif
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

C       CALCULATE THICKNESS DERIVATIVES of ice (dhdt) and snow (dhsdt)
        DO J=1,sNy
         DO I=1,sNx

          S_h(I,J) =
     &       NetExistingIceGrowthRate(I,J) *  AREApreTH(I,J)
     &     + IceGrowthRateOpenWater(I,J)   * (ONE - AREApreTH(I,J))
     &     + IceGrowthRateMixedLayer(I,J)

c         Both the accumulation and melt rates are in terms
c         of actual snow thickness.  As with ice, multiplying
c         with area converts to mean snow thickness.
          S_hsnow(I,J) =  AREApreTH(I,J) *
     &      ( SnowAccRateOverIce(I,J) - SnowMeltRateFromSurface(I,J))

         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     storing S_h would save one line of recomputation so do not do it
cnostore CADJ STORE S_h = comlev1_bibj, key = iicekey, byte = isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_SALT_PLUME
# ifdef SALT_PLUME_IN_LEADS
C     avoid repetitive division by a constant
        tmpscal0 = 5. _d 0 / (ONE - SPinflectionPoint)
C     Now that we know the thickness tendency terms, we can calculate
C     the saltPlumeFlux
        DO J=1,sNy
         DO I=1,sNx

c     It is assumed that ice production in leads can generate
c     salt plumes. The fraction of the salt sent to the plume package
c     from ice produced in leads (defined as the open water
c     fraction) is a nonlinear function of ice concentration, AREA.
c
c     Specifically, function is a logistic curve (sigmoid) with a range and
c     domain {0,1}. The function, f(AREA), has a single free parameter,
c     SEAICE_plumeInflectionPoint, the inflection point of the curve.
c     By construction, the function has the following properties:
c     f(1) \approx 1.0
c     f(SEAICE_plumeInflectionPoint) = 0.5
c     f(0) \approx 0.0 (when SEAICE_plumeInflectionPoint \geq 0.5)
c     f(0) > 0.0 (when SEAICE_plumeInflectionPoint < 0.5)
c
c     As AREA --> 1, the open water fraction occurs in narrow leads,
c     new ice production become spatially non-uniform, and the assumptions
c     motivating KPP no longer hold. To treat overturning in a more
c     physically realistic way, the salt produced in the leads should
c     be sent to depth via the plume package. To assure only narrow leads
c     generate plumes, choose a SEAICE_plumeInflectionPoint of > 0.8.

c     Ensure that there is already ice present or that the total ice
c     ice tendency term is positive.  We do not want to release
c     salt if sea ice is not established in the cell.

          IF (AREApreTH(I,J) .GT. ZERO .OR. S_h(I,J) .GT. ZERO ) THEN

           leadPlumeFraction(I,J) = ONE
     &          / ( ONE + EXP( ( SPinflectionPoint - AREApreTH(I,J) )
     &                         * tmpscal0 )
     &            )

c     Only consider positive ice growth rate in leads for salt production
           IceGrowthRateInLeads(I,J) = max( ZERO,
     &          (ONE - AREApreTH(I,J)) * IceGrowthRateOpenWater(I,J))

           saltPlumeFlux(I,J,bi,bj) = leadPlumeFraction(I,J) *
     &          HEFFM(I,J,bi,bj)*IceGrowthRateInLeads(I,J)*
     &          ICE2WATR*rhoConstFresh*
     &          (salt(I,J,kSurface,bi,bj) - SEAICE_salt0)
          ELSE

           saltPlumeFlux(I,J,bi,bj) = ZERO

          ENDIF

         ENDDO
        ENDDO
#endif /* SALT_PLUME_IN_LEADS */
#endif /* ALLOW_SALT_PLUME */

c       Caculate dA/dt (S_a)
        DO J=1,sNy
         DO I=1,sNx

          S_a(I,J) =  0. _d 0

c     Caculate the ice area growth rate from the open water fluxes.
c     First, determine whether the open water growth rate is positive or
c     negative.  If positive, make sure that ice is present or that the
c     net ice thickness growth rate is positive before extending ice cover

c     this is the geometric term: Area/(2*Heff),
c     with Area/Heff regularized as Area/(Heff^2 + epsilon^2)
c     Area/(Heff^2 + ep^2) = recip_hiceActual
c     epsilon = SEAICE_hice_reg

          tmpscal0 = 0.5 _d 0 * recip_hiceActual(I,J)

C    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C         /* IceGrowthRateOpenWater */

          S_a_IGROW(I,J) = ZERO

c         Expand ice cover if the open water growth rate is positive
          IF ( IceGrowthRateOpenWater(I,J) .GT. ZERO) THEN
           IF ( AREApreTH(I,J) .GT. ZERO   .OR.
     &          S_h(I,J)       .GT. ZERO )  THEN
c     Determine which hemisphere for hemisphere-dependent
c     "lead closing variable", HO
            IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
             S_a_IGROW(I,J) = (ONE - AREApreTH(I,J)) *
     &            IceGrowthRateOpenWater(I,J)*recip_HO_south
            ELSE
             S_a_IGROW(I,J) = (ONE - AREApreTH(I,J)) *
     &            IceGrowthRateOpenWater(I,J)*recip_HO
            ENDIF
           ENDIF

C    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
c         Contract ice cover if the open water growth rate is negative
          ELSE
           S_a_IGROW(I,J) = tmpscal0 *
     &          IceGrowthRateOpenWater(I,J) * (ONE - AREApreTH(I,J))
          ENDIF

          S_a(I,J) = S_a(I,J) + S_a_IGROW(I,J)

C    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
c         /* IceGrowthRateMixedLayer */
C         Contract ice if the IceGrowthRateMixedLayer is negative
          S_a_IGRML(I,J) = ZERO

          IF ( IceGrowthRateMixedLayer(I,J) .LE. ZERO)  THEN
           S_a_IGRML(I,J) = tmpscal0 * IceGrowthRateMixedLayer(I,J)
          ENDIF

          S_a(I,J) = S_a(I,J) + S_a_IGRML(I,J)

c
C    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C         Contract ice if the NetExistingIceGrowthRate is negative
C         /* NetExistingIceGrowthRate */
          S_a_IGRNE(I,J) = ZERO
          IF ( NetExistingIceGrowthRate(I,J) .LE. ZERO .AND.
     &         HEFFpreTH(I,J)                .GT. ZERO ) THEN

           S_a_IGRNE(I,J) =
     &       tmpscal0 * NetExistingIceGrowthRate(I,J) * AREApreTH(I,J)

          ENDIF

          S_a(I,J) = S_a(I,J) + S_a_IGRNE(I,J)

         ENDDO
        ENDDO

C     Update the area, heff, and hsnow
        DO J=1,sNy
         DO I=1,sNx
          HEFF(I,J,bi,bj)  = HEFFpreTH(I,J) +
     &         SEAICE_deltaTtherm * S_h(I,J) * HEFFM(I,J,bi,bj)

          AREA(I,J,bi,bj)  = AREApreTH(I,J) +
     &         SEAICE_deltaTtherm * S_a(I,J) * HEFFM(I,J,bi,bj)

          HSNOW(I,J,bi,bj) = HSNWpreTH(I,J) +
     &         SEAICE_deltaTtherm * S_hsnow(I,J) * HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE AREA (:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HEFF (:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
c     Bound area, heff, and hsnow
          tmpscal0 = AREA(I,J,bi,bj)
          AREA(I,J,bi,bj)  = MIN(ONE,  tmpscal0)
          tmpscal1 = AREA(I,J,bi,bj)
          AREA(I,J,bi,bj)  = MAX(ZERO, tmpscal1)
          tmpscal2 = HEFF(I,J,bi,bj)
          HEFF(I,J,bi,bj)  = MAX(ZERO, tmpscal2)
          tmpscal3 = HSNOW(I,J,bi,bj)
          HSNOW(I,J,bi,bj) = MAX(ZERO, tmpscal3)

c     Sanity checks
          IF ( HEFF(I,J,bi,bj) .LE. ZERO .OR.
     &         AREA(I,J,bi,bj) .LE. ZERO ) THEN

           AREA(I,J,bi,bj)       = 0. _d 0
           HEFF(I,J,bi,bj)       = 0. _d 0
           HSNOW(I,J,bi,bj)      = 0. _d 0

          ENDIF

         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx

c         THE EFFECTIVE SHORTWAVE HEATING RATE
#ifdef SHORTWAVE_HEATING
          QSW(I,J,bi,bj)  =
     &         QSWI(I,J)  * (      AREApreTH(I,J)) +
     &         QSWO(I,J)  * (ONE - AREApreTH(I,J))
#else
          QSW(I,J,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO

        IF(SEAICEuseFlooding) THEN
#ifdef ALLOW_AUTODIFF_TAMC
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CADJ STORE HEFF (:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_AUTODIFF_TAMC */
C     convert snow to ice if submerged.
         DO J=1,sNy
          DO I=1,sNx
           tmpscal0           = ( HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce )*recip_rhoConst
           d_HEFFbyFlood(I,J) = MAX(0. _d 0,tmpscal0-HEFF(I,J,bi,bj))
           HEFF(I,J,bi,bj)    = HEFF(I,J,bi,bj) + d_HEFFbyFlood(I,J)
           HSNOW(I,J,bi,bj)   = HSNOW(I,J,bi,bj)
     &                           - d_HEFFbyFlood(I,J)*ICE2SNOW
          ENDDO
         ENDDO
c     SEAICEuseFlooding
        ENDIF

        DO J=1,sNy
         DO I=1,sNx

c     The actual ice volume change over the time step
          ActualNewTotalVolumeChange(I,J) =
     &         HEFF(I,J,bi,bj) - HEFFpreTH(I,J)

c     The net average snow thickness melt that is actually realized. e.g.
c     hsnow_orig  = 0.25 m (e.g. 1 m of ice over a cell 1/4 covered in snow)
c     hsnow_new   = 0.20 m
c     snow accum  = 0.05 m
c     melt = 0.25 + 0.05 - 0.2 = 0.1 m

c     since this is in mean snow thickness it might have been  0.4 of actual
c     snow thickness over the 1/4 of the cell which is ice covered.
          ActualNewTotalSnowMelt(I,J) =
     &         HSNWpreTH(I,J) +
     &         SnowAccOverIce(I,J) -
     &         HSNOW(I,J,bi,bj)

c     The energy required to melt or form the new ice volume
          EnergyInNewTotalIceVolume(I,J) =
     &         ActualNewTotalVolumeChange(I,J)*recip_QI
c

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
     &               ( AREApreTH(I,J) *
     &                 (F_ia_net(I,J) + F_io_net(I,J) + QSWI(I,J))
     &         +     ( ONE - AREApreTH(I,J)) *  F_ao(I,J))

c     THE QUANTITY OF HEAT WHICH IS THE RESIDUAL TO THE QUANTITY OF
c     ML temperature.  If the net energy flux is exactly balanced by the
c     latent energy of fusion in the new ice created then we will not
c     change the ML temperature at all.

          ResidualEnergyOutOfOcean(I,J) =
     &             NetEnergyFluxOutOfOcean(I,J) -
     &             EnergyInNewTotalIceVolume(I,J)

C     NOW FORMULATE QNET
C     THIS QNET DETERMINES THE TEMPERATURE CHANGE
C     QNET IS A DEPTH AVERAGED HEAT FLUX FOR THE OCEAN COLUMN

          QNET(I,J,bi,bj) =
     &             ResidualEnergyOutOfOcean(I,J) * recip_deltaTtherm

c     Like snow melt, if there is melting, this quantity is positive.
c     The change of freshwater content is per unit area over the entire
c     cell, not just over the ice covered bits.  This term is only used
c     to calculate freshwater fluxes for the purpose of changing the
c     salinity of the liquid cell.  In the case of non-zero ice salinity,
c     the amount of freshwater is reduced by the ratio of ice salinity
c     to water cell salinity.
          IF  (salt(I,J,kSurface,bi,bj) .GE. SEAICE_salt0 .AND.
     &         salt(I,J,kSurface,bi,bj) .GT. 0. _d 0) THEN

           FreshwaterContribFromIce(I,J) =
     &            - ActualNewTotalVolumeChange(I,J) *
     &              rhoIce2rhoFresh
C     &             * (ONE - SEAICE_salt0/salt(I,J,kSurface,bi,bj))

          ELSE
C     If the liquid cell has a lower salinity than the specified
c     salinity of sea ice then assume the sea ice is completely fresh
           FreshwaterContribFromIce(I,J) =
     &             -ActualNewTotalVolumeChange(I,J) *
     &              rhoIce2rhoFresh
          ENDIF
CAB
          tmpscal3 = max( 0. _d 0,
     &                    min(SEAICE_salt0,salt(I,J,kSurface,bi,bj)) )
CAB   Salt in ice + ridgid - ice from snow flood
          tmpscal2 = (ActualNewTotalVolumeChange(I,J)+
     &                   SIheffNeg(I,J,bi,bj)+
     &                  0.0 )
     &                   * tmpscal3
     &                   * HEFFM(I,J,bi,bj)
     &                   * recip_deltaTtherm * SEAICE_rhoIce

CAB   test for getting the lines
          saltflux(I,J,bi,bj) = tmpscal2
CAB

c     The freshwater contribution from snow comes only in the form of melt
c     unlike ice, which takes freshwater upon growth and yields freshwater
c     upon melt.  This is why the the actual new average snow melt was
c     determined. In m/m^2 over the entire cell.
          FreshwaterContribFromSnowMelt(I,J) =
     &         ActualNewTotalSnowMelt(I,J)*rhoSnow2rhoFresh

c    This seems to be in m/s, original time level 2 for area
c    Only the precip and evap need to be area weighted.  The runoff
c    and freshwater contribs from ice and snow melt are already mean
c    weighted
          EmPmR(I,J,bi,bj)  = HEFFM(I,J,bi,bj)*(
     &         ( EVAP(I,J,bi,bj) - PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
     &         - PrecipRateOverIceSurfaceToSea(I,J)*AREApreTH(I,J)
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif
     &         - (FreshwaterContribFromIce(I,J) +
     &            FreshwaterContribFromSnowMelt(I,J))
     &           *recip_deltaTtherm ) * rhoConstFresh
     &         +( SIheffNeg(I,J,bi,bj)*SEAICE_rhoIce
     &          + SIhsnwNeg(I,J,bi,bj)*SEAICE_rhoSnow)
     &          *recip_deltaTtherm * HEFFM(I,J,bi,bj)

CAB   Budget test , adding the energy stored in snow melt to qnet
C     Moved bunch of these to line 780

          SItflux(I,J,bi,bj) = 0.0 +
     &         (AREApreTH(I,J)
     &         *(F_ia_net(I,J)
     &         + F_io_net(I,J) + QSWI(I,J))
     &         +( ONE - AREApreTH(I,J)) *  F_ao(I,J))
     &         *HEFFM(I,J,bi,bj)
     &         +(SIheffNeg(I,J,bi,bj)*recip_QI
     &         +( SIhsnwNeg(I,J,bi,bj) - ActualNewTotalSnowMelt(I,J)
     &          + SnowAccOverIce(I,J) )*recip_QS)
     &         * recip_deltaTtherm * HEFFM(I,J,bi,bj)

          SIaaflux(I,J,bi,bj) =  -EmPmR(I,J,bi,bj)
     &         * HeatCapacity_Cp *theta(I,J,kSurface,bi,bj)

          SIatmFW(I,J,bi,bj) = HEFFM(I,J,bi,bj)*(
     &         EVAP(I,J,bi,bj)*( ONE - AREApreTH(I,J) )
     &         - PRECIP(I,J,bi,bj)
#ifdef ALLOW_RUNOFF
     &          - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &           )*rhoConstFresh
     &     +    FWsublim(i,j)* SEAICE_rhoIce * recip_deltaTtherm
CAB

         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Sea Ice Load on the sea surface.
C     =================================
        IF ( useRealFreshWaterFlux ) THEN
         DO J=1,sNy
          DO I=1,sNx
#ifdef SEAICE_CAP_ICELOAD
           tmpscal1 = HEFF(I,J,bi,bj)*SEAICE_rhoIce
     &              + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
           tmpscal2 = MIN(tmpscal1,heffTooHeavy*rhoConst)
#else
           tmpscal2 = HEFF(I,J,bi,bj)*SEAICE_rhoIce
     &              + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
#endif
           sIceLoad(i,j,bi,bj) = tmpscal2
          ENDDO
         ENDDO
        ENDIF

#ifdef SEAICE_DEBUG
        DO j=1,sNy
         DO i=1,sNx

          IF ( (i .EQ. SEAICE_debugPointI)   .and.
     &         (j .EQ. SEAICE_debugPointJ) ) THEN

           print *,'ifice: myTime,myIter:',myTime,myIter

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j --------------  ',i,j

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j F(mi ao), rHIA  ',
     &          i,j, F_mi(i,j), F_ao(i,j),
     &          recip_hiceActual(i,j)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j Fi(a,ant2/1 ont)',
     &          i,j, F_ia(i,j),
     &          F_ia_net_before_snow(i,j),
     &          F_ia_net(i,j),
     &          F_io_net(i,j)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j AREA2/1 HEFF2/1 ',i,j,
     &          AREApreTH(I,J),
     &          AREA(i,j,bi,bj),
     &          HEFFpreTH(I,J),
     &          HEFF(i,j,bi,bj)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j HSNOW2/1 TMX    ',i,j,
     &          HSNWpreTH(I,J),
     &          HSNOW(I,J,bi,bj),
     &          theta(I,J,kSurface,bi,bj)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j TI ATP LWD      ',i,j,
     &          TICES(i,j,1, bi,bj) - celsius2k,
     &          ATEMP(i,j,bi,bj) - celsius2k,
     &          LWDOWN(i,j,bi,bj)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j S_a(tot,OW,ML,NE',i,j,
     &          S_a(i,j),
     &          S_a_IGROW(I,J),
     &          S_a_IGRML(I,J),
     &          S_a_IGRNE(I,J)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j S_a S_h S_hsnow ',i,j,
     &          S_a(i,j),
     &          S_h(i,j),
     &          S_hsnow(i,j)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j IGR(ML OW ICE)  ',i,j,
     &          IceGrowthRateMixedLayer(i,j),
     &          IceGrowthRateOpenWater(i,j),
     &          NetExistingIceGrowthRate(i,j)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j IVC(A ENIN)     ',i,j,
     &          ActualNewTotalVolumeChange(i,j),
     &          EnergyInNewTotalIceVolume(i,j)
c     &          ExpectedIceVolumeChange(i,j),

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j EF(NOS RE) QNET ',i,j,
     &          NetEnergyFluxOutOfOcean(i,j),
     &          ResidualEnergyOutOfOcean(i,j),
     &          QNET(I,J,bi,bj)

           print  '(A,2i4,3(1x,1P3E15.4))',
     &          'ifice i j QSW QSWO QSWI   ',i,j,
     &          QSW(i,j,bi,bj),
     &          QSWO(i,j),
     &          QSWI(i,j)

c                  print  '(A,2i4,3(1x,1P3E15.4))',
c     &                 'ifice i j SW(BML IML SW)  ',i,j,
c     &                 QSW_absorb_below_first_layer(i,j),
c     &                 QSW_absorb_in_first_layer(i,j),
c     &                 SWFRACB

c                  print  '(A,2i4,3(1x,1P3E15.4))',
c     &                 'ifice i j ptc(to, qsw, oa)',i,j,
c     &                 PredTempChange(i,j),
c     &                 PredTempChangeFromQSW (i,j),
c     &                 PredTempChangeFromOA_MQNET(i,j)

c                  print  '(A,2i4,3(1x,1P3E15.4))',
c     &                 'ifice i j ptc(fion,ian,ia)',i,j,
c     &                 PredTempChangeFromF_IO_NET(i,j),
c     &                 PredTempChangeFromF_IA_NET(i,j),
c     &                 PredTempChangeFromFIA(i,j)

c                  print  '(A,2i4,3(1x,1P3E15.4))',
c     &                 'ifice i j ptc(niv)        ',i,j,
c     &                 PredTempChangeFromNewIceVol(i,j)

           print  '(A,2i4,3(1x,1P3E15.4))',
     &          'ifice i j EmPmR EVP PRE RU',i,j,
     &          EmPmR(I,J,bi,bj),
     &          EVAP(I,J,bi,bj),
     &          PRECIP(I,J,bi,bj),
     &          RUNOFF(I,J,bi,bj)

           print  '(A,2i4,3(1x,1P3E15.4))',
     &          'ifice i j PRROIS,SAOI(R .)',i,j,
     &          PrecipRateOverIceSurfaceToSea(I,J),
     &          SnowAccRateOverIce(I,J),
     &          SnowAccOverIce(I,J)

           print  '(A,2i4,4(1x,1P3E15.4))',
     &          'ifice i j SM(PM PMR . .R) ',i,j,
     &          PotSnowMeltFromSurf(I,J),
     &          PotSnowMeltRateFromSurf(I,J),
     &          SnowMeltFromSurface(I,J),
     &          SnowMeltRateFromSurface(I,J)

           print  '(A,2i4,4(1x,1P3E15.4))',
     &          'ifice i j TotSnwMlt       ',i,j,
     &          ActualNewTotalSnowMelt(I,J)
c     &                 ExpectedSnowVolumeChange(I,J)

           print  '(A,2i4,4(1x,1P3E15.4))',
     &          'ifice i j fw(CFICE, CFSM) ',i,j,
     &          FreshwaterContribFromIce(I,J),
     &          FreshwaterContribFromSnowMelt(I,J)

           print  '(A,2i4,2(1x,1P3E15.4))',
     &          'ifice i j --------------  ',i,j

          ENDIF

         ENDDO
        ENDDO
#endif /* SEAICE_DEBUG */

C close bi,bj loops
       ENDDO
      ENDDO

      _BEGIN_MASTER( myThid )
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C these diags need to be done outside of the bi,bj loop so that
C we may do potential global mean adjustement to them consistently.
CAB
CAB        CALL DIAGNOSTICS_FILL(SItflux,

C        CALL DIAGNOSTICS_FILL(SIatmQnt,
C     &        'SIatmQnt',0,1,0,1,1,myThid)
C SIatmFW follows the same convention as empmr -- SIatmFW diag does not
CAB
C       CALL DIAGNOSTICS_FILL(SIacSubl,
C     &        'SIacSubl',0,1,3,bi,bj,myThid)

       CALL DIAGNOSTICS_FILL(SItflux, 'SItflux ',0,1,0,1,1,myThid)

       CALL DIAGNOSTICS_FILL(SIaaflux,'SIaaflux',0,1,0,1,1,myThid)

       tmpscal1= - 1. _d 0
       CALL DIAGNOSTICS_SCALE_FILL(SIatmFW,
     &      tmpscal1,1,'SIatmFW ',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C      WRITE(msgBuf,'(a,a,e24.17)') 'SINegFac ',
C     &       ' = ', SINegFac
C      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                      SQUEEZE_RIGHT, myThid )
C
      _END_MASTER( myThid )

#else  /* ALLOW_EXF and ALLOW_ATM_TEMP */
      STOP 'SEAICE_GROWTH_ADX not compiled with EXF and ALLOW_ATM_TEMP'
#endif /* ALLOW_EXF and ALLOW_ATM_TEMP */
#endif /* SEAICE_USE_GROWTH_ADX */

      RETURN
      END
