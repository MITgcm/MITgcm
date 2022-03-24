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

CBOP
C     !ROUTINE: SEAICE_GROWTH
C     !INTERFACE:
      SUBROUTINE SEAICE_GROWTH( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE seaice_growth
C     | o Updata ice thickness and snow depth
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

#ifndef SEAICE_USE_GROWTH_ADX
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
C    Within the thermodynamic computation all stocks, except HSNOW,
C      are in 'effective ice meters' units, and >0 implies more ice.
C    This holds for stocks due to ocean and atmosphere heat,
C      at the outset of 'PART 2: determine heat fluxes/stocks'
C      and until 'PART 7: determine ocean model forcing'
C    This strategy minimizes the need for multiplications/divisions
C      by ice fraction, heat capacity, etc. The only conversions that
C      occurs are for the HSNOW (in effective snow meters) and
C      PRECIP (fresh water m/s).
C
C HEFF is effective Hice thickness (m3/m2)
C HSNOW is Heffective snow thickness (m3/m2)
C HSALT is Heffective salt content (g/m2)
C AREA is the seaice cover fraction (0<=AREA<=1)
C Q denotes heat stocks -- converted to ice stocks (m3/m2) early on
C
C For all other stocks/increments, such as d_HEFFbyATMonOCN
C or a_QbyATM_cover, the naming convention is as follows:
C    The prefix 'a_' means available, the prefix 'd_' means delta
C       (i.e. increment), and the prefix 'r_' means residual.
C    The suffix '_cover' denotes a value for the ice covered fraction
C       of the grid cell, whereas '_open' is for the open water fraction.
C    The main part of the name states what ice/snow stock is concerned
C       (e.g. QbyATM or HEFF), and how it is affected (e.g. d_HEFFbyATMonOCN
C       is the increment of HEFF due to the ATMosphere extracting heat from the
C       OCeaN surface, or providing heat to the OCeaN surface).

C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface
C     IT :: ice thickness category index (MULTICATEGORIES and ITD code)
      INTEGER IT
C     msgBuf      :: Informational/error message buffer
#ifdef ALLOW_BALANCE_FLUXES
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#elif (defined (SEAICE_DEBUG))
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*12 msgBufForm
#endif
C     constants
      _RL tempFrz, ICE2SNOW, SNOW2ICE
      _RL QI, QS, recip_QI
      _RL lhSublim

C conversion factors to go from Q (W/m2) to HEFF (ice meters)
      _RL convertQ2HI, convertHI2Q
C conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      _RL convertPRECIP2HI, convertHI2PRECIP
C     Factor by which we increase the upper ocean friction velocity (u*) when
C     ice is absent in a grid cell  (dimensionless)
      _RL MixedLayerTurbulenceFactor

C     wind speed square
      _RL SPEED_SQ

C     Regularization values squared
      _RL area_reg_sq, hice_reg_sq
C     pathological cases thresholds
      _RL heffTooHeavy
C     local copy of surface layer thickness in meters
      _RL dzSurf

C     Helper variables: reciprocal of some constants
      _RL recip_multDim
      _RL recip_deltaTtherm
      _RL recip_rhoIce
C     local value (=1/HO or 1/HO_south)
      _RL recip_HO
C     local value (=1/ice thickness)
      _RL recip_HH
#ifndef SEAICE_ITD
C     facilitate multi-category snow implementation
      _RL pFac, pFacSnow
#endif
C     additional factors accounting for a non-uniform sea-ice PDF
      _RL denominator, recip_denominator, areaPDFfac

C     temporary variables available for the various computations
      _RL tmpscal0, tmpscal1, tmpscal2, tmpscal3, tmpscal4

#ifdef ALLOW_SITRACER
      INTEGER iTr
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8   diagName
#endif
#ifdef SEAICE_GREASE
      INTEGER iTrGrease
      _RL greaseDecayTime
      _RL greaseNewFrazil
      _RL THIRD
      PARAMETER (THIRD = 1.0 _d 0 / 3.0 _d 0)
#endif
#endif /* ALLOW_SITRACER */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iicekey
#endif

C==   local arrays ==
C--   TmixLoc        :: ocean surface/mixed-layer temperature (in K)
      _RL TmixLoc       (1:sNx,1:sNy)

#ifndef SEAICE_ITD
C     actual ice thickness (with upper and lower limit)
      _RL heffActual          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hsnowActual         (1:sNx,1:sNy)
#endif
C     actual ice thickness (with lower limit only) Reciprocal
      _RL recip_heffActual    (1:sNx,1:sNy)

C     AREA_PRE :: hold sea-ice fraction field before any seaice-thermo update
      _RL AREApreTH           (1:sNx,1:sNy)
      _RL HEFFpreTH           (1:sNx,1:sNy)
      _RL HSNWpreTH           (1:sNx,1:sNy)
#ifdef SEAICE_ITD
      _RL AREAITDpreTH        (1:sNx,1:sNy,1:nITD)
      _RL HEFFITDpreTH        (1:sNx,1:sNy,1:nITD)
      _RL HSNWITDpreTH        (1:sNx,1:sNy,1:nITD)
      _RL areaFracFactor      (1:sNx,1:sNy,1:nITD)
#endif

C     wind speed
      _RL UG                  (1:sNx,1:sNy)

C     temporary variables available for the various computations
      _RL tmparr1             (1:sNx,1:sNy)

      _RL ticeInMult          (1:sNx,1:sNy,nITD)
      _RL ticeOutMult         (1:sNx,1:sNy,nITD)
      _RL heffActualMult      (1:sNx,1:sNy,nITD)
      _RL hsnowActualMult     (1:sNx,1:sNy,nITD)
#ifdef SEAICE_ITD
      _RL recip_heffActualMult(1:sNx,1:sNy,nITD)
#endif
      _RL a_QbyATMmult_cover  (1:sNx,1:sNy,nITD)
      _RL a_QSWbyATMmult_cover(1:sNx,1:sNy,nITD)
      _RL a_FWbySublimMult    (1:sNx,1:sNy,nITD)
#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
      _RL greaseLayerThick    (1:sNx,1:sNy)
      _RL d_HEFFbyGREASE      (1:sNx,1:sNy)
      _RL uRelW               (1:sNx,1:sNy)
      _RL vRelW               (1:sNx,1:sNy)
#endif
#ifdef SEAICE_ITD
      _RL r_QbyATMmult_cover  (1:sNx,1:sNy,nITD)
      _RL r_FWbySublimMult    (1:sNx,1:sNy,nITD)
C for lateral melt parameterization:
      _RL latMeltFrac         (1:sNx,1:sNy,nITD)
      _RL latMeltRate         (1:sNx,1:sNy,nITD)
      _RL floeAlpha
      _RL floeDiameter
      _RL floeDiameterMin
      _RL floeDiameterMax
#endif

C     a_QbyATM_cover :: available heat (in W/m^2) due to the interaction of
C             the atmosphere and the ocean surface - for ice covered water
C     a_QbyATM_open  :: same but for open water
C     r_QbyATM_cover :: residual of a_QbyATM_cover after freezing/melting processes
C     r_QbyATM_open  :: same but for open water
      _RL a_QbyATM_cover      (1:sNx,1:sNy)
      _RL a_QbyATM_open       (1:sNx,1:sNy)
      _RL r_QbyATM_cover      (1:sNx,1:sNy)
      _RL r_QbyATM_open       (1:sNx,1:sNy)
C     a_QSWbyATM_open   - short wave heat flux over ocean in W/m^2
C     a_QSWbyATM_cover  - short wave heat flux under ice in W/m^2
      _RL a_QSWbyATM_open     (1:sNx,1:sNy)
      _RL a_QSWbyATM_cover    (1:sNx,1:sNy)
C     a_QbyOCN :: available heat (in W/m^2) due to the
C             interaction of the ice pack and the ocean surface
C     r_QbyOCN :: residual of a_QbyOCN after freezing/melting
C             processes have been accounted for
      _RL a_QbyOCN            (1:sNx,1:sNy)
      _RL r_QbyOCN            (1:sNx,1:sNy)

C     The change of mean ice thickness due to turbulent ocean-sea ice heat fluxes
      _RL d_HEFFbyOCNonICE    (1:sNx,1:sNy)

C     The sum of mean ice thickness increments due to atmospheric fluxes over
C     the open water fraction and ice-covered fractions of the grid cell
      _RL d_HEFFbyATMonOCN    (1:sNx,1:sNy)
C     The change of mean ice thickness due to flooding by snow
      _RL d_HEFFbyFLOODING    (1:sNx,1:sNy)

C     The mean ice thickness increments due to atmospheric fluxes over the open
C     water fraction and ice-covered fractions of the grid cell, respectively
      _RL d_HEFFbyATMonOCN_open(1:sNx,1:sNy)
      _RL d_HEFFbyATMonOCN_cover(1:sNx,1:sNy)

      _RL d_HSNWbyATMonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyOCNonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyRAIN        (1:sNx,1:sNy)

      _RL d_HFRWbyRAIN        (1:sNx,1:sNy)

C     a_FWbySublim :: fresh water flux implied by latent heat of
C                     sublimation to atmosphere, same sign convention
C                     as EVAP (positive upward)
      _RL a_FWbySublim        (1:sNx,1:sNy)
      _RL r_FWbySublim        (1:sNx,1:sNy)
      _RL d_HEFFbySublim      (1:sNx,1:sNy)
      _RL d_HSNWbySublim      (1:sNx,1:sNy)

#ifdef SEAICE_CAP_SUBLIM
C     The latent heat flux which will sublimate all snow and ice
C     over one time step
      _RL latentHeatFluxMax   (1:sNx,1:sNy)
      _RL latentHeatFluxMaxMult(1:sNx,1:sNy,nITD)
#endif

#ifdef SEAICE_ITD
      _RL d_HEFFbySublim_ITD         (1:sNx,1:sNy,1:nITD)
      _RL d_HSNWbySublim_ITD         (1:sNx,1:sNy,1:nITD)
      _RL d_HEFFbyOCNonICE_ITD       (1:sNx,1:sNy,1:nITD)
      _RL d_HSNWbyATMonSNW_ITD       (1:sNx,1:sNy,1:nITD)
      _RL d_HEFFbyATMonOCN_ITD       (1:sNx,1:sNy,1:nITD)
      _RL d_HEFFbyATMonOCN_cover_ITD (1:sNx,1:sNy,1:nITD)
      _RL d_HEFFbyATMonOCN_open_ITD  (1:sNx,1:sNy,1:nITD)
      _RL d_HSNWbyRAIN_ITD           (1:sNx,1:sNy,1:nITD)
      _RL d_HSNWbyOCNonSNW_ITD       (1:sNx,1:sNy,1:nITD)
      _RL d_HEFFbyFLOODING_ITD       (1:sNx,1:sNy,1:nITD)
#endif

#ifdef ALLOW_DIAGNOSTICS
C ICE/SNOW stocks tendencies associated with the various melt/freeze processes
      _RL d_AREAbyATM         (1:sNx,1:sNy)
      _RL d_AREAbyOCN         (1:sNx,1:sNy)
      _RL d_AREAbyICE         (1:sNx,1:sNy)
C     Helper variables for diagnostics
      _RL DIAGarrayA    (1:sNx,1:sNy)
      _RL DIAGarrayB    (1:sNx,1:sNy)
      _RL DIAGarrayC    (1:sNx,1:sNy)
      _RL DIAGarrayD    (1:sNx,1:sNy)
#endif /* ALLOW_DIAGNOSTICS */

      _RL SItflux     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIatmQnt    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SIatmFW     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_BALANCE_FLUXES
      _RL FWFsiTile(nSx,nSy)
      _RL FWFsiGlob
      _RL HFsiTile(nSx,nSy)
      _RL HFsiGlob
      _RL FWF2HFsiTile(nSx,nSy)
      _RL FWF2HFsiGlob
#endif

#ifdef ALLOW_SALT_PLUME
      _RL localSPfrac          (1:sNx,1:sNy)
#ifdef SALT_PLUME_IN_LEADS
      _RL leadPlumeFraction    (1:sNx,1:sNy)
      _RL IceGrowthRateInLeads (1:sNx,1:sNy)
#endif /* SALT_PLUME_IN_LEADS */
#endif /* ALLOW_SALT_PLUME */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C ===================================================================
C =================PART 0: constants and initializations=============
C ===================================================================

      IF ( usingPCoords ) THEN
       kSurface = Nr
       dzSurf   = drF(kSurface)*recip_rhoConst*recip_gravity
      ELSE
       kSurface = 1
       dzSurf   = drF(kSurface)
      ENDIF

C     avoid unnecessary divisions in loops
      recip_multDim        = SEAICE_multDim
      recip_multDim        = ONE / recip_multDim
C     above/below: double/single precision calculation of recip_multDim
c     recip_multDim        = 1./float(SEAICE_multDim)
      recip_deltaTtherm = ONE / SEAICE_deltaTtherm
      recip_rhoIce      = ONE / SEAICE_rhoIce

C     Cutoff for iceload
      heffTooHeavy = dzSurf * 0.2 _d 0
C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SEAICE_rhoIce/SEAICE_rhoSnow
      SNOW2ICE     = ONE / ICE2SNOW

C     HEAT OF FUSION OF ICE (J/m^3)
      QI           = SEAICE_rhoIce*SEAICE_lhFusion
      recip_QI     = ONE / QI
C     HEAT OF FUSION OF SNOW (J/m^3)
      QS           = SEAICE_rhoSnow*SEAICE_lhFusion

C     ICE LATENT HEAT CONSTANT
      lhSublim = SEAICE_lhEvap + SEAICE_lhFusion

C     regularization constants
      area_reg_sq = SEAICE_area_reg * SEAICE_area_reg
      hice_reg_sq = SEAICE_hice_reg * SEAICE_hice_reg

C conversion factors to go from Q (W/m2) to HEFF (ice meters)
      convertQ2HI=SEAICE_deltaTtherm/QI
      convertHI2Q = ONE/convertQ2HI
C conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      convertPRECIP2HI=SEAICE_deltaTtherm*rhoConstFresh/SEAICE_rhoIce
      convertHI2PRECIP = ONE/convertPRECIP2HI
C     compute parameters for thickness pdf (cheap enough to do it here
C     and not in seaice_readparms and store in common block)
      denominator = 0. _d 0
      DO IT=1,SEAICE_multDim
       denominator = denominator + IT * SEAICE_pdf(IT)
      ENDDO
      denominator = (2.0 _d 0 * denominator) - 1.0 _d 0
      recip_denominator = 1. _d 0 / denominator
#ifdef SEAICE_ITD
      areaPDFfac  = 1. _d 0
#else
      areaPDFfac  = denominator * recip_multDim
#endif /* SEAICE_ITD */
#ifdef SEAICE_ITD
C constants for lateral melt parameterization:
C following Steele (1992), Equ. 2
      floeAlpha                  = 0.66 _d 0
C typical mean diameter used in CICE 4.1:
C (this is currently computed as a function of ice concentration
C  following a suggestion by Luepkes at al. (2012))
C      floeDiameter               = 300. _d 0
C parameters needed for variable floe diameter following Luepkes et al. (2012):
      floeDiameterMin            = 8. _d 0
      floeDiameterMax            = 300. _d 0
#endif

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

#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C time scale of grease ice decline by solidification,
C with 50% grease ice becoming solid pancake ice within 1 day:
        greaseDecayTime=1.44*86400. _d 0
C store position of 'grease' in tracer array
        iTrGrease=-1
        DO iTr = 1, SItrNumInUse
         IF (SItrName(iTr).EQ.'grease') iTrGrease=iTr
        ENDDO
#endif

C array initializations
C =====================

        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_cover (I,J)       = 0.0 _d 0
          a_QbyATM_open(I,J)         = 0.0 _d 0
          r_QbyATM_cover (I,J)       = 0.0 _d 0
          r_QbyATM_open (I,J)        = 0.0 _d 0

          a_QSWbyATM_open (I,J)      = 0.0 _d 0
          a_QSWbyATM_cover (I,J)     = 0.0 _d 0

          a_QbyOCN (I,J)             = 0.0 _d 0
          r_QbyOCN (I,J)             = 0.0 _d 0

#ifdef ALLOW_DIAGNOSTICS
          d_AREAbyATM(I,J)           = 0.0 _d 0
          d_AREAbyICE(I,J)           = 0.0 _d 0
          d_AREAbyOCN(I,J)           = 0.0 _d 0
#endif

          d_HEFFbyOCNonICE(I,J)      = 0.0 _d 0
          d_HEFFbyATMonOCN(I,J)      = 0.0 _d 0
          d_HEFFbyFLOODING(I,J)      = 0.0 _d 0

          d_HEFFbyATMonOCN_open(I,J) = 0.0 _d 0
          d_HEFFbyATMonOCN_cover(I,J) = 0.0 _d 0

          d_HSNWbyATMonSNW(I,J)      = 0.0 _d 0
          d_HSNWbyOCNonSNW(I,J)      = 0.0 _d 0
          d_HSNWbyRAIN(I,J)          = 0.0 _d 0
          a_FWbySublim(I,J)          = 0.0 _d 0
          r_FWbySublim(I,J)          = 0.0 _d 0
          d_HEFFbySublim(I,J)        = 0.0 _d 0
          d_HSNWbySublim(I,J)        = 0.0 _d 0
#ifdef SEAICE_CAP_SUBLIM
          latentHeatFluxMax(I,J)     = 0.0 _d 0
#endif
          d_HFRWbyRAIN(I,J)          = 0.0 _d 0
          tmparr1(I,J)               = 0.0 _d 0
#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
          greaseLayerThick(I,J)      = 0.0 _d 0
          d_HEFFbyGREASE(I,J)        = 0.0 _d 0
#endif
          DO IT=1,SEAICE_multDim
            ticeInMult(I,J,IT)            = 0.0 _d 0
            ticeOutMult(I,J,IT)           = 0.0 _d 0
            a_QbyATMmult_cover(I,J,IT)    = 0.0 _d 0
            a_QSWbyATMmult_cover(I,J,IT)  = 0.0 _d 0
            a_FWbySublimMult(I,J,IT)      = 0.0 _d 0
#ifdef SEAICE_CAP_SUBLIM
            latentHeatFluxMaxMult(I,J,IT) = 0.0 _d 0
#endif
#ifdef SEAICE_ITD
            d_HEFFbySublim_ITD(I,J,IT)         = 0.0 _d 0
            d_HSNWbySublim_ITD(I,J,IT)         = 0.0 _d 0
            d_HEFFbyOCNonICE_ITD(I,J,IT)       = 0.0 _d 0
            d_HSNWbyATMonSNW_ITD(I,J,IT)       = 0.0 _d 0
            d_HEFFbyATMonOCN_ITD(I,J,IT)       = 0.0 _d 0
            d_HEFFbyATMonOCN_cover_ITD(I,J,IT) = 0.0 _d 0
            d_HEFFbyATMonOCN_open_ITD(I,J,IT)  = 0.0 _d 0
            d_HSNWbyRAIN_ITD(I,J,IT)           = 0.0 _d 0
            d_HSNWbyOCNonSNW_ITD(I,J,IT)       = 0.0 _d 0
            d_HEFFbyFLOODING_ITD(I,J,IT)       = 0.0 _d 0
            r_QbyATMmult_cover(I,J,IT)         = 0.0 _d 0
            r_FWbySublimMult(I,J,IT)           = 0.0 _d 0
C for lateral melt parameterization:
            latMeltFrac(I,J,IT)                = 0.0 _d 0
            latMeltRate(I,J,IT)                = 0.0 _d 0
#endif
          ENDDO
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
C ===========PART 1: treat pathological cases (post advdiff)===========
C =====================================================================

C     This part has been mostly moved to S/R seaice_reg_ridge, which is
C     called before S/R seaice_growth

C     store regularized values of heff, hsnow, area at the onset of thermo.
        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J)=HEFF(I,J,bi,bj)
          HSNWpreTH(I,J)=HSNOW(I,J,bi,bj)
          AREApreTH(I,J)=AREA(I,J,bi,bj)
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayB(I,J) = AREA(I,J,bi,bj)
          DIAGarrayC(I,J) = HEFF(I,J,bi,bj)
          DIAGarrayD(I,J) = HSNOW(I,J,bi,bj)
#endif
#ifdef ALLOW_SITRACER
          SItrHEFF(I,J,bi,bj,1)=HEFF(I,J,bi,bj)
          SItrAREA(I,J,bi,bj,2)=AREA(I,J,bi,bj)
#endif
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           HEFFITDpreTH(I,J,IT)=HEFFITD(I,J,IT,bi,bj)
           HSNWITDpreTH(I,J,IT)=HSNOWITD(I,J,IT,bi,bj)
           AREAITDpreTH(I,J,IT)=AREAITD(I,J,IT,bi,bj)

C     keep track of areal and volume fraction of each ITD category
           IF (AREA(I,J,bi,bj) .GT. ZERO) THEN
            areaFracFactor(I,J,IT)=AREAITD(I,J,IT,bi,bj)/AREA(I,J,bi,bj)
           ELSE
C           if there is no ice, potential growth starts in 1st category
            IF (IT .EQ. 1) THEN
             areaFracFactor(I,J,IT)=ONE
            ELSE
             areaFracFactor(I,J,IT)=ZERO
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_SITRACER
C     prepare SItrHEFF to be computed as cumulative sum
        DO iTr=2,5
         DO J=1,sNy
          DO I=1,sNx
           SItrHEFF(I,J,bi,bj,iTr)=ZERO
          ENDDO
         ENDDO
        ENDDO
C     prepare SItrAREA to be computed as cumulative sum
        DO J=1,sNy
         DO I=1,sNx
          SItrAREA(I,J,bi,bj,3)=ZERO
         ENDDO
        ENDDO
#endif
#endif /* SEAICE_ITD */

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(DIAGarrayB,'SIareaPT',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayC,'SIheffPT',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayD,'SIhsnoPT',0,1,3,bi,bj,myThid)
#ifdef ALLOW_SITRACER
         DO iTr = 1, SItrNumInUse
          WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTr,'PT'
          IF (SItrMate(iTr).EQ.'HEFF') THEN
           CALL DIAGNOSTICS_FRACT_FILL(
     I          SItracer(1-OLx,1-OLy,bi,bj,iTr),HEFF(1-OLx,1-OLy,bi,bj),
     I          ONE, 1, diagName,0,1,2,bi,bj,myThid )
          ELSE
           CALL DIAGNOSTICS_FRACT_FILL(
     I          SItracer(1-OLx,1-OLy,bi,bj,iTr),AREA(1-OLx,1-OLy,bi,bj),
     I          ONE, 1, diagName,0,1,2,bi,bj,myThid )
          ENDIF
         ENDDO
#endif /* ALLOW_SITRACER */
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf no additional dependency of air-sea fluxes to ice
        IF ( SEAICEadjMODE.GE.1 ) THEN
         DO J=1,sNy
          DO I=1,sNx
           HEFFpreTH(I,J) = 0. _d 0
           HSNWpreTH(I,J) = 0. _d 0
           AREApreTH(I,J) = 0. _d 0
          ENDDO
         ENDDO
#ifdef SEAICE_ITD
         DO IT=1,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            HEFFITDpreTH(I,J,IT) = 0. _d 0
            HSNWITDpreTH(I,J,IT) = 0. _d 0
            AREAITDpreTH(I,J,IT) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
#endif
        ENDIF
#endif

#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
        DO J=1,sNy
         DO I=1,sNx
          AREAforAtmFW(I,J,bi,bj) = AREApreTH(I,J)
         ENDDO
        ENDDO
#endif

C     COMPUTE ACTUAL ICE/SNOW THICKNESS; USE MIN/MAX VALUES
C     TO REGULARIZE SEAICE_SOLVE4TEMP/d_AREA COMPUTATIONS

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREApreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HEFFpreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HSNWpreTH = comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           IF (HEFFITDpreTH(I,J,IT) .GT. ZERO) THEN
C     regularize AREA with SEAICE_area_reg
            tmpscal1 = SQRT(AREAITDpreTH(I,J,IT) * AREAITDpreTH(I,J,IT)
     &                     + area_reg_sq)
C     heffActual calculated with the regularized AREA
            tmpscal2 = HEFFITDpreTH(I,J,IT) / tmpscal1
C     regularize heffActual with SEAICE_hice_reg (add lower bound)
            heffActualMult(I,J,IT) = SQRT(tmpscal2 * tmpscal2
     &                                    + hice_reg_sq)
C     hsnowActual calculated with the regularized AREA
            hsnowActualMult(I,J,IT) = HSNWITDpreTH(I,J,IT) / tmpscal1
C     regularize the inverse of heffActual by hice_reg
            recip_heffActualMult(I,J,IT)  = AREAITDpreTH(I,J,IT) /
     &           sqrt(HEFFITDpreTH(I,J,IT) * HEFFITDpreTH(I,J,IT)
     &           + hice_reg_sq)
C     Do not regularize when HEFFpreTH = 0
           ELSE
            heffActualMult(I,J,IT) = ZERO
            hsnowActualMult(I,J,IT) = ZERO
            recip_heffActualMult(I,J,IT)  = ZERO
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#else /* ndef SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          IF (HEFFpreTH(I,J) .GT. ZERO) THEN
Cif        regularize AREA with SEAICE_area_reg
           tmpscal1 = SQRT(AREApreTH(I,J)* AREApreTH(I,J) + area_reg_sq)
Cif        heffActual calculated with the regularized AREA
           tmpscal2 = HEFFpreTH(I,J) / tmpscal1
Cif        regularize heffActual with SEAICE_hice_reg (add lower bound)
           heffActual(I,J) = SQRT(tmpscal2 * tmpscal2 + hice_reg_sq)
Cif        hsnowActual calculated with the regularized AREA
           hsnowActual(I,J) = HSNWpreTH(I,J) / tmpscal1
Cif        regularize the inverse of heffActual by hice_reg
           recip_heffActual(I,J)  = AREApreTH(I,J) /
     &                 sqrt(HEFFpreTH(I,J)*HEFFpreTH(I,J) + hice_reg_sq)
Cif       Do not regularize when HEFFpreTH = 0
          ELSE
           heffActual(I,J) = ZERO
           hsnowActual(I,J) = ZERO
           recip_heffActual(I,J)  = ZERO
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
        CALL ZERO_ADJ_1D( sNx*sNy, heffActual, myThid)
        CALL ZERO_ADJ_1D( sNx*sNy, hsnowActual, myThid)
        CALL ZERO_ADJ_1D( sNx*sNy, recip_heffActual, myThid)
#endif

#ifdef SEAICE_CAP_SUBLIM
C     COMPUTE MAXIMUM LATENT HEAT FLUXES FOR THE CURRENT ICE
C     AND SNOW THICKNESS
C     The latent heat flux over the sea ice which
C     will sublimate all of the snow and ice over one time
C     step (W/m^2)
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           IF (HEFFITDpreTH(I,J,IT) .GT. ZERO) THEN
            latentHeatFluxMaxMult(I,J,IT) = lhSublim*recip_deltaTtherm *
     &           (HEFFITDpreTH(I,J,IT)*SEAICE_rhoIce +
     &            HSNWITDpreTH(I,J,IT)*SEAICE_rhoSnow)
     &           /AREAITDpreTH(I,J,IT)
           ELSE
            latentHeatFluxMaxMult(I,J,IT) = ZERO
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#else /* ndef SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          IF (HEFFpreTH(I,J) .GT. ZERO) THEN
           latentHeatFluxMax(I,J) = lhSublim * recip_deltaTtherm *
     &          (HEFFpreTH(I,J) * SEAICE_rhoIce +
     &           HSNWpreTH(I,J) * SEAICE_rhoSnow)/AREApreTH(I,J)
          ELSE
           latentHeatFluxMax(I,J) = ZERO
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */
#endif /* SEAICE_CAP_SUBLIM */

C ===================================================================
C ================PART 2: determine heat fluxes/stocks===============
C ===================================================================

C determine available heat due to the atmosphere -- for open water
C ================================================================

        DO j=1,sNy
         DO i=1,sNx
C ocean surface/mixed layer temperature
          TmixLoc(i,j) = theta(i,j,kSurface,bi,bj)+celsius2K
C wind speed from exf
          UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE qnet(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
cCADJ STORE UG = comlev1_bibj, key = iicekey,byte=isbyte
cCADJ STORE TmixLoc = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        CALL SEAICE_BUDGET_OCEAN(
     I       UG,
     I       TmixLoc,
     O       a_QbyATM_open, a_QSWbyATM_open,
     I       bi, bj, myTime, myIter, myThid )

C determine available heat due to the atmosphere -- for ice covered water
C =======================================================================

        IF (useRelativeWind.AND.useAtmWind) THEN
C     Compute relative wind speed over sea ice.
         DO J=1,sNy
          DO I=1,sNx
           SPEED_SQ =
     &          (uWind(I,J,bi,bj)
     &          -0.5 _d 0*(uice(i,j,bi,bj)+uice(i+1,j,bi,bj)))**2
     &          +(vWind(I,J,bi,bj)
     &          -0.5 _d 0*(vice(i,j,bi,bj)+vice(i,j+1,bi,bj)))**2
           IF ( SPEED_SQ .LE. SEAICE_EPS_SQ ) THEN
             UG(I,J)=SEAICE_EPS
           ELSE
             UG(I,J)=SQRT(SPEED_SQ)
           ENDIF
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnowActual = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE heffActual  = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE UG          = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE tices(:,:,:,bi,bj)
CADJ &     = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Start loop over multi-categories
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           ticeInMult(I,J,IT)  = TICES(I,J,IT,bi,bj)
           ticeOutMult(I,J,IT) = TICES(I,J,IT,bi,bj)
           TICES(I,J,IT,bi,bj) = ZERO
          ENDDO
         ENDDO
#ifndef SEAICE_ITD
C     for SEAICE_ITD heffActualMult and latentHeatFluxMaxMult have been
C     calculated above (instead of heffActual and latentHeatFluxMax)
C--   assume homogeneous distribution between 0 and 2 x heffActual
         pFac = (2.0 _d 0*IT - 1.0 _d 0)*recip_denominator
         pFacSnow = 1. _d 0
         IF ( SEAICE_useMultDimSnow ) pFacSnow=pFac
         DO J=1,sNy
          DO I=1,sNx
           heffActualMult(I,J,IT)        = heffActual(I,J)*pFac
           hsnowActualMult(I,J,IT)       = hsnowActual(I,J)*pFacSnow
#ifdef SEAICE_CAP_SUBLIM
           latentHeatFluxMaxMult(I,J,IT) = latentHeatFluxMax(I,J)*pFac
#endif
          ENDDO
         ENDDO
#endif /* ndef SEAICE_ITD */
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffActualMult = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE hsnowActualMult= comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE ticeInMult     = comlev1_bibj, key = iicekey, byte = isbyte
# ifdef SEAICE_CAP_SUBLIM
CADJ STORE latentHeatFluxMaxMult
CADJ &     = comlev1_bibj, key = iicekey, byte = isbyte
# endif
CADJ STORE a_QbyATMmult_cover   =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATMmult_cover =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_FWbySublimMult     =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO IT=1,SEAICE_multDim
         CALL SEAICE_SOLVE4TEMP(
     I        UG, heffActualMult(1,1,IT), hsnowActualMult(1,1,IT),
#ifdef SEAICE_CAP_SUBLIM
     I        latentHeatFluxMaxMult(1,1,IT),
#endif
     I        ticeInMult(1,1,IT),
     O        ticeOutMult(1,1,IT),
     O        a_QbyATMmult_cover(1,1,IT),
     O        a_QSWbyATMmult_cover(1,1,IT),
     O        a_FWbySublimMult(1,1,IT),
     I        bi, bj, myTime, myIter, myThid )
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffActualMult = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE hsnowActualMult= comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE ticeOutMult    = comlev1_bibj, key = iicekey, byte = isbyte
# ifdef SEAICE_CAP_SUBLIM
CADJ STORE latentHeatFluxMaxMult
CADJ &     = comlev1_bibj, key = iicekey, byte = isbyte
# endif
CADJ STORE a_QbyATMmult_cover   =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATMmult_cover =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_FWbySublimMult     =
CADJ &     comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C     update TICES
CMLC     and tIce, if required later on (currently not the case)
CML#ifdef SEAICE_ITD
CMLC     calculate area weighted mean
CMLC     (although the ice temperature relates to its energy content
CMLC      and hence should be averaged weighted by ice volume,
CMLC      the temperature here is a result of the fluxes through the ice surface
CMLC      computed individually for each single category in SEAICE_SOLVE4TEMP
CMLC      and hence is averaged area weighted [areaFracFactor])
CML           tIce(I,J,bi,bj) = tIce(I,J,bi,bj)
CML     &          +  ticeOutMult(I,J,IT)*areaFracFactor(I,J,IT)
CML#else
CML           tIce(I,J,bi,bj) = tIce(I,J,bi,bj)
CML     &          +  ticeOutMult(I,J,IT)*SEAICE_PDF(IT)
CML#endif
           TICES(I,J,IT,bi,bj) = ticeOutMult(I,J,IT)
C     average over categories
#ifdef SEAICE_ITD
C     calculate area weighted mean
C     (fluxes are per unit (ice surface) area and are thus area weighted)
           a_QbyATM_cover   (I,J) = a_QbyATM_cover(I,J)
     &          + a_QbyATMmult_cover(I,J,IT)*areaFracFactor(I,J,IT)
           a_QSWbyATM_cover (I,J) = a_QSWbyATM_cover(I,J)
     &          + a_QSWbyATMmult_cover(I,J,IT)*areaFracFactor(I,J,IT)
           a_FWbySublim     (I,J) = a_FWbySublim(I,J)
     &          + a_FWbySublimMult(I,J,IT)*areaFracFactor(I,J,IT)
#else
           a_QbyATM_cover   (I,J) = a_QbyATM_cover(I,J)
     &          + a_QbyATMmult_cover(I,J,IT)*SEAICE_PDF(IT)
           a_QSWbyATM_cover (I,J) = a_QSWbyATM_cover(I,J)
     &          + a_QSWbyATMmult_cover(I,J,IT)*SEAICE_PDF(IT)
           a_FWbySublim     (I,J) = a_FWbySublim(I,J)
     &          + a_FWbySublimMult(I,J,IT)*SEAICE_PDF(IT)
#endif
          ENDDO
         ENDDO
        ENDDO

#ifdef SEAICE_CAP_SUBLIM
# ifdef ALLOW_DIAGNOSTICS
        DO J=1,sNy
         DO I=1,sNx
C          The actual latent heat flux realized by SOLVE4TEMP
           DIAGarrayA(I,J) = a_FWbySublim(I,J) * lhSublim
         ENDDO
        ENDDO
Cif     The actual vs. maximum latent heat flux
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &     'SIactLHF',0,1,3,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL(latentHeatFluxMax,
     &     'SImaxLHF',0,1,3,bi,bj,myThid)
        ENDIF
# endif /* ALLOW_DIAGNOSTICS */
#endif /* SEAICE_CAP_SUBLIM */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREApreTH       = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_cover  = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_cover= comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_open   = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_open = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_FWbySublim    = comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C switch heat fluxes from W/m2 to 'effective' ice meters
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           a_QbyATMmult_cover(I,J,IT)   = a_QbyATMmult_cover(I,J,IT)
     &          * convertQ2HI * AREAITDpreTH(I,J,IT)
           a_QSWbyATMmult_cover(I,J,IT) = a_QSWbyATMmult_cover(I,J,IT)
     &          * convertQ2HI * AREAITDpreTH(I,J,IT)
C and initialize r_QbyATMmult_cover
           r_QbyATMmult_cover(I,J,IT)=a_QbyATMmult_cover(I,J,IT)
C     Convert fresh water flux by sublimation to 'effective' ice meters.
C     Negative sublimation is resublimation and will be added as snow.
#ifdef SEAICE_DISABLE_SUBLIM
           a_FWbySublimMult(I,J,IT) = ZERO
#endif
           a_FWbySublimMult(I,J,IT) = SEAICE_deltaTtherm*recip_rhoIce
     &            * a_FWbySublimMult(I,J,IT)*AREAITDpreTH(I,J,IT)
           r_FWbySublimMult(I,J,IT)=a_FWbySublimMult(I,J,IT)
          ENDDO
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_open(I,J)    = a_QbyATM_open(I,J)
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
          a_QSWbyATM_open(I,J)  = a_QSWbyATM_open(I,J)
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
C and initialize r_QbyATM_open
          r_QbyATM_open(I,J)=a_QbyATM_open(I,J)
         ENDDO
        ENDDO
#else /* SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_cover(I,J)   = a_QbyATM_cover(I,J)
     &         * convertQ2HI * AREApreTH(I,J)
          a_QSWbyATM_cover(I,J) = a_QSWbyATM_cover(I,J)
     &         * convertQ2HI * AREApreTH(I,J)
          a_QbyATM_open(I,J)    = a_QbyATM_open(I,J)
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
          a_QSWbyATM_open(I,J)  = a_QSWbyATM_open(I,J)
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
C and initialize r_QbyATM_cover/r_QbyATM_open
          r_QbyATM_cover(I,J)=a_QbyATM_cover(I,J)
          r_QbyATM_open(I,J)=a_QbyATM_open(I,J)
C     Convert fresh water flux by sublimation to 'effective' ice meters.
C     Negative sublimation is resublimation and will be added as snow.
#ifdef SEAICE_DISABLE_SUBLIM
Cgf just for those who may need to omit this term to reproduce old results
          a_FWbySublim(I,J) = ZERO
#endif /* SEAICE_DISABLE_SUBLIM */
          a_FWbySublim(I,J) = SEAICE_deltaTtherm*recip_rhoIce
     &           * a_FWbySublim(I,J)*AREApreTH(I,J)
          r_FWbySublim(I,J)=a_FWbySublim(I,J)
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREApreTH       = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_cover  = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_cover= comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_open   = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_open = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE a_FWbySublim    = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE r_QbyATM_cover  = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE r_QbyATM_open   = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE r_FWbySublim    = comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf no additional dependency through ice cover
        IF ( SEAICEadjMODE.GE.3 ) THEN
#ifdef SEAICE_ITD
         DO IT=1,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            a_QbyATMmult_cover(I,J,IT)   = 0. _d 0
            r_QbyATMmult_cover(I,J,IT)   = 0. _d 0
            a_QSWbyATMmult_cover(I,J,IT) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
#else /* ndef SEAICE_ITD */
         DO J=1,sNy
          DO I=1,sNx
           a_QbyATM_cover(I,J)   = 0. _d 0
           r_QbyATM_cover(I,J)   = 0. _d 0
           a_QSWbyATM_cover(I,J) = 0. _d 0
          ENDDO
         ENDDO
#endif /* SEAICE_ITD */
        ENDIF
#endif

C determine available heat due to the ice pack tying the
C underlying surface water temperature to freezing point
C ======================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif

        DO J=1,sNy
         DO I=1,sNx
C         FREEZING TEMP. OF SEA WATER (deg C)
          tempFrz = SEAICE_tempFrz0 +
     &              SEAICE_dTempFrz_dS *salt(I,J,kSurface,bi,bj)
C efficiency of turbulent fluxes : dependency to sign of THETA-TBC
          IF ( theta(I,J,kSurface,bi,bj) .GE. tempFrz ) THEN
           tmpscal1 = SEAICE_mcPheePiston
          ELSE
           tmpscal1 =SEAICE_frazilFrac*dzSurf/SEAICE_deltaTtherm
          ENDIF
C efficiency of turbulent fluxes : dependency to AREA (McPhee cases)
          IF ( (AREApreTH(I,J) .GT. 0. _d 0).AND.
     &         (.NOT.SEAICE_mcPheeStepFunc) ) THEN
           MixedLayerTurbulenceFactor = ONE -
     &          SEAICE_mcPheeTaper * AREApreTH(I,J)
          ELSEIF ( (AREApreTH(I,J) .GT. 0. _d 0).AND.
     &             (SEAICE_mcPheeStepFunc) ) THEN
           MixedLayerTurbulenceFactor = ONE - SEAICE_mcPheeTaper
          ELSE
           MixedLayerTurbulenceFactor = ONE
          ENDIF
C maximum turbulent flux, in ice meters
          tmpscal2= - (HeatCapacity_Cp*rhoConst * recip_QI)
     &         * (theta(I,J,kSurface,bi,bj)-tempFrz)
     &         * SEAICE_deltaTtherm * HEFFM(i,j,bi,bj)
C available turbulent flux
          a_QbyOCN(i,j) =
     &         tmpscal1 * tmpscal2 * MixedLayerTurbulenceFactor
          r_QbyOCN(i,j) = a_QbyOCN(i,j)
         ENDDO
        ENDDO

#ifdef SEAICE_ITD
C determine lateral melt rate at floe edges based on an
C average floe diameter or a floe size distribution
C following Steele (1992, Tab. 2)
C ======================================================
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           tempFrz = SEAICE_tempFrz0 +
     &          SEAICE_dTempFrz_dS *salt(I,J,kSurface,bi,bj)
           tmpscal1=(theta(I,J,kSurface,bi,bj)-tempFrz)
           tmpscal2=sqrt(0.87 + 0.067*UG(i,j)) * UG(i,j)

C     variable floe diameter following Luepkes et al. (2012, JGR, Equ. 26)
C     with beta=1
CML           tmpscal3=ONE/(ONE-(floeDiameterMin/floeDiameterMax))
CML           floeDiameter = floeDiameterMin
CML     &          * (tmpscal3 / (tmpscal3-AREApreTH(I,J)))
C     this form involves fewer divisions but gives the same result
           floeDiameter = floeDiameterMin * floeDiameterMax
     &          / ( floeDiameterMax*( 1. _d 0 - AREApreTH(I,J) )
     &            + floeDiameterMin*AREApreTH(I,J) )
C     following the idea of SEAICE_areaLossFormula == 1:
           IF (a_QbyATMmult_cover(i,j,it).LT.ZERO .OR.
     &         a_QbyATM_open(i,j) .LT.ZERO .OR.
     &         a_QbyOCN(i,j)      .LT.ZERO) THEN
C     lateral melt rate as suggested by Perovich, 1983 (PhD thesis)
C           latMeltRate(i,j,it) = 1.6 _d -6 * tmpscal1**1.36
C     The following for does the same, but is faster
            latMeltRate(i,j,it) = ZERO
            IF (tmpscal1 .GT. ZERO)
     &         latMeltRate(i,j,it) = 1.6 _d -6 * exp(1.36*log(tmpscal1))
C     lateral melt rate as suggested by Maykut and Perovich, 1987
C     (JGR 92(C7)), Equ. 24
c           latMeltRate(i,j,it) = 13.5 _d -6 * tmpscal2 * tmpscal1**1.3
C     further suggestion by Maykut and Perovich to avoid
C     latMeltRate -> 0 for UG -> 0
c           latMeltRate(i,j,it) = (1.6 _d -6 + 13.5 _d -6 * tmpscal2)
c    &                          * tmpscal1**1.3
C     factor determining fraction of area and ice volume reduction
C     due to lateral melt
            latMeltFrac(i,j,it) =
     &       latMeltRate(i,j,it)*SEAICE_deltaTtherm*PI /
     &       (floeAlpha * floeDiameter)
            latMeltFrac(i,j,it)=max(ZERO, min(latMeltFrac(i,j,it),ONE))
           ELSE
            latMeltRate(i,j,it)=0.0 _d 0
            latMeltFrac(i,j,it)=0.0 _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
        CALL ZERO_ADJ_1D( sNx*sNy, r_QbyOCN, myThid)
#endif

C ===================================================================
C =========PART 3: determine effective thicknesses increments========
C ===================================================================

#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C convert SItracer 'grease' from ratio to grease ice volume:
C ==========================================================
        DO j=1,sNy
         DO i=1,sNx
          SItracer(i,j,bi,bj,iTrGrease) =
     &     SItracer(i,j,bi,bj,iTrGrease) * HEFF(i,j,bi,bj)
          uRelW(i,j) = uWind(i,j,bi,bj)
          vRelW(i,j) = vWind(i,j,bi,bj)
         ENDDO
        ENDDO
        IF ( useRelativeWind ) THEN
         DO j=1,sNy
          DO i=1,sNx
           uRelW(i,j) = uWind(i,j,bi,bj)
     &          - 0.5 _d 0*( uIce(i,j,bi,bj) + uIce(i+1,j,bi,bj) )
           vRelW(i,j) = vWind(i,j,bi,bj)
     &          - 0.5 _d 0*( vIce(i,j,bi,bj) + vIce(i,j+1,bi,bj) )
          ENDDO
         ENDDO
        ENDIF
C compute actual grease ice layer thickness
C as a function of wind stress
C following Smedsrud [2011, Ann.Glac.]
C =========================================
         DO j=1,sNy
          DO i=1,sNx
C
C          computing compaction force acting on grease
C           (= air and water stress acting on ice)
C          wind stress (using calculation of SPEED_SQ & UG as template)
C          u_a**2 + v_a**2:
           tmpscal1 = uRelW(i,j)*uRelW(i,j) + vRelW(i,j)*vRelW(i,j)
           IF ( tmpscal1 .LE. SEAICE_EPS_SQ ) THEN
             tmpscal1=SEAICE_EPS
           ELSE
             tmpscal1=SQRT(tmpscal2)
           ENDIF
           tmpscal1 = 1.4 _d 0 * 1.3 _d -3 * tmpscal1
C          water stress
C          u_w - u_i:
           tmpscal2 =
     &        0.5 _d 0*(uVel(i,j,kSurface,bi,bj)
     &                 +uVel(i+1,j,kSurface,bi,bj))
     &       -0.5 _d 0*(uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj))
C          v_w - v_i:
           tmpscal3 =
     &        0.5 _d 0*(vVel(i,j,kSurface,bi,bj)
     &                 +vVel(i,j+1,kSurface,bi,bj))
     &       -0.5 _d 0*(vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))
C          (u_w - u_i)**2 + (v_w - v_i)**2:
           tmpscal4 = (tmpscal2*tmpscal2 + tmpscal3*tmpscal3)
           IF ( tmpscal4 .LE. SEAICE_EPS_SQ ) THEN
             tmpscal4=SEAICE_EPS
           ELSE
             tmpscal4=SQRT(tmpscal4)
           ENDIF
           tmpscal4 = 1027.0 _d 0 * 6.0 _d -3 * tmpscal4
C          magnitude of compined stresses:
           tmpscal0 =
     &         ( tmpscal1 * uRelW(i,j) + tmpscal4 * tmpscal2 )**2
     &       + ( tmpscal1 * vRelW(i,j) + tmpscal4 * tmpscal3 )**2
           IF ( tmpscal0 .LE. SEAICE_EPS_SQ ) THEN
             tmpscal0=SEAICE_EPS
           ELSE
             tmpscal0=SQRT(tmpscal0)
           ENDIF
C
C          mean grid cell width between tracer points
           tmpscal3 = 0.5 _d 0 * (dxC(i,j,bi,bj)+dyC(i,j,bi,bj))
C          grease ice volume Vg [m^3/m] as in Smedsrud [2011]
C           is SItracer * gridcell_area / lead_width
C           where lead width is lead extent along lead,
C           i.e. perpendicular to L_lead in Smedsrud (2011),
C           which is in the absence of lead length statistics
C           simply the grid cell length
           tmpscal4 = 4. _d 0 * SItracer(i,j,bi,bj,iTrGrease) * tmpscal3
C
C          mean grease ice layer thickness <h_g>, Eq.10 in Smedsrud [2011] but incl. water stress
           greaseLayerThick(i,j) = 0.763 _d 0
C          grease ice volume
     &           * ( tmpscal4
C          times magnitude of vector sum of air and water stresses
C          (in fact, only the component perpendicular to the lead edge, i.e. along L_lead counts)
C          ATTENTION: since we do not have lead orientation with respect to wind
C                     we use 80% of the total force assuming angles 45-90deg between wind and lead edge
     &           * 0.8 _d 0 * tmpscal0
C          devided by K_r = 100 N/m^3 (resistance of grease against compression)
     &           * 0.01 _d 0 )**THIRD
C
C          assure a minimum thickness of 4 cm (equals HO=0.01):
           greaseLayerThick(i,j)=max(4. _d -2, greaseLayerThick(i,j))
C          ... and a maximum thickness of 4 m (equals HO=1.0):
           greaseLayerThick(i,j)=min(4. _d 0 , greaseLayerThick(i,j))
C
          ENDDO
         ENDDO
#endif /* SEAICE_GREASE */

C compute snow/ice tendency due to sublimation
C ============================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_FWbySublim     = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
#endif /* SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
C     First sublimate/deposite snow
          tmpscal2 =
#ifdef SEAICE_ITD
     &     MAX(MIN(r_FWbySublimMult(I,J,IT),HSNOWITD(I,J,IT,bi,bj)
     &             *SNOW2ICE),ZERO)
          d_HSNWbySublim_ITD(I,J,IT) = - tmpscal2 * ICE2SNOW
C         accumulate change over ITD categories
          d_HSNWbySublim(I,J)     = d_HSNWbySublim(I,J)      - tmpscal2
     &                                                       *ICE2SNOW
          r_FWbySublimMult(I,J,IT)= r_FWbySublimMult(I,J,IT) - tmpscal2
#else /* ndef SEAICE_ITD */
     &     MAX(MIN(r_FWbySublim(I,J),HSNOW(I,J,bi,bj)*SNOW2ICE),ZERO)
          d_HSNWbySublim(I,J) = - tmpscal2 * ICE2SNOW
          HSNOW(I,J,bi,bj)    = HSNOW(I,J,bi,bj)  - tmpscal2*ICE2SNOW
          r_FWbySublim(I,J)   = r_FWbySublim(I,J) - tmpscal2
#endif /* SEAICE_ITD */
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_FWbySublim    = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     If anything is left, sublimate ice
          tmpscal2 =
#ifdef SEAICE_ITD
     &     MAX(MIN(r_FWbySublimMult(I,J,IT),HEFFITD(I,J,IT,bi,bj)),ZERO)
          d_HEFFbySublim_ITD(I,J,IT) = - tmpscal2
C         accumulate change over ITD categories
          d_HEFFbySublim(I,J)      = d_HEFFbySublim(I,J)      - tmpscal2
          r_FWbySublimMult(I,J,IT) = r_FWbySublimMult(I,J,IT) - tmpscal2
#else /* ndef SEAICE_ITD */
     &     MAX(MIN(r_FWbySublim(I,J),HEFF(I,J,bi,bj)),ZERO)
          d_HEFFbySublim(I,J) = - tmpscal2
          HEFF(I,J,bi,bj)     = HEFF(I,J,bi,bj)   - tmpscal2
          r_FWbySublim(I,J)   = r_FWbySublim(I,J) - tmpscal2
#endif /* SEAICE_ITD */
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
C     If anything is left, it will be evaporated from the ocean rather than sublimated.
C     Since a_QbyATM_cover was computed for sublimation, not simple evaporation, we need to
C     remove the fusion part for the residual (that happens to be precisely r_FWbySublim).
#ifdef SEAICE_ITD
          a_QbyATMmult_cover(I,J,IT) = a_QbyATMmult_cover(I,J,IT)
     &                               - r_FWbySublimMult(I,J,IT)
          r_QbyATMmult_cover(I,J,IT) = r_QbyATMmult_cover(I,J,IT)
     &                               - r_FWbySublimMult(I,J,IT)
#else /* ndef SEAICE_ITD */
          a_QbyATM_cover(I,J) = a_QbyATM_cover(I,J)-r_FWbySublim(I,J)
          r_QbyATM_cover(I,J) = r_QbyATM_cover(I,J)-r_FWbySublim(I,J)
#endif /* SEAICE_ITD */
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
C       end IT loop
        ENDDO
#endif /* SEAICE_ITD */

C compute ice thickness tendency due to ice-ocean interaction
C ===========================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

       IF (.NOT.SEAICE_growMeltByConv) THEN

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C          ice growth/melt due to ocean heat r_QbyOCN (W/m^2) is
C          equally distributed under the ice and hence weighted by
C          fractional area of each thickness category
           tmpscal1=MAX(r_QbyOCN(i,j)*areaFracFactor(I,J,IT),
     &                               -HEFFITD(I,J,IT,bi,bj))
           d_HEFFbyOCNonICE_ITD(I,J,IT)=tmpscal1
           d_HEFFbyOCNonICE(I,J) = d_HEFFbyOCNonICE(I,J) + tmpscal1
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_SITRACER
        DO J=1,sNy
         DO I=1,sNx
          SItrHEFF(I,J,bi,bj,2) = HEFFpreTH(I,J)
     &                          + d_HEFFbySublim(I,J)
     &                          + d_HEFFbyOCNonICE(I,J)
         ENDDO
        ENDDO
#endif
        DO J=1,sNy
         DO I=1,sNx
          r_QbyOCN(I,J)=r_QbyOCN(I,J)-d_HEFFbyOCNonICE(I,J)
         ENDDO
        ENDDO
#else /* SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          d_HEFFbyOCNonICE(I,J)=MAX(r_QbyOCN(i,j), -HEFF(I,J,bi,bj))
          r_QbyOCN(I,J)=r_QbyOCN(I,J)-d_HEFFbyOCNonICE(I,J)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj) + d_HEFFbyOCNonICE(I,J)
#ifdef ALLOW_SITRACER
          SItrHEFF(I,J,bi,bj,2)=HEFF(I,J,bi,bj)
#endif
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

      ENDIF !SEAICE_growMeltByConv

C compute snow melt tendency due to snow-atmosphere interaction
C ==================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C     Convert to standard units (meters of ice) rather than to meters
C     of snow. This appears to be more robust.
           tmpscal1=MAX(r_QbyATMmult_cover(I,J,IT),
     &                  -HSNOWITD(I,J,IT,bi,bj)*SNOW2ICE)
           tmpscal2=MIN(tmpscal1,0. _d 0)
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through snow
           IF ( SEAICEadjMODE.GE.2 ) tmpscal2 = 0. _d 0
#endif
           d_HSNWbyATMonSNW_ITD(I,J,IT) = tmpscal2*ICE2SNOW
           d_HSNWbyATMonSNW(I,J) = d_HSNWbyATMonSNW(I,J)
     &                           + tmpscal2*ICE2SNOW
           r_QbyATMmult_cover(I,J,IT)=r_QbyATMmult_cover(I,J,IT)
     &                           - tmpscal2
          ENDDO
         ENDDO
        ENDDO
#else /* SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
C     Convert to standard units (meters of ice) rather than to meters
C     of snow. This appears to be more robust.
          tmpscal1=MAX(r_QbyATM_cover(I,J),-HSNOW(I,J,bi,bj)*SNOW2ICE)
          tmpscal2=MIN(tmpscal1,0. _d 0)
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through snow
          IF ( SEAICEadjMODE.GE.2 ) tmpscal2 = 0. _d 0
#endif
          d_HSNWbyATMonSNW(I,J)= tmpscal2*ICE2SNOW
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + tmpscal2*ICE2SNOW
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J) - tmpscal2
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

C compute ice thickness tendency due to the atmosphere
C ====================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_cover  = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

Cgf note: this block is not actually tested by lab_sea
Cgf where all experiments start in January. So even though
Cgf the v1.81=>v1.82 revision would change results in
Cgf warming conditions, the lab_sea results were not changed.

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           tmpscal1 = HEFFITDpreTH(I,J,IT)
     &              + d_HEFFbySublim_ITD(I,J,IT)
     &              + d_HEFFbyOCNonICE_ITD(I,J,IT)
           tmpscal2 = MAX(-tmpscal1,
     &                     r_QbyATMmult_cover(I,J,IT)
C         Limit ice growth by potential melt by ocean
     &              + AREAITDpreTH(I,J,IT) * r_QbyOCN(I,J))
           d_HEFFbyATMonOCN_cover_ITD(I,J,IT) = tmpscal2
           d_HEFFbyATMonOCN_cover(I,J) = d_HEFFbyATMonOCN_cover(I,J)
     &                                 + tmpscal2
           d_HEFFbyATMonOCN_ITD(I,J,IT) = d_HEFFbyATMonOCN_ITD(I,J,IT)
     &                                 + tmpscal2
           d_HEFFbyATMonOCN(I,J)       = d_HEFFbyATMonOCN(I,J)
     &                                 + tmpscal2
           r_QbyATMmult_cover(I,J,IT)  = r_QbyATMmult_cover(I,J,IT)
     &                                 - tmpscal2
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_SITRACER
        DO J=1,sNy
         DO I=1,sNx
          SItrHEFF(I,J,bi,bj,3) = SItrHEFF(I,J,bi,bj,2)
     &                          + d_HEFFbyATMonOCN_cover(I,J)
         ENDDO
        ENDDO
#endif
#else /* ndef SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx

          tmpscal2 = MAX(-HEFF(I,J,bi,bj),r_QbyATM_cover(I,J)+
C         Limit ice growth by potential melt by ocean
     &        AREApreTH(I,J) * r_QbyOCN(I,J))

          d_HEFFbyATMonOCN_cover(I,J)=tmpscal2
          d_HEFFbyATMonOCN(I,J)=d_HEFFbyATMonOCN(I,J)+tmpscal2
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)-tmpscal2
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + tmpscal2

#ifdef ALLOW_SITRACER
          SItrHEFF(I,J,bi,bj,3)=HEFF(I,J,bi,bj)
#endif
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

C add snow precipitation to HSNOW.
C =================================================
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE a_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE PRECIP(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREApreTH = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        IF ( snowPrecipFile .NE. ' ' ) THEN
C add snowPrecip to HSNOW
         DO J=1,sNy
          DO I=1,sNx
           d_HSNWbyRAIN(I,J) = convertPRECIP2HI * ICE2SNOW *
     &          snowPrecip(i,j,bi,bj) * AREApreTH(I,J)
           d_HFRWbyRAIN(I,J) = -convertPRECIP2HI *
     &          ( PRECIP(I,J,bi,bj) - snowPrecip(I,J,bi,bj) ) *
     &          AREApreTH(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbyRAIN(I,J)
          ENDDO
         ENDDO
        ELSE
C attribute precip to fresh water or snow stock,
C depending on atmospheric conditions.
         DO J=1,sNy
          DO I=1,sNx
C possible alternatives to the a_QbyATM_cover criterium
c          IF (tIce(I,J,bi,bj) .LT. TMIX) THEN ! would require computing tIce
c          IF (atemp(I,J,bi,bj) .LT. celsius2K) THEN
           IF ( a_QbyATM_cover(I,J).GE. 0. _d 0 ) THEN
C           add precip as snow
            d_HFRWbyRAIN(I,J)=0. _d 0
            d_HSNWbyRAIN(I,J)=convertPRECIP2HI*ICE2SNOW*
     &            PRECIP(I,J,bi,bj)*AREApreTH(I,J)
           ELSE
C           add precip to the fresh water bucket
            d_HFRWbyRAIN(I,J)=-convertPRECIP2HI*
     &            PRECIP(I,J,bi,bj)*AREApreTH(I,J)
            d_HSNWbyRAIN(I,J)=0. _d 0
           ENDIF
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           d_HSNWbyRAIN_ITD(I,J,IT)
     &     = d_HSNWbyRAIN(I,J)*areaFracFactor(I,J,IT)
          ENDDO
         ENDDO
        ENDDO
#else /* ndef SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbyRAIN(I,J)
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */
Cgf note: this does not affect air-sea heat flux,
Cgf since the implied air heat gain to turn
Cgf rain to snow is not a surface process.
C end of IF statement snowPrecipFile:
        ENDIF

C compute snow melt due to heat available from ocean.
C =================================================================

Cgf do we need to keep this comment and cpp bracket?
Cph( very sensitive bit here by JZ
#ifndef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

      IF (.NOT.SEAICE_growMeltByConv) THEN

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           tmpscal4 = HSNWITDpreTH(I,J,IT)
     &              + d_HSNWbySublim_ITD(I,J,IT)
     &              + d_HSNWbyATMonSNW_ITD(I,J,IT)
     &              + d_HSNWbyRAIN_ITD(I,J,IT)
           tmpscal1=MAX(r_QbyOCN(i,j)*ICE2SNOW*areaFracFactor(I,J,IT),
     &                  -tmpscal4)
           tmpscal2=MIN(tmpscal1,0. _d 0)
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through snow
           if ( SEAICEadjMODE.GE.2 ) tmpscal2 = 0. _d 0
#endif
           d_HSNWbyOCNonSNW_ITD(I,J,IT) = tmpscal2
           d_HSNWbyOCNonSNW(I,J) = d_HSNWbyOCNonSNW(I,J) + tmpscal2
           r_QbyOCN(I,J)=r_QbyOCN(I,J) - tmpscal2*SNOW2ICE
          ENDDO
         ENDDO
        ENDDO
#else /* ndef SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1=MAX(r_QbyOCN(i,j)*ICE2SNOW, -HSNOW(I,J,bi,bj))
          tmpscal2=MIN(tmpscal1,0. _d 0)
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through snow
          if ( SEAICEadjMODE.GE.2 ) tmpscal2 = 0. _d 0
#endif
          d_HSNWbyOCNonSNW(I,J) = tmpscal2
          r_QbyOCN(I,J)=r_QbyOCN(I,J)
     &                               -d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+d_HSNWbyOCNonSNW(I,J)
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

      ENDIF !SEAICE_growMeltByConv

#endif /* SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING */
Cph)

C gain of new ice over open water
C ===============================
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_QSWbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_QSWbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
#ifdef SEAICE_ITD
C         HEFF will be updated at the end of PART 3,
C         hence sum of tendencies so far is needed
          tmpscal4 = HEFFpreTH(I,J)
     &             + d_HEFFbySublim(I,J)
     &             + d_HEFFbyOCNonICE(I,J)
     &             + d_HEFFbyATMonOCN(I,J)
#else /* ndef SEAICE_ITD */
C         HEFF is updated step by step throughout seaice_growth
          tmpscal4 = HEFF(I,J,bi,bj)
#endif /* SEAICE_ITD */
C           Initial ice growth is triggered by open water
C           heat flux overcoming potential melt by ocean
            tmpscal1=r_QbyATM_open(I,J)+r_QbyOCN(i,j) *
     &            (1.0 _d 0 - AREApreTH(I,J))
C           Penetrative shortwave flux beyond first layer
C           that is therefore not available to ice growth/melt
            tmpscal2=SWFracB * a_QSWbyATM_open(I,J)
C           impose -HEFF as the maxmum melting if SEAICE_doOpenWaterMelt
C           or 0. otherwise (no melting if not SEAICE_doOpenWaterMelt)
            tmpscal3=facOpenGrow*MAX(tmpscal1-tmpscal2,
     &           -tmpscal4*facOpenMelt)*HEFFM(I,J,bi,bj)
#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C Grease ice is a tracer or "bucket" for newly formed frazil ice
C  that instead of becoming solid sea ice instantly has a half-time
C  of 1 day (see greaseDecayTime) before solidifying.
C The most important effect is that for fluxes the grease ice area/volume
C  acts like open water.
C
C store freezing/melting condition:
          greaseNewFrazil = max(0.0 _d 0, tmpscal3)
C
C 1) mechanical removal of grease by ridging:
C    if there is no open water left after advection, there cannot be any grease ice
          if ((1.0 _d 0 - AREApreTH(I,J)).LE.siEps) then
           tmpscal3 = tmpscal3 + SItracer(I,J,bi,bj,iTrGrease)
           SItracer(I,J,bi,bj,iTrGrease) = 0. _d 0

          elseif (greaseNewFrazil .GT. 0. _d 0) then
C    new ice growth goes into grease tracer not HEFF:
           tmpscal3=0. _d 0
C
C 2) solidification of "old" grease ice
C    (only when cold enough for ice growth)
C
C    time scale dependent solidification
C    (50%  of grease ice area become solid ice within 24h):
           tmpscal1=exp(-SEAICE_deltaTtherm/greaseDecayTime)
C    gain in solid sea ice volume due to solidified grease:
           d_HEFFbyGREASE(I,J) =
     &             SItracer(I,J,bi,bj,iTrGrease)
     &           * (1.0 _d 0 - tmpscal1)
C    ... and related loss of grease ice (tracer) volume
           SItracer(I,J,bi,bj,iTrGrease) =
     &             SItracer(I,J,bi,bj,iTrGrease) * tmpscal1
C    the solidified grease ice volume needs to be added to HEFF:
           SItrBucket(I,J,bi,bj,iTrGrease) =
     &             SItrBucket(I,J,bi,bj,iTrGrease)
     &           + d_HEFFbyGREASE(I,J)
C
C 3) grease ice growth from new frazil ice:
C
           SItracer(i,j,bi,bj,iTrGrease) =
     &      SItracer(i,j,bi,bj,iTrGrease) + greaseNewFrazil
          endif
C 4) mapping SItrBucket to external variable, in this case HEFF, ...
          tmpscal3=tmpscal3+SItrBucket(I,J,bi,bj,iTrGrease)
C    ... and empty SItrBucket for tracer 'grease'
          SItrBucket(I,J,bi,bj,iTrGrease)=0. _d 0
#endif /* SEAICE_GREASE */
#ifdef SEAICE_ITD
C         ice growth in open water adds to first category
          d_HEFFbyATMonOCN_open_ITD(I,J,1)=tmpscal3
          d_HEFFbyATMonOCN_ITD(I,J,1)     =d_HEFFbyATMonOCN_ITD(I,J,1)
     &                                    +tmpscal3
#endif /* SEAICE_ITD */
          d_HEFFbyATMonOCN_open(I,J)=tmpscal3
          d_HEFFbyATMonOCN(I,J)=d_HEFFbyATMonOCN(I,J)+tmpscal3
          r_QbyATM_open(I,J)=r_QbyATM_open(I,J)-tmpscal3
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + tmpscal3
         ENDDO
        ENDDO

#ifdef ALLOW_SITRACER
        DO J=1,sNy
         DO I=1,sNx
C needs to be here to allow use also with LEGACY branch
#ifdef SEAICE_ITD
          SItrHEFF(I,J,bi,bj,4)=SItrHEFF(I,J,bi,bj,3)
     &                         +d_HEFFbyATMonOCN_open(I,J)
#else /* ndef SEAICE_ITD */
          SItrHEFF(I,J,bi,bj,4)=HEFF(I,J,bi,bj)
#endif /* SEAICE_ITD */
         ENDDO
        ENDDO
#endif /* ALLOW_SITRACER */

C convert snow to ice if submerged.
C =================================

C note: in legacy, this process is done at the end
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        IF ( SEAICEuseFlooding ) THEN
#ifdef SEAICE_ITD
         DO IT=1,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            tmpscal3 = HEFFITDpreTH(I,J,IT)
     &               + d_HEFFbySublim_ITD(I,J,IT)
     &               + d_HEFFbyOCNonICE_ITD(I,J,IT)
     &               + d_HEFFbyATMonOCN_ITD(I,J,IT)
            tmpscal4 = HSNWITDpreTH(I,J,IT)
     &               + d_HSNWbySublim_ITD(I,J,IT)
     &               + d_HSNWbyATMonSNW_ITD(I,J,IT)
     &               + d_HSNWbyRAIN_ITD(I,J,IT)
            tmpscal0 = (tmpscal4*SEAICE_rhoSnow
     &               +  tmpscal3*SEAICE_rhoIce)
     &               * recip_rhoConst
            tmpscal1 = MAX( 0. _d 0, tmpscal0 - tmpscal3)
            d_HEFFbyFLOODING_ITD(I,J,IT) = tmpscal1
            d_HEFFbyFLOODING(I,J) = d_HEFFbyFLOODING(I,J)  + tmpscal1
           ENDDO
          ENDDO
         ENDDO
#else /* ndef SEAICE_ITD */
         DO J=1,sNy
          DO I=1,sNx
           tmpscal0 = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)*recip_rhoConst
           tmpscal1 = MAX( 0. _d 0, tmpscal0 - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW
          ENDDO
         ENDDO
#endif /* SEAICE_ITD */
        ENDIF

#ifdef SEAICE_ITD
C apply ice and snow thickness changes
C =================================================================
         DO IT=1,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            HEFFITD(I,J,IT,bi,bj) = HEFFITD(I,J,IT,bi,bj)
     &                            + d_HEFFbySublim_ITD(I,J,IT)
     &                            + d_HEFFbyOCNonICE_ITD(I,J,IT)
     &                            + d_HEFFbyATMonOCN_ITD(I,J,IT)
     &                            + d_HEFFbyFLOODING_ITD(I,J,IT)
            HSNOWITD(I,J,IT,bi,bj) = HSNOWITD(I,J,IT,bi,bj)
     &                            + d_HSNWbySublim_ITD(I,J,IT)
     &                            + d_HSNWbyATMonSNW_ITD(I,J,IT)
     &                            + d_HSNWbyRAIN_ITD(I,J,IT)
     &                            + d_HSNWbyOCNonSNW_ITD(I,J,IT)
     &                            - d_HEFFbyFLOODING_ITD(I,J,IT)
     &                            * ICE2SNOW
           ENDDO
          ENDDO
         ENDDO
#endif /* SEAICE_ITD */

C ===================================================================
C ==========PART 4: determine ice cover fraction increments=========-
C ===================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE recip_heffActual = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_hsnwbyatmonsnw = comlev1_bibj,key=iicekey,byte=isbyte
cph(
cphCADJ STORE d_AREAbyATM  = comlev1_bibj,key=iicekey,byte=isbyte
cphCADJ STORE d_AREAbyICE  = comlev1_bibj,key=iicekey,byte=isbyte
cphCADJ STORE d_AREAbyOCN  = comlev1_bibj,key=iicekey,byte=isbyte
cph)
CADJ STORE a_QbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE heffActual = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREApreTH = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HEFF(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREA(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_ITD
C--   in thinnest category account for lateral ice growth and melt the
C--   "non-ITD" way, so that the ITD simulation with SEAICE_multDim=1
C--   is identical with the non-ITD simulation;
C--   use HEFF, ARE, HSNOW, etc. as temporal storage for 1st category
        DO J=1,sNy
         DO I=1,sNx
          HEFF(I,J,bi,bj)=HEFFITD(I,J,1,bi,bj)
          AREA(I,J,bi,bj)=AREAITD(I,J,1,bi,bj)
          HSNOW(I,J,bi,bj)=HSNOWITD(I,J,1,bi,bj)
          HEFFpreTH(I,J)=HEFFITDpreTH(I,J,1)
          AREApreTH(I,J)=AREAITDpreTH(I,J,1)
          recip_heffActual(I,J)=recip_heffActualMult(I,J,1)
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */
        DO J=1,sNy
         DO I=1,sNx

#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C         grease ice layer thickness (Hg) includes
C         1 part frazil ice and 3 parts sea water
C         i.e. HO = 0.25 * Hg
          recip_HO=4. _d 0 / greaseLayerThick(I,J)
#else /* SEAICE_GREASE */
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           recip_HO=1. _d 0 / HO_south
          ELSE
           recip_HO=1. _d 0 / HO
          ENDIF
#endif /* SEAICE_GREASE */
          recip_HH = recip_heffActual(I,J)

C gain of ice over open water : computed from
#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C   from growth by ATM with grease ice time delay
          tmpscal4 = MAX(ZERO,d_HEFFbyGREASE(I,J))
#else /* SEAICE_GREASE */
C   (SEAICE_areaGainFormula.EQ.1) from growth by ATM
C   (SEAICE_areaGainFormula.EQ.2) from predicted growth by ATM
          IF (SEAICE_areaGainFormula.EQ.1) THEN
            tmpscal4 = MAX(ZERO,d_HEFFbyATMonOCN_open(I,J))
          ELSE
            tmpscal4=MAX(ZERO,a_QbyATM_open(I,J))
          ENDIF
#endif /* SEAICE_GREASE */

C loss of ice cover by melting : computed from
C   (SEAICE_areaLossFormula.EQ.1) from all but only melt conributions by ATM and OCN
C   (SEAICE_areaLossFormula.EQ.2) from net melt-growth>0 by ATM and OCN
C   (SEAICE_areaLossFormula.EQ.3) from predicted melt by ATM
          IF (SEAICE_areaLossFormula.EQ.1) THEN
            tmpscal3 = MIN( 0. _d 0 , d_HEFFbyATMonOCN_cover(I,J) )
     &        + MIN( 0. _d 0 , d_HEFFbyATMonOCN_open(I,J) )
     &        + MIN( 0. _d 0 , d_HEFFbyOCNonICE(I,J) )
          ELSEIF (SEAICE_areaLossFormula.EQ.2) THEN
            tmpscal3 = MIN( 0. _d 0 , d_HEFFbyATMonOCN_cover(I,J)
     &        + d_HEFFbyATMonOCN_open(I,J) + d_HEFFbyOCNonICE(I,J) )
          ELSE
C           compute heff after ice melt by ocn:
            tmpscal0=HEFF(I,J,bi,bj) - d_HEFFbyATMonOCN(I,J)
C           compute available heat left after snow melt by atm:
            tmpscal1= a_QbyATM_open(I,J)+a_QbyATM_cover(I,J)
     &            - d_HSNWbyATMonSNW(I,J)*SNOW2ICE
C           could not melt more than all the ice
            tmpscal2 = MAX(-tmpscal0,tmpscal1)
            tmpscal3 = MIN(ZERO,tmpscal2)
          ENDIF

C apply tendency
          IF ( (HEFF(i,j,bi,bj).GT.0. _d 0).OR.
     &        (HSNOW(i,j,bi,bj).GT.0. _d 0) ) THEN
           AREA(I,J,bi,bj)=MAX(0. _d 0,
     &      MIN( SEAICE_area_max, AREA(I,J,bi,bj)
     &       + recip_HO*tmpscal4+HALF*recip_HH*tmpscal3
     &       * areaPDFfac ))
          ELSE
           AREA(I,J,bi,bj)=0. _d 0
          ENDIF
#ifdef ALLOW_SITRACER
          SItrAREA(I,J,bi,bj,3)=AREA(I,J,bi,bj)
#endif /* ALLOW_SITRACER */
#ifdef ALLOW_DIAGNOSTICS
          d_AREAbyATM(I,J)=
     &       recip_HO*MAX(ZERO,d_HEFFbyATMonOCN_open(I,J))
     &       +HALF*recip_HH*MIN(0. _d 0,d_HEFFbyATMonOCN_open(I,J))
     &       *areaPDFfac
          d_AREAbyICE(I,J)=
     &        HALF*recip_HH*MIN(0. _d 0,d_HEFFbyATMonOCN_cover(I,J))
     &       *areaPDFfac
          d_AREAbyOCN(I,J)=
     &        HALF*recip_HH*MIN( 0. _d 0,d_HEFFbyOCNonICE(I,J) )
     &       *areaPDFfac
#endif /* ALLOW_DIAGNOSTICS */
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
C       transfer 1st category values back into ITD variables
        DO J=1,sNy
         DO I=1,sNx
          HEFFITD(I,J,1,bi,bj)=HEFF(I,J,bi,bj)
          AREAITD(I,J,1,bi,bj)=AREA(I,J,bi,bj)
          HSNOWITD(I,J,1,bi,bj)=HSNOW(I,J,bi,bj)
         ENDDO
        ENDDO
C       now melt ice laterally in all other thickness categories
C       (areal growth, i.e. new ice formation, only occurrs in 1st category)
        IF (SEAICE_multDim .gt. 1) THEN
         DO IT=2,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            IF (HEFFITD(I,J,IT,bi,bj).LE.ZERO) THEN
C       when thickness is zero, area should be zero, too:
             AREAITD(I,J,IT,bi,bj)=ZERO
            ELSE
C     tmpscal1 is the minimal ice concentration after lateral melt that will
C     not lead to an unphysical increase of ice thickness by lateral melt;
C     estimated as the concentration before thermodynamics scaled by the
C     ratio of new ice thickness and ice thickness before thermodynamics
             IF ( HEFFITDpreTH(I,J,IT).LE.ZERO ) THEN
              tmpscal1=0. _d 0
             ELSE
              tmpscal1=AREAITDpreTH(I,J,IT)*
     &             HEFFITD(I,J,IT,bi,bj)/HEFFITDpreTH(I,J,IT)
             ENDIF
C       melt ice laterally based on an average floe sice
C       following Steele (1992)
             AREAITD(I,J,IT,bi,bj) = AREAITD(I,J,IT,bi,bj)
     &                             * (ONE - latMeltFrac(I,J,IT))
CML   not necessary:
CML          AREAITD(I,J,IT,bi,bj) = max(ZERO,AREAITD(I,J,IT,bi,bj))
C       limit area reduction so that actual ice thickness does not increase
             AREAITD(I,J,IT,bi,bj) = max(AREAITD(I,J,IT,bi,bj),
     &                                   tmpscal1)
            ENDIF
#ifdef ALLOW_SITRACER
            SItrAREA(I,J,bi,bj,3)=SItrAREA(I,J,bi,bj,3)
     &                           +AREAITD(I,J,IT,bi,bj)
#endif /* ALLOW_SITRACER */
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ITD */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf 'bulk' linearization of area=f(HEFF)
        IF ( SEAICEadjMODE.GE.1 ) THEN
#ifdef SEAICE_ITD
         DO IT=1,SEAICE_multDim
          DO J=1,sNy
           DO I=1,sNx
            AREAITD(I,J,IT,bi,bj) = AREAITDpreTH(I,J,IT) + 0.1 _d 0 *
     &               ( HEFFITD(I,J,IT,bi,bj) - HEFFITDpreTH(I,J,IT) )
           ENDDO
          ENDDO
         ENDDO
#else /* ndef SEAICE_ITD */
         DO J=1,sNy
          DO I=1,sNx
C            AREA(I,J,bi,bj) = 0.1 _d 0 * HEFF(I,J,bi,bj)
           AREA(I,J,bi,bj) = AREApreTH(I,J) + 0.1 _d 0 *
     &               ( HEFF(I,J,bi,bj) - HEFFpreTH(I,J) )
          ENDDO
         ENDDO
#endif /* SEAICE_ITD */
        ENDIF
#endif
#ifdef SEAICE_ITD
C check categories for consistency with limits after growth/melt ...
        IF ( SEAICEuseLinRemapITD ) CALL SEAICE_ITD_REMAP(
     I     heffitdPreTH, areaitdPreTH,
     I     bi, bj, myTime, myIter, myThid )
        CALL SEAICE_ITD_REDIST(bi, bj, myTime, myIter, myThid)
C ... and update total AREA, HEFF, HSNOW
C     (the updated HEFF is used below for ice salinity increments)
        CALL SEAICE_ITD_SUM(bi, bj, myTime, myIter, myThid)
#endif /* SEAICE_ITD */
#if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C convert SItracer 'grease' from grease ice volume back to ratio:
C ===============================================================
        DO J=1,sNy
         DO I=1,sNx
          if (HEFF(I,J,bi,bj).GT.siEps) then
           SItracer(I,J,bi,bj,iTrGrease) =
     &      SItracer(I,J,bi,bj,iTrGrease) / HEFF(I,J,bi,bj)
          else
           SItracer(I,J,bi,bj,iTrGrease) = 0. _d 0
          endif
         ENDDO
        ENDDO
#endif /* SEAICE_GREASE */

C ===================================================================
C =============PART 5: determine ice salinity increments=============
C ===================================================================

#ifndef SEAICE_VARIABLE_SALINITY
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_HEFFbyNEG(:,:,bi,bj) = comlev1_bibj,
CADJ &                              key = iicekey, byte = isbyte
#  ifdef ALLOW_SALT_PLUME
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyFLOODING = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbySublim = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#  endif /* ALLOW_SALT_PLUME */
# endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1 = d_HEFFbyNEG(I,J,bi,bj) + d_HEFFbyOCNonICE(I,J) +
     &               d_HEFFbyATMonOCN(I,J) + d_HEFFbyFLOODING(I,J)
     &             + d_HEFFbySublim(I,J)
#ifdef EXF_SEAICE_FRACTION
     &             + d_HEFFbyRLX(I,J,bi,bj)
#endif
Catn: can not take out more that surface salinity when SSS<SEAICE_salt0
          tmpscal3 = max( 0. _d 0,
     &                    min(SEAICE_salt0,salt(I,J,kSurface,bi,bj)) )
          tmpscal2 = tmpscal1 * tmpscal3 * HEFFM(I,J,bi,bj)
     &            * recip_deltaTtherm * SEAICE_rhoIce
          saltFlux(I,J,bi,bj) = tmpscal2
#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_SPLIT_BASIN
catn attempt to split East/West basins in Arctic
          localSPfrac(I,J) = SPsalFRAC(1)
          IF ( SaltPlumeSplitBasin ) THEN
            localSPfrac(I,J) = SPsalFRAC(2)
            IF(YC(I,J,bi,bj).LT. 85.0 .AND. YC(I,J,bi,bj).GT. 71.0
     &       .AND. XC(I,J,bi,bj) .LT. -90.0) THEN
             localSPfrac(I,J) = SPsalFRAC(1)
            ENDIF
          ENDIF
#else
          localSPfrac(I,J) = SPsalFRAC
#endif /* SALT_PLUME_SPLIT_BASIN */
#ifdef SALT_PLUME_IN_LEADS
Catn: Only d_HEFFbyATMonOCN should contribute to plume.
C     By redefining tmpscal1 here, saltPlumeFlux is smaller in case
C     define inLeads than case undef inLeads.  Physical interpretation
C     is that when d_HEFF is formed from below via ocean freezing, it
C     occurs more uniform over grid cell and not inLeads, thus not
C     participating in pkg/salt_plume.
C     Note: tmpscal1 is defined only after saltFlux is calculated.
          IceGrowthRateInLeads(I,J)=max(0. _d 0,d_HEFFbyATMonOCN(I,J))
          tmpscal1 = IceGrowthRateInLeads(I,J)
          leadPlumeFraction(I,J) =
     &      (ONE + EXP( (SPinflectionPoint - AREApreTH(I,J))*5.0
     &                 /(ONE - SPinflectionPoint) ))**(-ONE)
          localSPfrac(I,J)=localSPfrac(I,J)*leadPlumeFraction(I,J)
#endif /* SALT_PLUME_IN_LEADS */
          tmpscal3 = tmpscal1*salt(I,J,kSurface,bi,bj)*HEFFM(I,J,bi,bj)
     &            * recip_deltaTtherm * SEAICE_rhoIce
          saltPlumeFlux(I,J,bi,bj) = MAX( tmpscal3-tmpscal2 , 0. _d 0)
     &            *localSPfrac(I,J)
C if SaltPlumeSouthernOcean=.FALSE. turn off salt plume in Southern Ocean
          IF ( .NOT. SaltPlumeSouthernOcean ) THEN
           IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 )
     &          saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
          ENDIF
#endif /* ALLOW_SALT_PLUME */
         ENDDO
        ENDDO
#endif /* ndef SEAICE_VARIABLE_SALINITY */

#ifdef SEAICE_VARIABLE_SALINITY

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
C sum up the terms that affect the salt content of the ice pack
         tmpscal1=d_HEFFbyOCNonICE(I,J)+d_HEFFbyATMonOCN(I,J)

C recompute HEFF before thermodynamic updates (which is not AREApreTH in legacy code)
         tmpscal2=HEFF(I,J,bi,bj)-tmpscal1-d_HEFFbyFLOODING(I,J)
C tmpscal1 > 0 : m of sea ice that is created
          IF ( tmpscal1 .GE. 0.0 ) THEN
             saltFlux(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &            *SEAICE_saltFrac*salt(I,J,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_SPLIT_BASIN
catn attempt to split East/West basins in Arctic
             localSPfrac(I,J) = SPsalFRAC(1)
             IF ( SaltPlumeSplitBasin ) THEN
               localSPfrac(I,J) = SPsalFRAC(2)
               IF(YC(I,J,bi,bj).LT. 85.0 .AND. YC(I,J,bi,bj).GT. 71.0
     &                  .AND. XC(I,J,bi,bj) .LT. -90.0) THEN
                 localSPfrac(I,J) = SPsalFRAC(1)
               ENDIF
             ENDIF
#else
             localSPfrac(I,J) = SPsalFRAC
#endif /* SALT_PLUME_SPLIT_BASIN */
#ifndef SALT_PLUME_IN_LEADS
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &            *(ONE-SEAICE_saltFrac)*salt(I,J,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
     &            *localSPfrac(I,J)
#endif /* ndef SALT_PLUME_IN_LEADS */
#endif /* ALLOW_SALT_PLUME */

C tmpscal1 < 0 : m of sea ice that is melted
          ELSE
             saltFlux(I,J,bi,bj) =
     &         HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &         *HSALT(I,J,bi,bj)
     &         *tmpscal1/tmpscal2
#ifdef ALLOW_SALT_PLUME
#ifndef SALT_PLUME_IN_LEADS
             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
#endif /* ndef SALT_PLUME_IN_LEADS */
#endif /* ALLOW_SALT_PLUME */
          ENDIF

#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_IN_LEADS
Catn: only d_HEFFbyATMonOCN should contribute to plume
C     By redefining tmpscal1 here, saltPlumeFlux is smaller in case
C     define inLeads than case undef inLeads.  Physical interpretation
C     is that when d_HEFF is formed from below via ocean freezing, it
C     occurs more uniform over grid cell and not inLeads, thus not
C     participating in pkg/salt_plume.
C     Note: tmpscal1 is defined only after saltFlux is calculated.
          IceGrowthRateInLeads(I,J)=max(0. _d 0,d_HEFFbyATMonOCN(I,J))
          tmpscal1 = IceGrowthRateInLeads(I,J)
          leadPlumeFraction(I,J) =
     &      (ONE + EXP( (SPinflectionPoint - AREApreTH(I,J))*5.0
     &                 /(ONE - SPinflectionPoint) ))**(-ONE)
          localSPfrac(I,J)=localSPfrac(I,J)*leadPlumeFraction(I,J)
          IF ( tmpscal1 .GE. 0.0) THEN
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &            *(ONE-SEAICE_saltFrac)*salt(I,J,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
     &            *localSPfrac(I,J)
          ELSE
             saltPlumeFlux(I,J,bi,bj) = 0. _d 0
          ENDIF
#endif /* SALT_PLUME_IN_LEADS */
C if SaltPlumeSouthernOcean=.FALSE. turn off salt plume in Southern Ocean
          IF ( .NOT. SaltPlumeSouthernOcean ) THEN
           IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 )
     &          saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
          ENDIF
#endif /* ALLOW_SALT_PLUME */
C update HSALT based on surface saltFlux
          HSALT(I,J,bi,bj) = HSALT(I,J,bi,bj) +
     &         saltFlux(I,J,bi,bj) * SEAICE_deltaTtherm
          saltFlux(I,J,bi,bj) =
     &         saltFlux(I,J,bi,bj) + saltFluxAdjust(I,J,bi,bj)
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */

#ifdef ALLOW_SITRACER
        DO J=1,sNy
         DO I=1,sNx
C needs to be here to allow use also with LEGACY branch
          SItrHEFF(I,J,bi,bj,5)=HEFF(I,J,bi,bj)
         ENDDO
        ENDDO
#endif /* ALLOW_SITRACER */

C ===================================================================
C ==============PART 7: determine ocean model forcing================
C ===================================================================

C compute net heat flux leaving/entering the ocean,
C accounting for the part used in melt/freeze processes
C =====================================================

#ifdef SEAICE_ITD
C compute total of "mult" fluxes for ocean forcing
        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_cover(I,J)   = 0.0 _d 0
          r_QbyATM_cover(I,J)   = 0.0 _d 0
          a_QSWbyATM_cover(I,J) = 0.0 _d 0
          r_FWbySublim(I,J)     = 0.0 _d 0
         ENDDO
        ENDDO
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C if fluxes in W/m^2 then use:
c           a_QbyATM_cover(I,J)=a_QbyATM_cover(I,J)
c     &      + a_QbyATMmult_cover(I,J,IT) * areaFracFactor(I,J,IT)
c           r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)
c     &      + r_QbyATMmult_cover(I,J,IT) * areaFracFactor(I,J,IT)
c           a_QSWbyATM_cover(I,J)=a_QSWbyATM_cover(I,J)
c     &      + a_QSWbyATMmult_cover(I,J,IT) * areaFracFactor(I,J,IT)
c           r_FWbySublim(I,J)=r_FWbySublim(I,J)
c     &      + r_FWbySublimMult(I,J,IT) * areaFracFactor(I,J,IT)
C if fluxes in effective ice meters, i.e. ice volume per area, then use:
           a_QbyATM_cover(I,J)=a_QbyATM_cover(I,J)
     &      + a_QbyATMmult_cover(I,J,IT)
           r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)
     &      + r_QbyATMmult_cover(I,J,IT)
           a_QSWbyATM_cover(I,J)=a_QSWbyATM_cover(I,J)
     &      + a_QSWbyATMmult_cover(I,J,IT)
           r_FWbySublim(I,J)=r_FWbySublim(I,J)
     &      + r_FWbySublimMult(I,J,IT)
          ENDDO
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_hsnwbyneg(:,:,bi,bj) = comlev1_bibj,
CADJ &                              key = iicekey, byte = isbyte
CADJ STORE d_hsnwbyocnonsnw = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = r_QbyATM_cover(I,J) + r_QbyATM_open(I,J)
     &         +   a_QSWbyATM_cover(I,J)
     &         - ( d_HEFFbyOCNonICE(I,J)
     &           + d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &           + d_HEFFbyNEG(I,J,bi,bj)
#ifdef EXF_SEAICE_FRACTION
     &           + d_HEFFbyRLX(I,J,bi,bj)
#endif
     &           + d_HSNWbyNEG(I,J,bi,bj)*SNOW2ICE
     &           - convertPRECIP2HI *
     &             snowPrecip(i,j,bi,bj) * (ONE-AREApreTH(I,J))
     &           ) * HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          QSW(I,J,bi,bj)  = a_QSWbyATM_cover(I,J) + a_QSWbyATM_open(I,J)
         ENDDO
        ENDDO

C switch heat fluxes from 'effective' ice meters to W/m2
C ======================================================

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)*convertHI2Q
          QSW(I,J,bi,bj)  = QSW(I,J,bi,bj)*convertHI2Q
         ENDDO
        ENDDO

#ifndef SEAICE_DISABLE_HEATCONSFIX
C treat advective heat flux by ocean to ice water exchange (at 0decC)
C ===================================================================
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_HEFFbyNEG(:,:,bi,bj) = comlev1_bibj,
CADJ &                              key = iicekey, byte = isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HSNWbyNEG(:,:,bi,bj) = comlev1_bibj,
CADJ &                              key = iicekey, byte = isbyte
CADJ STORE d_HSNWbyOCNonSNW = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HSNWbyATMonSNW = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE theta(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
Cgf Unlike for evap and precip, the temperature of gained/lost
C ocean liquid water due to melt/freeze of solid water cannot be chosen
C arbitrarily to be e.g. the ocean SST. Indeed the present seaice model
C implies a constant ice temperature of 0degC. If melt/freeze water is exchanged
C at a different temperature, it leads to a loss of conservation in the
C ocean+ice system. While this is mostly a serious issue in the
C real fresh water + non linear free surface framework, a mismatch
C between ice and ocean boundary condition can result in all cases.
C Below we therefore anticipate on external_forcing_surf.F
C to diagnoze and/or apply the correction to QNET.
        DO J=1,sNy
         DO I=1,sNx
catn: initialize tmpscal1
           tmpscal1 = ZERO
C ocean water going to ice/snow, in precip units
           tmpscal3=rhoConstFresh*HEFFM(I,J,bi,bj)*(
     &       ( d_HSNWbyATMonSNW(I,J)*SNOW2ICE
     &       + d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &       + d_HEFFbyOCNonICE(I,J) + d_HEFFbyATMonOCN(I,J)
     &       + d_HEFFbyNEG(I,J,bi,bj)+ d_HSNWbyNEG(I,J,bi,bj)*SNOW2ICE )
     &       * convertHI2PRECIP
     &       - snowPrecip(i,j,bi,bj) * (ONE-AREApreTH(I,J)) )
C factor in the heat content as done in external_forcing_surf.F
           IF ( (temp_EvPrRn.NE.UNSET_RL).AND.
     &         useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
             tmpscal1 = - tmpscal3*
     &         HeatCapacity_Cp * temp_EvPrRn
           ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL).AND.
     &         useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
             tmpscal1 = - tmpscal3*
     &         HeatCapacity_Cp * theta(I,J,kSurface,bi,bj)
           ELSEIF ( (temp_EvPrRn.NE.UNSET_RL) ) THEN
             tmpscal1 = - tmpscal3*HeatCapacity_Cp*
     &       ( temp_EvPrRn - theta(I,J,kSurface,bi,bj) )
           ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL) ) THEN
             tmpscal1 = ZERO
           ENDIF
#ifdef ALLOW_DIAGNOSTICS
C in all cases, diagnoze the boundary condition mismatch to SIaaflux
           DIAGarrayA(I,J)=tmpscal1
#endif
C remove the mismatch when real fresh water is exchanged (at 0degC here)
           IF ( useRealFreshWaterFlux.AND.(nonlinFreeSurf.GT.0)
     &          .AND.SEAICEheatConsFix )
     &       QNET(I,J,bi,bj)=QNET(I,J,bi,bj)+tmpscal1
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &        'SIaaflux',0,1,3,bi,bj,myThid)
        ENDIF
#endif
#endif /* ndef SEAICE_DISABLE_HEATCONSFIX */

C compute the net heat flux, incl. adv. by water, entering ocean+ice
C ===================================================================
        DO J=1,sNy
         DO I=1,sNx
Cgf 1) SIatmQnt (analogous to qnet; excl. adv. by water exch.)
CML If I consider the atmosphere above the ice, the surface flux
CML which is relevant for the air temperature dT/dt Eq
CML accounts for sensible and radiation (with different treatment
CML according to wave-length) fluxes but not for "latent heat flux",
CML since it does not contribute to heating the air.
CML So this diagnostic is only good for heat budget calculations within
CML the ice-ocean system.
           SIatmQnt(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)*convertHI2Q*(
     &            a_QSWbyATM_cover(I,J) +
     &            a_QbyATM_cover(I,J) + a_QbyATM_open(I,J) )
Cgf 2) SItflux (analogous to tflux; includes advection by water
C             exchanged between atmosphere and ocean+ice)
C solid water going to atm, in precip units
           tmpscal1 = rhoConstFresh*HEFFM(I,J,bi,bj)
     &       * convertHI2PRECIP * ( - d_HSNWbyRAIN(I,J)*SNOW2ICE
     &       + a_FWbySublim(I,J) - r_FWbySublim(I,J) )
C liquid water going to atm, in precip units
           tmpscal2=rhoConstFresh*HEFFM(I,J,bi,bj)*
     &       ( ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &         + ( d_HFRWbyRAIN(I,J) + r_FWbySublim(I,J) )
     &         *convertHI2PRECIP )
C In real fresh water flux + nonlinFS, we factor in the advected specific
C energy (referenced to 0 for 0deC liquid water). In virtual salt flux or
C linFS, rain/evap get a special treatment (see external_forcing_surf.F).
           tmpscal1= - tmpscal1*
     &       ( -SEAICE_lhFusion + HeatCapacity_Cp * ZERO )
           IF ( (temp_EvPrRn.NE.UNSET_RL).AND.
     &          useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
             tmpscal2= - tmpscal2*
     &        ( ZERO + HeatCapacity_Cp * temp_EvPrRn )
           ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL).AND.
     &          useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
             tmpscal2= - tmpscal2*
     &        ( ZERO + HeatCapacity_Cp * theta(I,J,kSurface,bi,bj) )
           ELSEIF ( (temp_EvPrRn.NE.UNSET_RL) ) THEN
             tmpscal2= - tmpscal2*HeatCapacity_Cp*
     &        ( temp_EvPrRn - theta(I,J,kSurface,bi,bj) )
           ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL) ) THEN
             tmpscal2= ZERO
           ENDIF
           SItflux(I,J,bi,bj)= SIatmQnt(I,J,bi,bj)-tmpscal1-tmpscal2
         ENDDO
        ENDDO

C compute net fresh water flux leaving/entering
C the ocean, accounting for fresh/salt water stocks.
C ==================================================

        DO J=1,sNy
         DO I=1,sNx
          tmpscal1= d_HSNWbyATMonSNW(I,J)*SNOW2ICE
     &             +d_HFRWbyRAIN(I,J)
     &             +d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &             +d_HEFFbyOCNonICE(I,J)
     &             +d_HEFFbyATMonOCN(I,J)
     &             +d_HEFFbyNEG(I,J,bi,bj)
#ifdef EXF_SEAICE_FRACTION
     &             +d_HEFFbyRLX(I,J,bi,bj)
#endif
     &             +d_HSNWbyNEG(I,J,bi,bj)*SNOW2ICE
C     If r_FWbySublim>0, then it is evaporated from ocean.
     &             +r_FWbySublim(I,J)
          EmPmR(I,J,bi,bj)  = HEFFM(I,J,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &         + tmpscal1*convertHI2PRECIP
     &         )*rhoConstFresh
#ifdef SEAICE_ITD
C     beware of the sign: fw2ObyRidge is snow mass moved into the ocean
C     by ridging, so requires a minus sign
     &         - fw2ObyRidge(I,J,bi,bj)*recip_deltaTtherm
     &           * HEFFM(I,J,bi,bj)
#endif /* SEAICE_ITD */
C and the flux leaving/entering the ocean+ice
           SIatmFW(I,J,bi,bj) = HEFFM(I,J,bi,bj)*(
     &          EVAP(I,J,bi,bj)*( ONE - AREApreTH(I,J) )
     &          - PRECIP(I,J,bi,bj)
#ifdef ALLOW_RUNOFF
     &          - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &           )*rhoConstFresh
     &     + a_FWbySublim(I,J) * SEAICE_rhoIce * recip_deltaTtherm

         ENDDO
        ENDDO

#ifdef ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION
C--
        DO J=1,sNy
         DO I=1,sNx
            frWtrAtm(I,J,bi,bj) = HEFFM(I,J,bi,bj)*(
     &           PRECIP(I,J,bi,bj)
     &           - EVAP(I,J,bi,bj)*( ONE - AREApreTH(I,J) )
# ifdef ALLOW_RUNOFF
     &           + RUNOFF(I,J,bi,bj)
# endif /* ALLOW_RUNOFF */
     &           )*rhoConstFresh
# ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
     &           - a_FWbySublim(I,J)*AREApreTH(I,J)
# endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
         ENDDO
        ENDDO
C--
#else  /* ndef ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION */
C--
# ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
        DO J=1,sNy
         DO I=1,sNx
          frWtrAtm(I,J,bi,bj) = HEFFM(I,J,bi,bj)*(
     &         PRECIP(I,J,bi,bj)
     &         - EVAP(I,J,bi,bj)
     &         *( ONE - AREApreTH(I,J) )
#  ifdef ALLOW_RUNOFF
     &         + RUNOFF(I,J,bi,bj)
#  endif /* ALLOW_RUNOFF */
     &         )*rhoConstFresh
     &     - a_FWbySublim(I,J) * SEAICE_rhoIce * recip_deltaTtherm
         ENDDO
        ENDDO
# endif
C--
#endif /* ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION */

#ifdef SEAICE_DEBUG
       IF ( plotLevel.GE.debLevC ) THEN
        CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
        CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
        CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
       ENDIF
#endif /* SEAICE_DEBUG */

C Sea Ice Load on the sea surface.
C =================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

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

#ifdef ALLOW_BALANCE_FLUXES
C Compute tile integrals of heat/fresh water fluxes to/from atm.
C ==============================================================
        FWFsiTile(bi,bj) = 0. _d 0
        IF ( selectBalanceEmPmR.EQ.1 ) THEN
         DO j=1,sNy
          DO i=1,sNx
           FWFsiTile(bi,bj) =
     &       FWFsiTile(bi,bj) + SIatmFW(i,j,bi,bj)
     &       * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
C to translate global mean FWF adjustements (see below) we may need :
        FWF2HFsiTile(bi,bj) = 0. _d 0
        IF ( selectBalanceEmPmR.EQ.1 .AND.
     &       temp_EvPrRn.EQ.UNSET_RL ) THEN
         DO j=1,sNy
          DO i=1,sNx
           FWF2HFsiTile(bi,bj) = FWF2HFsiTile(bi,bj) +
     &       HeatCapacity_Cp * theta(I,J,kSurface,bi,bj)
     &       * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
        HFsiTile(bi,bj) = 0. _d 0
        IF ( balanceQnet ) THEN
         DO j=1,sNy
          DO i=1,sNx
           HFsiTile(bi,bj) =
     &       HFsiTile(bi,bj) + SItflux(i,j,bi,bj)
     &       * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_BALANCE_FLUXES */

C ===================================================================
C ======================PART 8: diagnostics==========================
C ===================================================================

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         tmpscal1=1. _d 0 * recip_deltaTtherm
         CALL DIAGNOSTICS_SCALE_FILL(a_QbyATM_cover,
     &      tmpscal1,1,'SIaQbATC',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(a_QbyATM_open,
     &      tmpscal1,1,'SIaQbATO',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(a_QbyOCN,
     &      tmpscal1,1,'SIaQbOCN',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HEFFbyOCNonICE,
     &      tmpscal1,1,'SIdHbOCN',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HEFFbyATMonOCN_cover,
     &      tmpscal1,1,'SIdHbATC',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HEFFbyATMonOCN_open,
     &      tmpscal1,1,'SIdHbATO',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HEFFbyFLOODING,
     &      tmpscal1,1,'SIdHbFLO',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HSNWbyOCNonSNW,
     &      tmpscal1,1,'SIdSbOCN',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_HSNWbyATMonSNW,
     &      tmpscal1,1,'SIdSbATC',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_AREAbyATM,
     &      tmpscal1,1,'SIdAbATO',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_AREAbyICE,
     &      tmpscal1,1,'SIdAbATC',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(d_AREAbyOCN,
     &      tmpscal1,1,'SIdAbOCN',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(r_QbyATM_open,
     &      convertHI2Q,1, 'SIqneto ',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(r_QbyATM_cover,
     &      convertHI2Q,1, 'SIqneti ',0,1,3,bi,bj,myThid)
C three that actually need intermediate storage
         DO J=1,sNy
          DO I=1,sNx
            DIAGarrayA(I,J) = HEFFM(I,J,bi,bj)
     &        * d_HSNWbyRAIN(I,J)*SEAICE_rhoSnow*recip_deltaTtherm
            DIAGarrayB(I,J) =  AREA(I,J,bi,bj)-AREApreTH(I,J)
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIsnPrcp',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(DIAGarrayB,
     &      tmpscal1,1,'SIdA    ',0,1,3,bi,bj,myThid)
         DO J=1,sNy
          DO I=1,sNx
           DIAGarrayB(I,J) = HEFFM(I,J,bi,bj) *
     &       a_FWbySublim(I,J) * SEAICE_rhoIce * recip_deltaTtherm
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayB,
     &      'SIfwSubl',0,1,3,bi,bj,myThid)
C
         DO J=1,sNy
          DO I=1,sNx
C the actual Freshwater flux of sublimated ice, >0 decreases ice
           DIAGarrayA(I,J) = HEFFM(I,J,bi,bj)
     &       * (a_FWbySublim(I,J)-r_FWbySublim(I,J))
     &       * SEAICE_rhoIce * recip_deltaTtherm
C the residual Freshwater flux of sublimated ice
           DIAGarrayC(I,J) = HEFFM(I,J,bi,bj)
     &       * r_FWbySublim(I,J)
     &       * SEAICE_rhoIce * recip_deltaTtherm
C the latent heat flux
           tmpscal1= EVAP(I,J,bi,bj)*( ONE - AREApreTH(I,J) )
     &             + r_FWbySublim(I,J)*convertHI2PRECIP
           tmpscal2= ( a_FWbySublim(I,J)-r_FWbySublim(I,J) )
     &             * convertHI2PRECIP
           tmpscal3= SEAICE_lhEvap+SEAICE_lhFusion
           DIAGarrayB(I,J) = -HEFFM(I,J,bi,bj)*rhoConstFresh
     &             * ( tmpscal1*SEAICE_lhEvap + tmpscal2*tmpscal3 )
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayA,'SIacSubl',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayC,'SIrsSubl',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayB,'SIhl    ',0,1,3,bi,bj,myThid)
# if defined ( ALLOW_SITRACER ) && defined ( SEAICE_GREASE )
C actual grease ice layer thickness
         CALL DIAGNOSTICS_FILL(greaseLayerThick,
     &        'SIgrsLT ',0,1,3,bi,bj,myThid)
# endif /* SEAICE_GREASE */
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C close bi,bj loops
       ENDDO
      ENDDO

C ===================================================================
C =========PART 9: HF/FWF global integrals and balancing=============
C ===================================================================

#ifdef ALLOW_BALANCE_FLUXES

C 1)  global sums
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE FWFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE HFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE FWF2HFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
      FWFsiGlob = 0. _d 0
      FWF2HFsiGlob = 0. _d 0
      IF ( selectBalanceEmPmR.EQ.1 ) THEN
        CALL GLOBAL_SUM_TILE_RL( FWFsiTile, FWFsiGlob, myThid )
        IF ( temp_EvPrRn.EQ.UNSET_RL ) THEN
          CALL GLOBAL_SUM_TILE_RL( FWF2HFsiTile, FWF2HFsiGlob, myThid )
        ELSE
          FWF2HFsiGlob = HeatCapacity_Cp * temp_EvPrRn * globalArea
        ENDIF
      ENDIF
      HFsiGlob=0. _d 0
      IF ( balanceQnet )
     &   CALL GLOBAL_SUM_TILE_RL( HFsiTile, HFsiGlob, myThid )

C 2) global means
C mean SIatmFW
      tmpscal0=FWFsiGlob / globalArea
C corresponding mean advection by atm to ocean+ice water exchange
C        (if mean SIatmFW was removed uniformely from ocean)
      tmpscal1=FWFsiGlob / globalArea * FWF2HFsiGlob / globalArea
C mean SItflux (before potential adjustement due to SIatmFW)
      tmpscal2=HFsiGlob / globalArea
C mean SItflux (after potential adjustement due to SIatmFW)
      IF ( selectBalanceEmPmR.EQ.1 ) tmpscal2=tmpscal2-tmpscal1

C 3) balancing adjustments
      IF ( selectBalanceEmPmR.EQ.1 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            empmr(i,j,bi,bj) = empmr(i,j,bi,bj) - tmpscal0
            SIatmFW(i,j,bi,bj) = SIatmFW(i,j,bi,bj) - tmpscal0
C           adjust SItflux consistently
            IF ( (temp_EvPrRn.NE.UNSET_RL).AND.
     &        useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
            tmpscal1=
     &       ( ZERO + HeatCapacity_Cp * temp_EvPrRn )
            ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL).AND.
     &        useRealFreshWaterFlux.AND.(nonlinFreeSurf.NE.0) ) THEN
            tmpscal1=
     &       ( ZERO + HeatCapacity_Cp * theta(I,J,kSurface,bi,bj) )
            ELSEIF ( (temp_EvPrRn.NE.UNSET_RL) ) THEN
            tmpscal1=
     &       HeatCapacity_Cp*(temp_EvPrRn - theta(I,J,kSurface,bi,bj))
            ELSE
            tmpscal1=ZERO
            ENDIF
            SItflux(i,j,bi,bj) = SItflux(i,j,bi,bj) - tmpscal0*tmpscal1
C           no qnet or tflux adjustement is needed
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       IF ( balancePrintMean ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A,1PE21.14,A,I10)') 'rm Global mean of',
     &               ' SIatmFW = ', tmpscal0, '  @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
      ENDIF
      IF ( balanceQnet ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            SItflux(i,j,bi,bj) = SItflux(i,j,bi,bj) - tmpscal2
            qnet(i,j,bi,bj) = qnet(i,j,bi,bj) - tmpscal2
            SIatmQnt(i,j,bi,bj) = SIatmQnt(i,j,bi,bj) - tmpscal2
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       IF ( balancePrintMean ) THEN
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A,1PE21.14,A,I10)') 'rm Global mean of',
     &               ' SItflux = ', tmpscal2, '  @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        _END_MASTER( myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_BALANCE_FLUXES */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C these diags need to be done outside of the bi,bj loop so that
C we may do potential global mean adjustement to them consistently.
        CALL DIAGNOSTICS_FILL(SItflux,
     &        'SItflux ',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(SIatmQnt,
     &        'SIatmQnt',0,1,0,1,1,myThid)
C SIatmFW follows the same convention as empmr -- SIatmFW diag does not
        tmpscal1= - 1. _d 0
        CALL DIAGNOSTICS_SCALE_FILL(SIatmFW,
     &        tmpscal1,1,'SIatmFW ',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#else  /* ALLOW_EXF and ALLOW_ATM_TEMP */
      STOP 'SEAICE_GROWTH not compiled without EXF and ALLOW_ATM_TEMP'
#endif /* ALLOW_EXF and ALLOW_ATM_TEMP */
#endif /* ndef SEAICE_USE_GROWTH_ADX */

      RETURN
      END
