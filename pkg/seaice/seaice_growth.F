C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.175 2012/08/28 19:19:32 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
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
C     itc :: thickness category index (multi-categorires)
      INTEGER itc
      _RL pFac
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

C     Helper variables: reciprocal of some constants
      _RL recip_multDim
      _RL recip_deltaTtherm
      _RL recip_rhoIce
C     local value (=1/HO or 1/HO_south)
      _RL recip_HO
C     local value (=1/ice thickness)
      _RL recip_HH

C     temporary variables available for the various computations
      _RL tmpscal0, tmpscal1, tmpscal2, tmpscal3, tmpscal4

#ifdef ALLOW_SITRACER
      INTEGER iTr
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8   diagName
#endif
#endif /* ALLOW_SITRACER */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ilockey
#endif

C==   local arrays ==
C--   TmixLoc        :: ocean surface/mixed-layer temperature (in K)
      _RL TmixLoc       (1:sNx,1:sNy)

C     actual ice thickness (with upper and lower limit)
      _RL heffActual          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hsnowActual         (1:sNx,1:sNy)
C     actual ice thickness (with lower limit only) Reciprocal
      _RL recip_heffActual    (1:sNx,1:sNy)

C     AREA_PRE :: hold sea-ice fraction field before any seaice-thermo update
      _RL AREApreTH           (1:sNx,1:sNy)
      _RL HEFFpreTH           (1:sNx,1:sNy)
      _RL HSNWpreTH           (1:sNx,1:sNy)

C     wind speed
      _RL UG                  (1:sNx,1:sNy)

C     temporary variables available for the various computations
      _RL tmparr1             (1:sNx,1:sNy)
#ifdef SEAICE_VARIABLE_SALINITY
      _RL saltFluxAdjust      (1:sNx,1:sNy)
#endif

      _RL ticeInMult          (1:sNx,1:sNy,MULTDIM)
      _RL ticeOutMult         (1:sNx,1:sNy,MULTDIM)
      _RL heffActualMult      (1:sNx,1:sNy,MULTDIM)
      _RL a_QbyATMmult_cover  (1:sNx,1:sNy,MULTDIM)
      _RL a_QSWbyATMmult_cover(1:sNx,1:sNy,MULTDIM)
      _RL a_FWbySublimMult    (1:sNx,1:sNy,MULTDIM)

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
C     a_QbyOCN :: available heat (in in W/m^2) due to the
C             interaction of the ice pack and the ocean surface
C     r_QbyOCN :: residual of a_QbyOCN after freezing/melting
C             processes have been accounted for
      _RL a_QbyOCN            (1:sNx,1:sNy)
      _RL r_QbyOCN            (1:sNx,1:sNy)

C     The change of mean ice thickness due to out-of-bounds values following
C     sea ice dyhnamics
      _RL d_HEFFbyNEG         (1:sNx,1:sNy)

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

      _RL d_HSNWbyNEG         (1:sNx,1:sNy)
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
      _RL latentHeatFluxMaxMult(1:sNx,1:sNy,MULTDIM)
#endif

#ifdef EXF_ALLOW_SEAICE_RELAX
C     ICE/SNOW stocks tendency associated with relaxation towards observation
      _RL d_AREAbyRLX         (1:sNx,1:sNy)
C     The change of mean ice thickness due to relaxation
      _RL d_HEFFbyRLX         (1:sNx,1:sNy)
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
      CHARACTER*(max_len_mbuf) msgbuf
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C ===================================================================
C =================PART 0: constants and initializations=============
C ===================================================================

      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     avoid unnecessary divisions in loops
      recip_multDim        = SEAICE_multDim
      recip_multDim        = ONE / recip_multDim
C     above/below: double/single precision calculation of recip_multDim
c     recip_multDim        = 1./float(SEAICE_multDim)
      recip_deltaTtherm = ONE / SEAICE_deltaTtherm
      recip_rhoIce      = ONE / SEAICE_rhoIce

C     Cutoff for iceload
      heffTooHeavy=drF(kSurface) / 5. _d 0

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

#ifdef EXF_ALLOW_SEAICE_RELAX
          d_AREAbyRLX(I,J)           = 0.0 _d 0
          d_HEFFbyRLX(I,J)           = 0.0 _d 0
#endif

          d_HEFFbyNEG(I,J)           = 0.0 _d 0
          d_HEFFbyOCNonICE(I,J)      = 0.0 _d 0
          d_HEFFbyATMonOCN(I,J)      = 0.0 _d 0
          d_HEFFbyFLOODING(I,J)      = 0.0 _d 0

          d_HEFFbyATMonOCN_open(I,J) = 0.0 _d 0
          d_HEFFbyATMonOCN_cover(I,J) = 0.0 _d 0

          d_HSNWbyNEG(I,J)           = 0.0 _d 0
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
#ifdef SEAICE_VARIABLE_SALINITY
          saltFluxAdjust(I,J)        = 0.0 _d 0
#endif
          DO itc=1,SEAICE_multDim
            ticeInMult(I,J,itc)            = 0.0 _d 0
            ticeOutMult(I,J,itc)           = 0.0 _d 0
            a_QbyATMmult_cover(I,J,itc)    = 0.0 _d 0
            a_QSWbyATMmult_cover(I,J,itc)  = 0.0 _d 0
            a_FWbySublimMult(I,J,itc)      = 0.0 _d 0
#ifdef SEAICE_CAP_SUBLIM
            latentHeatFluxMaxMult(I,J,itc) = 0.0 _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
        DO J=1-oLy,sNy+oLy
         DO I=1-oLx,sNx+oLx
          frWtrAtm(I,J,bi,bj)        = 0.0 _d 0
         ENDDO
        ENDDO
#endif

C =====================================================================
C ===========PART 1: treat pathological cases (post advdiff)===========
C =====================================================================

#ifdef SEAICE_GROWTH_LEGACY

        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J)=HEFFNM1(I,J,bi,bj)
          HSNWpreTH(I,J)=HSNOW(I,J,bi,bj)
          AREApreTH(I,J)=AREANM1(I,J,bi,bj)
          d_HEFFbyNEG(I,J)=0. _d 0
          d_HSNWbyNEG(I,J)=0. _d 0
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = AREANM1(I,J,bi,bj)
          DIAGarrayB(I,J) = AREANM1(I,J,bi,bj)
          DIAGarrayC(I,J) = HEFFNM1(I,J,bi,bj)
          DIAGarrayD(I,J) = HSNOW(I,J,bi,bj)
#endif
         ENDDO
        ENDDO

#else /* SEAICE_GROWTH_LEGACY */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf no dependency through pathological cases treatment
        IF ( SEAICEadjMODE.EQ.0 ) THEN
#endif

#ifdef EXF_ALLOW_SEAICE_RELAX
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
C 0) relax sea ice concentration towards observation
        IF (SEAICE_tauAreaObsRelax .GT. 0.) THEN
        DO J=1,sNy
         DO I=1,sNx
C         d_AREAbyRLX(i,j) = 0. _d 0
C         d_HEFFbyRLX(i,j) = 0. _d 0
           IF ( obsSIce(I,J,bi,bj).GT.AREA(I,J,bi,bj)) THEN
            d_AREAbyRLX(i,j) =
     &       SEAICE_deltaTtherm/SEAICE_tauAreaObsRelax
     &       * (obsSIce(I,J,bi,bj) - AREA(I,J,bi,bj))
           ENDIF
           IF ( obsSIce(I,J,bi,bj).GT.0. _d 0 .AND.
     &          AREA(I,J,bi,bj).EQ.0. _d 0) THEN
C           d_HEFFbyRLX(i,j) = 1. _d 1 * siEps * d_AREAbyRLX(i,j)
            d_HEFFbyRLX(i,j) = 1. _d 1 * siEps
           ENDIF
            AREA(I,J,bi,bj) = AREA(I,J,bi,bj) +  d_AREAbyRLX(i,j)
            HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) +  d_HEFFbyRLX(i,j)
         ENDDO
        ENDDO
        ENDIF
#endif /* EXF_ALLOW_SEAICE_RELAX */

C 1) treat the case of negative values:

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          d_HEFFbyNEG(I,J)=MAX(-HEFF(I,J,bi,bj),0. _d 0)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj)+d_HEFFbyNEG(I,J)
          d_HSNWbyNEG(I,J)=MAX(-HSNOW(I,J,bi,bj),0. _d 0)
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)+d_HSNWbyNEG(I,J)
          AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),0. _d 0)
         ENDDO
        ENDDO

C 1.25) treat the case of very thin ice:

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal2=0. _d 0
          tmpscal3=0. _d 0
          IF (HEFF(I,J,bi,bj).LE.siEps) THEN
            tmpscal2=-HEFF(I,J,bi,bj)
            tmpscal3=-HSNOW(I,J,bi,bj)
            TICE(I,J,bi,bj)=celsius2K
            DO itc=1,SEAICE_multDim
              TICES(I,J,itc,bi,bj)=celsius2K
            ENDDO
          ENDIF
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj)+tmpscal2
          d_HEFFbyNEG(I,J)=d_HEFFbyNEG(I,J)+tmpscal2
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)+tmpscal3
          d_HSNWbyNEG(I,J)=d_HSNWbyNEG(I,J)+tmpscal3
         ENDDO
        ENDDO

C 1.5) treat the case of area but no ice/snow:

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ((HEFF(i,j,bi,bj).EQ.0. _d 0).AND.
     &        (HSNOW(i,j,bi,bj).EQ.0. _d 0)) AREA(I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO

C 2) treat the case of very small area:

#ifndef DISABLE_AREA_FLOOR
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ((HEFF(i,j,bi,bj).GT.0).OR.(HSNOW(i,j,bi,bj).GT.0)) THEN
           AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),SEAICE_area_floor)
          ENDIF
         ENDDO
        ENDDO
#endif /* DISABLE_AREA_FLOOR */

C 2.5) treat case of excessive ice cover, e.g., due to ridging:

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = AREA(I,J,bi,bj)
#endif
#ifdef ALLOW_SITRACER
          SItrAREA(I,J,bi,bj,1)=AREA(I,J,bi,bj)
#endif
          AREA(I,J,bi,bj)=MIN(AREA(I,J,bi,bj),SEAICE_area_max)
         ENDDO
        ENDDO

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
        ENDIF
#endif

C 3) store regularized values of heff, hsnow, area at the onset of thermo.
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

C 4) treat sea ice salinity pathological cases
#ifdef SEAICE_VARIABLE_SALINITY
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ( (HSALT(I,J,bi,bj) .LT. 0.0).OR.
     &         (HEFF(I,J,bi,bj) .EQ. 0.0)  ) THEN
             saltFluxAdjust(I,J) = - HEFFM(I,J,bi,bj) *
     &            HSALT(I,J,bi,bj) * recip_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0 _d 0
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */

#endif /* SEAICE_GROWTH_LEGACY */

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(DIAGarrayA,'SIareaPR',0,1,3,bi,bj,myThid)
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
        ENDIF
#endif

#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
        DO J=1,sNy
         DO I=1,sNx
          AREAforAtmFW(I,J,bi,bj) = AREApreTH(I,J)
         ENDDO
        ENDDO
#endif

C 4) COMPUTE ACTUAL ICE/SNOW THICKNESS; USE MIN/MAX VALUES
C    TO REGULARIZE SEAICE_SOLVE4TEMP/d_AREA COMPUTATIONS

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE AREApreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HEFFpreTH = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE HSNWpreTH = comlev1_bibj, key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx

          IF (HEFFpreTH(I,J) .GT. ZERO) THEN
#ifdef SEAICE_GROWTH_LEGACY
            tmpscal1         = MAX(SEAICE_area_reg,AREApreTH(I,J))
            hsnowActual(I,J) = HSNWpreTH(I,J)/tmpscal1
            tmpscal2         = HEFFpreTH(I,J)/tmpscal1
            heffActual(I,J)  = MAX(tmpscal2,SEAICE_hice_reg)
#else /* SEAICE_GROWTH_LEGACY */
Cif        regularize AREA with SEAICE_area_reg
           tmpscal1 = SQRT(AREApreTH(I,J)* AREApreTH(I,J) + area_reg_sq)
Cif        heffActual calculated with the regularized AREA
           tmpscal2 = HEFFpreTH(I,J) / tmpscal1
Cif        regularize heffActual with SEAICE_hice_reg (add lower bound)
           heffActual(I,J) = SQRT(tmpscal2 * tmpscal2 + hice_reg_sq)
Cif        hsnowActual calculated with the regularized AREA
           hsnowActual(I,J) = HSNWpreTH(I,J) / tmpscal1
#endif /* SEAICE_GROWTH_LEGACY */
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

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
        CALL ZERO_ADJ_1D( sNx*sNy, heffActual, myThid)
        CALL ZERO_ADJ_1D( sNx*sNy, hsnowActual, myThid)
        CALL ZERO_ADJ_1D( sNx*sNy, recip_heffActual, myThid)
#endif

#ifdef SEAICE_CAP_SUBLIM
C 5) COMPUTE MAXIMUM LATENT HEAT FLUXES FOR THE CURRENT ICE
C    AND SNOW THICKNESS
        DO J=1,sNy
         DO I=1,sNx
C        The latent heat flux over the sea ice which
C        will sublimate all of the snow and ice over one time
C        step (W/m^2)
          IF (HEFFpreTH(I,J) .GT. ZERO) THEN
            latentHeatFluxMax(I,J) = lhSublim * recip_deltaTtherm *
     &         (HEFFpreTH(I,J) * SEAICE_rhoIce +
     &          HSNWpreTH(I,J) * SEAICE_rhoSnow)/AREApreTH(I,J)
          ELSE
            latentHeatFluxMax(I,J) = ZERO
          ENDIF
         ENDDO
        ENDDO
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
     &          +0.5 _d 0*(uVel(i,j,kSurface,bi,bj)
     &                    +uVel(i+1,j,kSurface,bi,bj))
     &          -0.5 _d 0*(uice(i,j,bi,bj)+uice(i+1,j,bi,bj)))**2
     &          +(vWind(I,J,bi,bj)
     &          +0.5 _d 0*(vVel(i,j,kSurface,bi,bj)
     &                    +vVel(i,j+1,kSurface,bi,bj))
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
CADJ STORE tice(:,:,bi,bj)
CADJ &     = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE hsnowActual = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE heffActual  = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE UG          = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE tices(:,:,:,bi,bj)
CADJ &     = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Start loop over multi-categories
        DO itc=1,SEAICE_multDim
C        homogeneous distribution between 0 and 2 x heffActual
         pFac = (2.0 _d 0*itc - 1.0 _d 0)*recip_multDim
         DO J=1,sNy
          DO I=1,sNx
           heffActualMult(I,J,itc)= heffActual(I,J)*pFac
#ifdef SEAICE_CAP_SUBLIM
           latentHeatFluxMaxMult(I,J,itc) = latentHeatFluxMax(I,J)*pFac
#endif
           ticeInMult(I,J,itc)=TICES(I,J,itc,bi,bj)
           ticeOutMult(I,J,itc)=TICES(I,J,itc,bi,bj)
           TICE(I,J,bi,bj) = ZERO
           TICES(I,J,itc,bi,bj) = ZERO
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffActualMult = comlev1_bibj, key = iicekey, byte = isbyte
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

        DO itc=1,SEAICE_multDim
         CALL SEAICE_SOLVE4TEMP(
     I        UG, heffActualMult(1,1,itc), hsnowActual,
#ifdef SEAICE_CAP_SUBLIM
     I        latentHeatFluxMaxMult(1,1,itc),
#endif
     U        ticeInMult(1,1,itc), ticeOutMult(1,1,itc),
     O        a_QbyATMmult_cover(1,1,itc),
     O        a_QSWbyATMmult_cover(1,1,itc),
     O        a_FWbySublimMult(1,1,itc),
     I        bi, bj, myTime, myIter, myThid )
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffActualMult = comlev1_bibj, key = iicekey, byte = isbyte
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

        DO itc=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C     update TICE & TICES
           TICE(I,J,bi,bj) = TICE(I,J,bi,bj)
     &          +  ticeOutMult(I,J,itc)*recip_multDim
           TICES(I,J,itc,bi,bj) = ticeOutMult(I,J,itc)
C     average over categories
           a_QbyATM_cover   (I,J) = a_QbyATM_cover(I,J)
     &          + a_QbyATMmult_cover(I,J,itc)*recip_multDim
           a_QSWbyATM_cover (I,J) = a_QSWbyATM_cover(I,J)
     &          + a_QSWbyATMmult_cover(I,J,itc)*recip_multDim
           a_FWbySublim     (I,J) = a_FWbySublim(I,J)
     &          + a_FWbySublimMult(I,J,itc)*recip_multDim
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
         DO J=1,sNy
          DO I=1,sNx
           a_QbyATM_cover(I,J)   = 0. _d 0
           r_QbyATM_cover(I,J)   = 0. _d 0
           a_QSWbyATM_cover(I,J) = 0. _d 0
          ENDDO
         ENDDO
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
           tmpscal1 =SEAICE_frazilFrac*drF(kSurface)/SEAICE_deltaTtherm
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
     &         * SEAICE_deltaTtherm * maskC(i,j,kSurface,bi,bj)
C available turbulent flux
          a_QbyOCN(i,j) =
     &         tmpscal1 * tmpscal2 * MixedLayerTurbulenceFactor
          r_QbyOCN(i,j) = a_QbyOCN(i,j)
         ENDDO
        ENDDO

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
        CALL ZERO_ADJ_1D( sNx*sNy, r_QbyOCN, myThid)
#endif

C ===================================================================
C =========PART 3: determine effective thicknesses increments========
C ===================================================================

C compute snow/ice tendency due to sublimation
C ============================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_FWbySublim     = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     First sublimate/deposite snow
          tmpscal2 =
     &     MAX(MIN(r_FWbySublim(I,J),HSNOW(I,J,bi,bj)*SNOW2ICE),ZERO)
          d_HSNWbySublim(I,J) = - tmpscal2 * ICE2SNOW
          HSNOW(I,J,bi,bj)    = HSNOW(I,J,bi,bj)  - tmpscal2*ICE2SNOW
          r_FWbySublim(I,J)   = r_FWbySublim(I,J) - tmpscal2
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
     &     MAX(MIN(r_FWbySublim(I,J),HEFF(I,J,bi,bj)),ZERO)
          d_HEFFbySublim(I,J) = - tmpscal2
          HEFF(I,J,bi,bj)     = HEFF(I,J,bi,bj)   - tmpscal2
          r_FWbySublim(I,J)   = r_FWbySublim(I,J) - tmpscal2
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
C     If anything is left, it will be evaporated from the ocean rather than sublimated.
C     Since a_QbyATM_cover was computed for sublimation, not simple evapation, we need to
C     remove the fusion part for the residual (that happens to be precisely r_FWbySublim).
          a_QbyATM_cover(I,J) = a_QbyATM_cover(I,J)-r_FWbySublim(I,J)
          r_QbyATM_cover(I,J) = r_QbyATM_cover(I,J)-r_FWbySublim(I,J)
         ENDDO
        ENDDO

C compute ice thickness tendency due to ice-ocean interaction
C ===========================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

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

C compute snow melt tendency due to snow-atmosphere interaction
C ==================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

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

        DO J=1,sNy
         DO I=1,sNx

#ifdef SEAICE_GROWTH_LEGACY
          tmpscal2 = MAX(-HEFF(I,J,bi,bj),r_QbyATM_cover(I,J))
#else
          tmpscal2 = MAX(-HEFF(I,J,bi,bj),r_QbyATM_cover(I,J)+
C         Limit ice growth by potential melt by ocean
     &        AREApreTH(I,J) * r_QbyOCN(I,J))
#endif /* SEAICE_GROWTH_LEGACY */

          d_HEFFbyATMonOCN_cover(I,J)=tmpscal2
          d_HEFFbyATMonOCN(I,J)=d_HEFFbyATMonOCN(I,J)+tmpscal2
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)-tmpscal2
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + tmpscal2

#ifdef ALLOW_SITRACER
          SItrHEFF(I,J,bi,bj,3)=HEFF(I,J,bi,bj)
#endif
         ENDDO
        ENDDO

C add snow precipitation to HSNOW.
C =================================================
#ifdef ALLOW_ATM_TEMP
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE a_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE PRECIP(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREApreTH = comlev1_bibj,key=iicekey,byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
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
c          IF (TICE(I,J,bi,bj) .LT. TMIX) THEN
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
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbyRAIN(I,J)
          ENDDO
         ENDDO
Cgf note: this does not affect air-sea heat flux,
Cgf since the implied air heat gain to turn
Cgf rain to snow is not a surface process.
        ENDIF
#endif /* ALLOW_ATM_TEMP */

C compute snow melt due to heat available from ocean.
C =================================================================

Cgf do we need to keep this comment and cpp bracket?
Cph( very sensitive bit here by JZ
#ifndef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
     &           -HEFF(I,J,bi,bj)*facOpenMelt)*HEFFM(I,J,bi,bj)
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
          SItrHEFF(I,J,bi,bj,4)=HEFF(I,J,bi,bj)
         ENDDO
        ENDDO
#endif /* ALLOW_SITRACER */

C convert snow to ice if submerged.
C =================================

#ifndef SEAICE_GROWTH_LEGACY
C note: in legacy, this process is done at the end
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        IF ( SEAICEuseFlooding ) THEN
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
        ENDIF
#endif /* SEAICE_GROWTH_LEGACY */

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

        DO J=1,sNy
         DO I=1,sNx

          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           recip_HO=1. _d 0 / HO_south
          ELSE
           recip_HO=1. _d 0 / HO
          ENDIF
#ifdef SEAICE_GROWTH_LEGACY
           tmpscal0=HEFF(I,J,bi,bj) - d_HEFFbyATMonOCN(I,J)
           recip_HH = AREApreTH(I,J) /(tmpscal0+.00001 _d 0)
#else
           recip_HH = recip_heffActual(I,J)
#endif

C gain of ice over open water : computed from
C   (SEAICE_areaGainFormula.EQ.1) from growth by ATM
C   (SEAICE_areaGainFormula.EQ.2) from predicted growth by ATM
          IF (SEAICE_areaGainFormula.EQ.1) THEN
            tmpscal4 = MAX(ZERO,d_HEFFbyATMonOCN_open(I,J))
          ELSE
            tmpscal4=MAX(ZERO,a_QbyATM_open(I,J))
          ENDIF

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
     &       + recip_HO*tmpscal4+HALF*recip_HH*tmpscal3 ))
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
          d_AREAbyICE(I,J)=
     &        HALF*recip_HH*MIN(0. _d 0,d_HEFFbyATMonOCN_cover(I,J))
          d_AREAbyOCN(I,J)=
     &        HALF*recip_HH*MIN( 0. _d 0,d_HEFFbyOCNonICE(I,J) )
#endif /* ALLOW_DIAGNOSTICS */
         ENDDO
        ENDDO

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf 'bulk' linearization of area=f(HEFF)
        IF ( SEAICEadjMODE.GE.1 ) THEN
         DO J=1,sNy
          DO I=1,sNx
C            AREA(I,J,bi,bj) = 0.1 _d 0 * HEFF(I,J,bi,bj)
           AREA(I,J,bi,bj) = AREApreTH(I,J) + 0.1 _d 0 *
     &               ( HEFF(I,J,bi,bj) - HEFFpreTH(I,J) )
          ENDDO
         ENDDO
        ENDIF
#endif

C ===================================================================
C =============PART 5: determine ice salinity increments=============
C ===================================================================

#ifndef SEAICE_VARIABLE_SALINITY
# if (defined ALLOW_AUTODIFF_TAMC && defined ALLOW_SALT_PLUME)
CADJ STORE d_HEFFbyNEG = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyFLOODING = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbySublim = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
# endif /* ALLOW_AUTODIFF_TAMC and ALLOW_SALT_PLUME */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1 = d_HEFFbyNEG(I,J) + d_HEFFbyOCNonICE(I,J) +
     &               d_HEFFbyATMonOCN(I,J) + d_HEFFbyFLOODING(I,J)
     &             + d_HEFFbySublim(I,J)
#ifdef EXF_ALLOW_SEAICE_RELAX
     &             + d_HEFFbyRLX(I,J)
#endif
          tmpscal2 = tmpscal1 * SEAICE_salt0 * HEFFM(I,J,bi,bj)
     &            * recip_deltaTtherm * SEAICE_rhoIce
          saltFlux(I,J,bi,bj) = tmpscal2
#ifdef ALLOW_SALT_PLUME
          tmpscal3 = tmpscal1*salt(I,J,kSurface,bi,bj)*HEFFM(I,J,bi,bj)
     &            * recip_deltaTtherm * SEAICE_rhoIce
          saltPlumeFlux(I,J,bi,bj) = MAX( tmpscal3-tmpscal2 , 0. _d 0)
     &            *SPsalFRAC
#endif /* ALLOW_SALT_PLUME */
         ENDDO
        ENDDO
#endif /* ndef SEAICE_VARIABLE_SALINITY */

#ifdef SEAICE_VARIABLE_SALINITY

#ifdef SEAICE_GROWTH_LEGACY
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C set HSALT = 0 if HSALT < 0 and compute salt to remove from ocean
          IF ( HSALT(I,J,bi,bj) .LT. 0.0 ) THEN
             saltFluxAdjust(I,J) = - HEFFM(I,J,bi,bj) *
     &            HSALT(I,J,bi,bj) * recip_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0 _d 0
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_GROWTH_LEGACY */

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
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &            *(ONE-SEAICE_saltFrac)*salt(I,J,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
     &            *SPsalFRAC
C if SaltPlumeSouthernOcean=.FALSE. turn off salt plume in Southern Ocean
             IF ( .NOT. SaltPlumeSouthernOcean ) THEN
              IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 )
     &             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
             ENDIF
#endif /* ALLOW_SALT_PLUME */

C tmpscal1 < 0 : m of sea ice that is melted
          ELSE
             saltFlux(I,J,bi,bj) =
     &         HEFFM(I,J,bi,bj)*recip_deltaTtherm
     &         *HSALT(I,J,bi,bj)
     &         *tmpscal1/tmpscal2
#ifdef ALLOW_SALT_PLUME
             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
#endif /* ALLOW_SALT_PLUME */
          ENDIF
C update HSALT based on surface saltFlux
          HSALT(I,J,bi,bj) = HSALT(I,J,bi,bj) +
     &         saltFlux(I,J,bi,bj) * SEAICE_deltaTtherm
          saltFlux(I,J,bi,bj) =
     &         saltFlux(I,J,bi,bj) + saltFluxAdjust(I,J)
#ifdef SEAICE_GROWTH_LEGACY
C set HSALT = 0 if HEFF = 0 and compute salt to dump into ocean
          IF ( HEFF(I,J,bi,bj) .EQ. 0.0 ) THEN
           saltFlux(I,J,bi,bj) = saltFlux(I,J,bi,bj) -
     &          HEFFM(I,J,bi,bj) * HSALT(I,J,bi,bj) * recip_deltaTtherm
           HSALT(I,J,bi,bj) = 0.0 _d 0
#ifdef ALLOW_SALT_PLUME
           saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
#endif /* ALLOW_SALT_PLUME */
          ENDIF
#endif /* SEAICE_GROWTH_LEGACY */
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */

C =======================================================================
C ==LEGACY PART 6 (LEGACY) treat pathological cases, then do flooding ===
C =======================================================================

#ifdef SEAICE_GROWTH_LEGACY

C treat values of ice cover fraction oustide
C the [0 1] range, and other such issues.
C ===========================================

Cgf note: this part cannot be heat and water conserving

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     NOW SET AREA(I,J,bi,bj)=0 WHERE THERE IS NO ICE
CML   replaced "/.0001 _d 0" by "*1. _d 4", 1e-4 is probably
CML   meant to be something like a minimum thickness
          AREA(I,J,bi,bj)=MIN(AREA(I,J,bi,bj),HEFF(I,J,bi,bj)*1. _d 4)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C NOW TRUNCATE AREA
          AREA(I,J,bi,bj)=MIN(ONE,AREA(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          AREA(I,J,bi,bj) = MAX(ZERO,AREA(I,J,bi,bj))
          HSNOW(I,J,bi,bj)  = MAX(ZERO,HSNOW(I,J,bi,bj))
          AREA(I,J,bi,bj) = AREA(I,J,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)*HEFFM(I,J,bi,bj)
#ifdef SEAICE_CAP_HEFF
C     This is not energy conserving, but at least it conserves fresh water
          tmpscal0         = -MAX(HEFF(I,J,bi,bj)-MAX_HEFF,0. _d 0)
          d_HEFFbyNeg(I,J) = d_HEFFbyNeg(I,J) + tmpscal0
          HEFF(I,J,bi,bj)  = HEFF(I,J,bi,bj)  + tmpscal0
#endif /* SEAICE_CAP_HEFF */
          HSNOW(I,J,bi,bj)  = HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

C convert snow to ice if submerged.
C =================================

        IF ( SEAICEuseFlooding ) THEN
         DO J=1,sNy
          DO I=1,sNx
           tmpscal0     = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)*recip_rhoConst
           tmpscal1 = MAX( 0. _d 0, tmpscal0 - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW
          ENDDO
         ENDDO
        ENDIF

#endif /* SEAICE_GROWTH_LEGACY */

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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_hsnwbyneg = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_hsnwbyocnonsnw = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = r_QbyATM_cover(I,J) + r_QbyATM_open(I,J)
#ifndef SEAICE_GROWTH_LEGACY
C in principle a_QSWbyATM_cover should always be included here, however
C for backward compatibility it is left out of the LEGACY branch
     &         +   a_QSWbyATM_cover(I,J)
#endif /* SEAICE_GROWTH_LEGACY */
     &         - ( d_HEFFbyOCNonICE(I,J)
     &           + d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &           + d_HEFFbyNEG(I,J)
#ifdef EXF_ALLOW_SEAICE_RELAX
     &           + d_HEFFbyRLX(I,J)
#endif
     &           + d_HSNWbyNEG(I,J)*SNOW2ICE
     &           - convertPRECIP2HI *
     &             snowPrecip(i,j,bi,bj) * (ONE-AREApreTH(I,J))
     &           ) * maskC(I,J,kSurface,bi,bj)
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
CADJ STORE d_HEFFbyNEG = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HSNWbyNEG = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HSNWbyOCNonSNW = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HSNWbyATMonSNW = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE theta(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
cgf Unlike for evap and precip, the temperature of gained/lost
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
C ocean water going to ice/snow, in precip units
           tmpscal3=rhoConstFresh*maskC(I,J,kSurface,bi,bj)*(
     &       ( d_HSNWbyATMonSNW(I,J)*SNOW2ICE
     &       + d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &       + d_HEFFbyOCNonICE(I,J) + d_HEFFbyATMonOCN(I,J)
     &       + d_HEFFbyNEG(I,J) + d_HSNWbyNEG(I,J)*SNOW2ICE )
     &       * convertHI2PRECIP
     &       - snowPrecip(i,j,bi,bj) * (ONE-AREApreTH(I,J)) )
C factor in the heat content as done in external_forcing_surf.F
      IF ( (temp_EvPrRn.NE.UNSET_RL).AND.useRealFreshWaterFlux
     &     .AND.(nonlinFreeSurf.NE.0) ) THEN
             tmpscal1 = - tmpscal3*
     &         HeatCapacity_Cp * temp_EvPrRn
      ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL).AND.useRealFreshWaterFlux
     &         .AND.(nonlinFreeSurf.NE.0) ) THEN
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
      IF ( useRealFreshWaterFlux.AND.(nonlinFreeSurf.GT.0).AND.
     &   SEAICEheatConsFix ) QNET(I,J,bi,bj)=QNET(I,J,bi,bj)+tmpscal1
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
        CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIaaflux',0,1,3,bi,bj,myThid)
#endif
#endif /* ndef SEAICE_DISABLE_HEATCONSFIX */

C compute the net heat flux, incl. adv. by water, entering ocean+ice
C ===================================================================
         DO J=1,sNy
          DO I=1,sNx
cgf 1) SIatmQnt (analogous to qnet; excl. adv. by water exch.)
CML If I consider the atmosphere above the ice, the surface flux
CML which is relevant for the air temperature dT/dt Eq
CML accounts for sensible and radiation (with different treatment
CML according to wave-length) fluxes but not for "latent heat flux",
CML since it does not contribute to heating the air.
CML So this diagnostic is only good for heat budget calculations within
CML the ice-ocean system.
           SIatmQnt(I,J,bi,bj) = 
     &            maskC(I,J,kSurface,bi,bj)*convertHI2Q*(
#ifndef SEAICE_GROWTH_LEGACY
     &            a_QSWbyATM_cover(I,J) +
#endif /* SEAICE_GROWTH_LEGACY */
     &            a_QbyATM_cover(I,J) + a_QbyATM_open(I,J) )
cgf 2) SItflux (analogous to tflux; includes advection by water 
C             exchanged between atmosphere and ocean+ice)
C solid water going to atm, in precip units
           tmpscal1 = rhoConstFresh*maskC(I,J,kSurface,bi,bj)
     &       * convertHI2PRECIP * ( - d_HSNWbyRAIN(I,J)*SNOW2ICE
     &       + a_FWbySublim(I,J) - r_FWbySublim(I,J) )
C liquid water going to atm, in precip units
           tmpscal2=rhoConstFresh*maskC(I,J,kSurface,bi,bj)*
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
      IF ( (temp_EvPrRn.NE.UNSET_RL).AND.useRealFreshWaterFlux
     &     .AND.(nonlinFreeSurf.NE.0) ) THEN
           tmpscal2= - tmpscal2*
     &       ( ZERO + HeatCapacity_Cp * temp_EvPrRn )
      ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL).AND.useRealFreshWaterFlux
     &        .AND.(nonlinFreeSurf.NE.0) ) THEN
           tmpscal2= - tmpscal2*
     &       ( ZERO + HeatCapacity_Cp * theta(I,J,kSurface,bi,bj) )
      ELSEIF ( (temp_EvPrRn.NE.UNSET_RL) ) THEN
           tmpscal2= - tmpscal2*HeatCapacity_Cp*
     &       ( temp_EvPrRn - theta(I,J,kSurface,bi,bj) )
      ELSEIF ( (temp_EvPrRn.EQ.UNSET_RL) ) THEN
           tmpscal2= ZERO
      ENDIF
           SItflux(I,J,bi,bj)=
     &            SIatmQnt(I,J,bi,bj)-tmpscal1-tmpscal2     
          ENDDO
         ENDDO

C compute net fresh water flux leaving/entering
C the ocean, accounting for fresh/salt water stocks.
C ==================================================

#ifdef ALLOW_ATM_TEMP
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1= d_HSNWbyATMonSNW(I,J)*SNOW2ICE
     &             +d_HFRWbyRAIN(I,J)
     &             +d_HSNWbyOCNonSNW(I,J)*SNOW2ICE
     &             +d_HEFFbyOCNonICE(I,J)
     &             +d_HEFFbyATMonOCN(I,J)
     &             +d_HEFFbyNEG(I,J)
#ifdef EXF_ALLOW_SEAICE_RELAX
     &             +d_HEFFbyRLX(I,J)
#endif
     &             +d_HSNWbyNEG(I,J)*SNOW2ICE
C     If r_FWbySublim>0, then it is evaporated from ocean.
     &             +r_FWbySublim(I,J)
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &         + tmpscal1*convertHI2PRECIP
     &         )*rhoConstFresh
c and the flux leaving/entering the ocean+ice
           SIatmFW(I,J,bi,bj) = maskC(I,J,kSurface,bi,bj)*(
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
            frWtrAtm(I,J,bi,bj) = maskC(I,J,kSurface,bi,bj)*(
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
          frWtrAtm(I,J,bi,bj) = maskC(I,J,kSurface,bi,bj)*(
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

#endif /* ALLOW_ATM_TEMP */

#ifdef SEAICE_DEBUG
        CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
        CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
        CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
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
       IF ( balanceEmPmR ) THEN
        DO j=1,sNy
         DO i=1,sNx
          FWFsiTile(bi,bj) = 
     &      FWFsiTile(bi,bj) + SIatmFW(i,j,bi,bj)
     &      * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
c to translate global mean FWF adjustements (see below) we may need :
       FWF2HFsiTile(bi,bj) = 0. _d 0       
       IF ( balanceEmPmR.AND.(temp_EvPrRn.EQ.UNSET_RL) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          FWF2HFsiTile(bi,bj) = FWF2HFsiTile(bi,bj) +
     &      HeatCapacity_Cp * theta(I,J,kSurface,bi,bj)
     &      * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
       HFsiTile(bi,bj) = 0. _d 0
       IF ( balanceQnet ) THEN
        DO j=1,sNy
         DO i=1,sNx
          HFsiTile(bi,bj) = 
     &      HFsiTile(bi,bj) + SItflux(i,j,bi,bj)
     &      * rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
#endif

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
            DIAGarrayA(I,J) = maskC(I,J,kSurface,bi,bj)
     &        * d_HSNWbyRAIN(I,J)*SEAICE_rhoSnow*recip_deltaTtherm
            DIAGarrayB(I,J) =  AREA(I,J,bi,bj)-AREApreTH(I,J)
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIsnPrcp',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_SCALE_FILL(DIAGarrayB,
     &      tmpscal1,1,'SIdA    ',0,1,3,bi,bj,myThid)
#ifdef ALLOW_ATM_TEMP
         DO J=1,sNy
          DO I=1,sNx
           DIAGarrayB(I,J) = maskC(I,J,kSurface,bi,bj) *
     &       a_FWbySublim(I,J) * SEAICE_rhoIce * recip_deltaTtherm
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayB,
     &      'SIfwSubl',0,1,3,bi,bj,myThid)
C
         DO J=1,sNy
          DO I=1,sNx
C the actual Freshwater flux of sublimated ice, >0 decreases ice
           DIAGarrayA(I,J) = maskC(I,J,kSurface,bi,bj)
     &       * (a_FWbySublim(I,J)-r_FWbySublim(I,J))
     &       * SEAICE_rhoIce * recip_deltaTtherm
C the residual Freshwater flux of sublimated ice
           DIAGarrayC(I,J) = maskC(I,J,kSurface,bi,bj)
     &       * r_FWbySublim(I,J)
     &       * SEAICE_rhoIce * recip_deltaTtherm
C the latent heat flux
           tmpscal1= EVAP(I,J,bi,bj)*( ONE - AREApreTH(I,J) )
     &             + r_FWbySublim(I,J)*convertHI2PRECIP
           tmpscal2= ( a_FWbySublim(I,J)-r_FWbySublim(I,J) )
     &             * convertHI2PRECIP
           tmpscal3= SEAICE_lhEvap+SEAICE_lhFusion
           DIAGarrayB(I,J) = -maskC(I,J,kSurface,bi,bj)*rhoConstFresh
     &             * ( tmpscal1*SEAICE_lhEvap + tmpscal2*tmpscal3 )
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(DIAGarrayA,'SIacSubl',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayC,'SIrsSubl',0,1,3,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(DIAGarrayB,'SIhl    ',0,1,3,bi,bj,myThid)
#endif /* ALLOW_ATM_TEMP */

        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C close bi,bj loops
       ENDDO
      ENDDO


C ===================================================================
C =========PART 9: HF/FWF global integrals and balancing=============
C ===================================================================

#ifdef ALLOW_BALANCE_FLUXES

c 1)  global sums
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE FWFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE HFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE FWF2HFsiTile = comlev1, key=ikey_dynamics, kind=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
      FWFsiGlob=0. _d 0
      IF ( balanceEmPmR )
     &   CALL GLOBAL_SUM_TILE_RL( FWFsiTile, FWFsiGlob, myThid )        
      FWF2HFsiGlob=0. _d 0
      IF ( balanceEmPmR.AND.(temp_EvPrRn.EQ.UNSET_RL) ) THEN
         CALL GLOBAL_SUM_TILE_RL(FWF2HFsiTile, FWF2HFsiGlob, myThid)
      ELSEIF ( balanceEmPmR ) THEN
         FWF2HFsiGlob=HeatCapacity_Cp * temp_EvPrRn * globalArea
      ENDIF
      HFsiGlob=0. _d 0
      IF ( balanceQnet )
     &   CALL GLOBAL_SUM_TILE_RL( HFsiTile, HFsiGlob, myThid )

c 2) global means
c mean SIatmFW
      tmpscal0=FWFsiGlob / globalArea
c corresponding mean advection by atm to ocean+ice water exchange
c        (if mean SIatmFW was removed uniformely from ocean)
      tmpscal1=FWFsiGlob / globalArea * FWF2HFsiGlob / globalArea
c mean SItflux (before potential adjustement due to SIatmFW)
      tmpscal2=HFsiGlob / globalArea
c mean SItflux (after potential adjustement due to SIatmFW)
      IF ( balanceEmPmR ) tmpscal2=tmpscal2-tmpscal1

c 3) balancing adjustments
      IF ( balanceEmPmR ) THEN
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            empmr(i,j,bi,bj) = empmr(i,j,bi,bj) - tmpscal0
            SIatmFW(i,j,bi,bj) = SIatmFW(i,j,bi,bj) - tmpscal0
c           adjust SItflux consistently       
            IF (temp_EvPrRn.NE.UNSET_RL) THEN
            tmpscal1=
     &       ( ZERO + HeatCapacity_Cp * temp_EvPrRn )
            ELSE
            tmpscal1=
     &       ( ZERO + HeatCapacity_Cp * theta(I,J,kSurface,bi,bj) )
            ENDIF
            SItflux(i,j,bi,bj) = SItflux(i,j,bi,bj) - tmpscal0*tmpscal1
c           no qnet or tflux adjustement is needed
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgbuf,'(a,a,e24.17)') 'rm Global mean of ',
     &      'SIatmFW = ', tmpscal0
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid)
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
       WRITE(msgbuf,'(a,a,e24.17)') 'rm Global mean of ',
     &      'SItflux = ', tmpscal2
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid)
       _END_MASTER( myThid )
      ENDIF
      ENDIF
#endif /* */

#ifdef ALLOW_DIAGNOSTICS
c these diags need to be done outside of the bi,bj loop so that
c we may do potential global mean adjustement to them consistently.
         CALL DIAGNOSTICS_FILL(SItflux,
     &      'SItflux ',0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(SIatmQnt,
     &      'SIatmQnt',0,1,0,1,1,myThid)
c SIatmFW follows the same convention as empmr -- SIatmFW diag does not
         tmpscal1= - 1. _d 0
         CALL DIAGNOSTICS_SCALE_FILL(SIatmFW,
     &      tmpscal1,1,'SIatmFW ',0,1,0,1,1,myThid)
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
