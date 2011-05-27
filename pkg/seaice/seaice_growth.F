C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.122 2011/05/27 23:27:15 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid

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

CEOP
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface
C     constants
      _RL TBC, ICE2SNOW
      _RL QI, QS

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

C conversion factors to go from Q (W/m2) to HEFF (ice meters)
      _RL convertQ2HI, convertHI2Q
C conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      _RL convertPRECIP2HI, convertHI2PRECIP

C ICE/SNOW stocks tendencies associated with the various melt/freeze processes
      _RL d_AREAbyATM         (1:sNx,1:sNy)
#ifdef FENTY_AREA_EXPANSION_CONTRACTION
      _RL d_AREAbyOCN         (1:sNx,1:sNy)
      _RL d_AREAbyICE         (1:sNx,1:sNy)
#endif

c     The change of mean ice thickness due to out-of-bounds values following
c     sea ice dyhnamics
      _RL d_HEFFbyNEG         (1:sNx,1:sNy)

c     The change of mean ice thickness due to turbulent ocean-sea ice heat fluxes
      _RL d_HEFFbyOCNonICE    (1:sNx,1:sNy)

c     The sum of mean ice thickness increments due to atmospheric fluxes over the open water 
c     fraction and ice-covered fractions of the grid cell
      _RL d_HEFFbyATMonOCN    (1:sNx,1:sNy)
c     The change of mean ice thickness due to flooding by snow
      _RL d_HEFFbyFLOODING    (1:sNx,1:sNy)

c     The mean ice thickness increments due to atmospheric fluxes over the open water 
c     fraction and ice-covered fractions of the grid cell, respectively
      _RL d_HEFFbyATMonOCN_open(1:sNx,1:sNy)
      _RL d_HEFFbyATMonOCN_cover(1:sNx,1:sNy)

      _RL d_HSNWbyNEG         (1:sNx,1:sNy)
      _RL d_HSNWbyATMonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyOCNonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyRAIN        (1:sNx,1:sNy)

      _RL d_HFRWbyRAIN        (1:sNx,1:sNy)
C
C     a_FWbySublim :: fresh water flux implied by latent heat of 
C                     sublimation to atmosphere, same sign convention
C                     as EVAP (positive upward)
      _RL a_FWbySublim        (1:sNx,1:sNy)
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET 
      _RL d_HEFFbySublim      (1:sNx,1:sNy)
      _RL d_HSNWbySublim      (1:sNx,1:sNy)
      _RL rodt, rrodt
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */

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
#ifdef ALLOW_ATM_WIND
      _RL SPEED_SQ
#endif

C     pathological cases thresholds
      _RL heffTooThin, heffTooHeavy

C temporary variables available for the various computations
#ifdef SEAICE_GROWTH_LEGACY
      _RL tmpscal0
#endif
      _RL tmpscal1, tmpscal2, tmpscal3, tmpscal4
      _RL tmparr1             (1:sNx,1:sNy)

#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft
#endif /* ALLOW_SEAICE_FLOODING */

#ifdef SEAICE_VARIABLE_SALINITY
      _RL saltFluxAdjust      (1:sNx,1:sNy)
#endif

      INTEGER nDim
#ifdef SEAICE_MULTICATEGORY
      INTEGER ilockey
      PARAMETER ( nDim = MULTDIM )
      INTEGER it
      _RL pFac
      _RL heffActualP         (1:sNx,1:sNy)
      _RL a_QbyATMmult_cover  (1:sNx,1:sNy)
      _RL a_QSWbyATMmult_cover(1:sNx,1:sNy)
      _RL a_FWbySublimMult    (1:sNx,1:sNy)
#else
      PARAMETER ( nDim = 1 )
#endif /* SEAICE_MULTICATEGORY */

#ifdef MCPHEE_OCEAN_ICE_HEAT_FLUX
C     Factor by which we increase the upper ocean friction velocity (u*) when 
C     ice is absent in a grid cell  (dimensionless)
      _RL MixedLayerTurbulenceFactor

c     The Stanton number for the McPhee 
c     ocean-ice heat flux parameterization (dimensionless)
      _RL STANTON_NUMBER

c     A typical friction velocity beneath sea ice for the 
c     McPhee heat flux parameterization (m/s)
      _RL USTAR_BASE

      _RL surf_theta
#endif

#ifdef ALLOW_DIAGNOSTICS
c     Helper variables for diagnostics
      _RL DIAGarray     (1:sNx,1:sNy)
      _RL DIAGarrayA    (1:sNx,1:sNy)
      _RL DIAGarrayB    (1:sNx,1:sNy)
      _RL DIAGarrayC    (1:sNx,1:sNy)
      _RL DIAGarrayD    (1:sNx,1:sNy)
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

C     Cutoff for very thin ice
      heffTooThin=1. _d -5
C     Cutoff for iceload
      heffTooHeavy=dRf(kSurface) / 5. _d 0

C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze

C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SEAICE_rhoIce/SEAICE_rhoSnow

C     HEAT OF FUSION OF ICE (J/m^3)
      QI           = SEAICE_rhoIce*SEAICE_lhFusion
C     HEAT OF FUSION OF SNOW (J/m^3)
      QS           = SEAICE_rhoSnow*SEAICE_lhFusion
C
C     note that QI/QS=ICE2SNOW -- except it wasnt in old code

#ifdef MCPHEE_OCEAN_ICE_HEAT_FLUX
      STANTON_NUMBER = 0.0056 _d 0 
      USTAR_BASE = 0.0125 _d 0
#endif

C conversion factors to go from Q (W/m2) to HEFF (ice meters)
      convertQ2HI=SEAICE_deltaTtherm/QI
      convertHI2Q=1/convertQ2HI
C conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      convertPRECIP2HI=SEAICE_deltaTtherm*rhoConstFresh/SEAICE_rhoIce
      convertHI2PRECIP=1./convertPRECIP2HI

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

          d_AREAbyATM(I,J)           = 0.0 _d 0
#ifdef FENTY_AREA_EXPANSION_CONTRACTION
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
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET 
          d_HEFFbySublim(I,J)        = 0.0 _d 0
          d_HSNWbySublim(I,J)        = 0.0 _d 0
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
c
          d_HFRWbyRAIN(I,J)          = 0.0 _d 0

          tmparr1(I,J)               = 0.0 _d 0

#ifdef SEAICE_VARIABLE_SALINITY
          saltFluxAdjust(I,J)        = 0.0 _d 0
#endif
#ifdef SEAICE_MULTICATEGORY
          a_QbyATMmult_cover(I,J)    = 0.0 _d 0
          a_QSWbyATMmult_cover(I,J)  = 0.0 _d 0
          a_FWbySublimMult(I,J)      = 0.0 _d 0
#endif /* SEAICE_MULTICATEGORY */
         ENDDO
        ENDDO
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
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
         ENDDO
        ENDDO

#else /* SEAICE_GROWTH_LEGACY */

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no dependency through pathological cases treatment
      if ( SEAICEadjMODE.EQ.0 ) then
#endif
#endif

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
          if (HEFF(I,J,bi,bj).LE.heffTooThin) then
            tmpscal2=-HEFF(I,J,bi,bj)
            tmpscal3=-HSNOW(I,J,bi,bj)
            TICE(I,J,bi,bj)=celsius2K
#ifdef SEAICE_AGE
            IceAgeTr(i,j,bi,bj,2)=0. _d 0
#endif /* SEAICE_AGE */
          else
            tmpscal2=0. _d 0
            tmpscal3=0. _d 0
          endif
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

#ifdef ALLOW_PRECLUDE_INFINITESIMAL_AREA
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ((HEFF(i,j,bi,bj).GT.0).OR.(HSNOW(i,j,bi,bj).GT.0)) THEN
#ifdef SEAICE_AGE
           IF (AREA(I,J,bi,bj).LT.areaMin) THEN
            IceAgeTr(i,j,bi,bj,1)=IceAgeTr(i,j,bi,bj,1)
     &       + IceAgeTr(i,j,bi,bj,1)
     &       *(areaMin-AREA(I,J,bi,bj)) / areaMin
           ENDIF
#endif /* SEAICE_AGE */
           AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),areaMin)
          ENDIF
         ENDDO
        ENDDO
#endif

C 2.5) treat case of excessive ice cover, e.g., due to ridging:

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
#ifdef SEAICE_AGE
          IF (AREA(I,J,bi,bj).GT.areaMax) THEN
           IceAgeTr(i,j,bi,bj,1)=IceAgeTr(i,j,bi,bj,1)
     &      - IceAgeTr(i,j,bi,bj,1)
     &      *(AREA(I,J,bi,bj)-areaMax) / areaMax
          ENDIF
#endif /* SEAICE_AGE */
          AREA(I,J,bi,bj)=MIN(AREA(I,J,bi,bj),areaMax)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_MODIFY_GROWTH_ADJ
      endif
#endif
#endif

C 3) store regularized values of heff, hsnow, area at the onset of thermo.
        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J)=HEFF(I,J,bi,bj)
          HSNWpreTH(I,J)=HSNOW(I,J,bi,bj)
          AREApreTH(I,J)=AREA(I,J,bi,bj)
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
     &            HSALT(I,J,bi,bj) / SEAICE_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0 _d 0
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */

C 5) treat sea ice age pathological cases
C ...

#ifdef SEAICE_AGE

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE IceAgeTr(:,:,bi,bj,1)  = comlev1_bibj, 
CADJ &     key = iicekey,byte=isbyte
CADJ STORE IceAgeTr(:,:,bi,bj,2)  = comlev1_bibj, 
CADJ &     key = iicekey,byte=isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, 
CADJ &     key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF (HEFF(i,j,bi,bj).EQ.0.) THEN
            IceAgeTr(i,j,bi,bj,1)=0. _d 0
            IceAgeTr(i,j,bi,bj,2)=0. _d 0
          ENDIF
          IF (IceAgeTr(i,j,bi,bj,1).LT.0.) IceAgeTr(i,j,bi,bj,1)=0. _d 0
          IF (IceAgeTr(i,j,bi,bj,2).LT.0.) IceAgeTr(i,j,bi,bj,2)=0. _d 0
         ENDDO
        ENDDO
#endif /* SEAICE_AGE */

#endif /* SEAICE_GROWTH_LEGACY */

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency of air-sea fluxes to ice
      if ( SEAICEadjMODE.GE.1 ) then
        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J) = 0. _d 0
          HSNWpreTH(I,J) = 0. _d 0
          AREApreTH(I,J) = 0. _d 0
         ENDDO
        ENDDO
      endif
#endif
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
          tmpscal1         = MAX(areaMin,AREApreTH(I,J))
          hsnowActual(I,J) = HSNWpreTH(I,J)/tmpscal1
          tmpscal2         = HEFFpreTH(I,J)/tmpscal1
#ifdef SEAICE_GROWTH_LEGACY
          heffActual(I,J)  = MAX(tmpscal2,hiceMin)
#else
          heffActual(I,J)  = sqrt(tmpscal2*tmpscal2 + hiceMin*hiceMin)
c here we dont impose the area minimum          
          recip_heffActual(I,J)  = AREApreTH(I,J) / 
     &           sqrt(HEFFpreTH(I,J)*HEFFpreTH(I,J) + hiceMin*hiceMin)
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_SIMPLIFY_GROWTH_ADJ
      CALL ZERO_ADJ_1D( sNx*sNy, heffActual, myThid)
      CALL ZERO_ADJ_1D( sNx*sNy, hsnowActual, myThid)
      CALL ZERO_ADJ_1D( sNx*sNy, recip_heffActual, myThid)
#endif
#endif



C ===================================================================
C ================PART 2: determine heat fluxes/stocks===============
C ===================================================================

C determine available heat due to the atmosphere -- for open water
C ================================================================

C ocean surface/mixed layer temperature
        DO J=1,sNy
         DO I=1,sNx
          TMIX(I,J,bi,bj)=theta(I,J,kSurface,bi,bj)+celsius2K
         ENDDO
        ENDDO

C wind speed from exf
        DO J=1,sNy
         DO I=1,sNx
          UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE qnet(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
cCADJ STORE UG = comlev1_bibj, key = iicekey,byte=isbyte
cCADJ STORE TMIX(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        CALL SEAICE_BUDGET_OCEAN(
     I       UG,
     U       TMIX,
     O       a_QbyATM_open, a_QSWbyATM_open,
     I       bi, bj, myTime, myIter, myThid )

C determine available heat due to the atmosphere -- for ice covered water
C =======================================================================

#ifdef ALLOW_ATM_WIND
        IF (useRelativeWind) THEN
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
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tice   = comlev1, key = ikey_dynamics, byte = isbyte
CADJ STORE hsnowActual = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE heffActual = comlev1_bibj, key = iicekey, byte = isbyte
CADJ STORE UG = comlev1_bibj, key = iicekey, byte = isbyte
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices  = comlev1, key = ikey_dynamics, byte = isbyte
# endif /* SEAICE_MULTICATEGORY */
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Start loop over multi-categories, if SEAICE_MULTICATEGORY is undefined
C     nDim = 1, and there is only one loop iteration
#ifdef SEAICE_MULTICATEGORY
        DO IT=1,nDim
#ifdef ALLOW_AUTODIFF_TAMC
C     Why do we need this store directive when we have just stored
C     TICES before the loop?
         ilockey = (iicekey-1)*nDim + IT
CADJ STORE tices(:,:,it,bi,bj) = comlev1_multdim,
CADJ &                           key = ilockey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         pFac = (2.0 _d 0*real(IT)-1.0 _d 0)/nDim
         DO J=1,sNy
          DO I=1,sNx
           heffActualP(I,J)= heffActual(I,J)*pFac
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_SOLVE4TEMP(
     I        UG, heffActualP, hsnowActual,
     U        TICE,
     O        a_QbyATMmult_cover, a_QSWbyATMmult_cover,
     O        a_FWbySublimMult,
     I        bi, bj, myTime, myIter, myThid )
         DO J=1,sNy
          DO I=1,sNx
C     average over categories
           a_QbyATM_cover   (I,J) = a_QbyATM_cover(I,J)
     &          + a_QbyATMmult_cover(I,J)/nDim
           a_QSWbyATM_cover (I,J) = a_QSWbyATM_cover(I,J)
     &          + a_QSWbyATMmult_cover(I,J)/nDim
           a_FWbySublim     (I,J) = a_FWbySublim(I,J) 
     &          + a_FWbySublimMult(I,J)/nDim
           TICES(I,J,IT,bi,bj) = TICE(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#else
        CALL SEAICE_SOLVE4TEMP(
     I       UG, heffActual, hsnowActual,
     U       TICE,
     O       a_QbyATM_cover, a_QSWbyATM_cover, a_FWbySublim,
     I       bi, bj, myTime, myIter, myThid )
#endif /* SEAICE_MULTICATEGORY */
C--  End loop over multi-categories

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIatmQnt',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
CML If I consider the atmosphere above the ice, the surface flux 
CML which is relevant for the air temperature dT/dt Eq 
CML accounts for sensible and radiation (with different treatment
CML according to wave-length) fluxes but not for "latent heat flux",
CML since it does not contribute to heating the air.
CML So this diagnostic is only good for heat budget calculations within
CML the ice-ocean system.
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj) * (
     &             a_QbyATM_cover(I,J) * AREApreTH(I,J)
#ifndef SEAICE_GROWTH_LEGACY
     &           + a_QSWbyATM_cover(I,J) * AREApreTH(I,J)
#endif /* SEAICE_GROWTH_LEGACY */          
     &           + a_QbyATM_open(I,J) * ( ONE - AREApreTH(I,J) ) )
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmQnt',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIfwSubl',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj) *
     &           a_FWbySublim(I,J) * AREApreTH(I,J)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIfwSubl',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIatmFW ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj)*(
     &           PRECIP(I,J,bi,bj)
     &           - EVAP(I,J,bi,bj)
     &           *( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &           + RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &           )*rhoConstFresh
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET 
     &           - a_FWbySublim(I,J)*AREApreTH(I,J)
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmFW ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
C     Convert fresh water flux by sublimation to 'effective' ice meters. 
C     Negative sublimation is resublimation and will be added as snow.
          a_FWbySublim(I,J) = SEAICE_deltaTtherm/SEAICE_rhoIce
     &           * a_FWbySublim(I,J)*AREApreTH(I,J)
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIaQbATC',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(a_QbyATM_cover,
     &      'SIaQbATC',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIaQbATO',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(a_QbyATM_open,
     &      'SIaQbATO',0,1,3,bi,bj,myThid)
         ENDIF
       ENDIF
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through ice cover 
      if ( SEAICEadjMODE.GE.3 ) then
        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_cover(I,J)   = 0. _d 0
          r_QbyATM_cover(I,J)   = 0. _d 0
          a_QSWbyATM_cover(I,J) = 0. _d 0
         ENDDO
        ENDDO
      endif
#endif
#endif

C determine available heat due to the ice pack tying the
C underlying surface water temperature to freezing point
C ======================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif

        DO J=1,sNy
         DO I=1,sNx

#ifdef SEAICE_VARIABLE_FREEZING_POINT
           TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */

#ifdef MCPHEE_OCEAN_ICE_HEAT_FLUX

c     Bound the ocean temperature to be at or above the freezing point.
           surf_theta = max(theta(I,J,kSurface,bi,bj), TBC)
#ifdef GRADIENT_MIXED_LAYER_TURBULENCE_FACTOR
           MixedLayerTurbulenceFactor = 12.5 _d 0 - 
     &            11.5 _d 0 *AREApreTH(I,J)
#else 
c     If ice is present, MixedLayerTurbulenceFactor = 1.0, else 12.50
           IF (AREApreTH(I,J) .GT. 0. _d 0) THEN
               MixedLayerTurbulenceFactor = ONE
            ELSE
               MixedLayerTurbulenceFactor = 12.5 _d 0
           ENDIF
#endif /* GRADIENT_MIXED_LAYER_TURBULENCE_FACTOR */

c          The turbulent ocean-ice heat flux
           a_QbyOCN(I,J) = -STANTON_NUMBER * USTAR_BASE * rhoConst *
     &       HeatCapacity_Cp  *(surf_theta - TBC)*
     &       MixedLayerTurbulenceFactor*maskC(i,j,kSurface,bi,bj)

c          The turbulent ocean-ice heat flux converted to meters
c          of potential ice melt
           a_QbyOCN(I,J) = a_QbyOCN(I,J) * convertQ2HI

c          by design r_QbyOCN .LE. 0. so that initial ice growth cannot 
c          be triggered by this term, which Ian says is better for adjoint
#else 

           IF ( theta(I,J,kSurface,bi,bj) .GE. TBC ) THEN
             tmpscal1 = SEAICE_availHeatFrac
           ELSE
             tmpscal1 = SEAICE_availHeatFracFrz
           ENDIF
           tmpscal1 = tmpscal1 
     &              * dRf(kSurface) * maskC(i,j,kSurface,bi,bj)

           a_QbyOCN(i,j) = -tmpscal1 * (HeatCapacity_Cp*rhoConst/QI)
     &                   * (theta(I,J,kSurface,bi,bj)-TBC)

#endif /* MCPHEE_OCEAN_ICE_HEAT_FLUX */

#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA (I,J) = a_QbyOCN(I,J) 
#endif
          r_QbyOCN(i,j) = a_QbyOCN(i,j)

         ENDDO
        ENDDO
        
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIaQbOCN',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIaQbOCN',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif



#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_SIMPLIFY_GROWTH_ADJ
      CALL ZERO_ADJ_1D( sNx*sNy, r_QbyOCN, myThid)
#endif
#endif


C ===================================================================
C =========PART 3: determine effective thicknesses increments========
C ===================================================================

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
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = d_HEFFbyOCNonICE(i,j)
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIdHbOCN',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIdHbOCN',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

#ifdef SEAICE_GROWTH_LEGACY
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIyneg  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(d_HEFFbyOCNonICE,
     &      'SIyneg  ',0,1,1,bi,bj,myThid)
         ENDIF
        ENDIF
#endif
#endif

C compute snow melt tendency due to snow-atmosphere interaction
C ==================================================================

#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_FWbySublim     = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     First apply sublimation to snow
        rodt  = ICE2SNOW
        rrodt = 1./rodt
        DO J=1,sNy
         DO I=1,sNx
          IF ( a_FWbySublim(I,J) .LT. 0. _d 0 ) THEN
C     resublimate as snow
           d_HSNWbySublim(I,J) = -a_FWbySublim(I,J)*rodt
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbySublim(I,J)
           a_FWbySublim(I,J) = 0. _d 0
          ENDIF
C     sublimate snow first
          tmpscal1 = MIN(a_FWbySublim(I,J)*rodt,HSNOW(I,J,bi,bj))
          tmpscal2 = MAX(tmpscal1,0. _d 0)
          d_HSNWbySublim(I,J) = - tmpscal2
          HSNOW(I,J,bi,bj)    = HSNOW(I,J,bi,bj)  - tmpscal2
          a_FWbySublim(I,J)   = a_FWbySublim(I,J) - tmpscal2*rrodt
         ENDDO
        ENDDO
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          tmpscal1=MAX(r_QbyATM_cover(I,J)*ICE2SNOW,-HSNOW(I,J,bi,bj))
          tmpscal2=MIN(tmpscal1,0. _d 0)
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf no additional dependency through snow
          if ( SEAICEadjMODE.GE.2 ) tmpscal2 = 0. _d 0
#endif
          d_HSNWbyATMonSNW(I,J)= tmpscal2
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + tmpscal2
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J) - tmpscal2/ICE2SNOW
         ENDDO
        ENDDO

C compute ice thickness tendency due to the atmosphere
C ====================================================

#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_FWbySublim    = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     Apply sublimation to ice
        rodt  = 1. _d 0
        rrodt = 1./rodt
        DO J=1,sNy
         DO I=1,sNx
C     If anything is left, sublimate ice
          tmpscal1 = MIN(a_FWbySublim(I,J)*rodt,HEFF(I,J,bi,bj))
          tmpscal2 = MAX(tmpscal1,0. _d 0)
          d_HEFFbySublim(I,J) = - tmpscal2
          HEFF(I,J,bi,bj)     = HEFF(I,J,bi,bj)   - tmpscal2
          a_FWbySublim(I,J)   = a_FWbySublim(I,J) - tmpscal2*rrodt
         ENDDO
        ENDDO
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */

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
c         Limit ice growth by potential melt by ocean 
     &        AREApreTH(I,J) * r_QbyOCN(I,J))
#endif /* SEAICE_GROWTH_LEGACY */

          d_HEFFbyATMonOCN_cover(I,J)=tmpscal2
          d_HEFFbyATMonOCN(I,J)=d_HEFFbyATMonOCN(I,J)+tmpscal2
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)-tmpscal2
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + tmpscal2

#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = d_HEFFbyATMonOCN_cover(I,J)
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIdHbATC',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIdHbATC',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW DIAGNOSTICS */


C attribute precip to fresh water or snow stock,
C depending on atmospheric conditions.
C =================================================
#ifdef ALLOW_ATM_TEMP
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE a_QbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE PRECIP(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREApreTH = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIsnPrcp',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj) 
     &           * d_HSNWbyRAIN(I,J)*SEAICE_rhoSnow/SEAICE_deltaTtherm
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIsnPrcp',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
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
     &                               -d_HSNWbyOCNonSNW(I,J)/ICE2SNOW
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+d_HSNWbyOCNonSNW(I,J)
         ENDDO
        ENDDO
#endif /* SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING */
Cph)

C gain of new ice over open water
C ===============================
#ifndef SEAICE_GROWTH_LEGACY
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE r_QbyOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_QSWbyATM_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_QSWbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
#ifdef ALLOW_DIAGNOSTICS         
            DIAGarrayA(I,J) = ZERO
#endif

#ifdef SEAICE_DO_OPEN_WATER_GROWTH
         
c           Initial ice growth is triggered by open water 
c           heat flux overcoming potential melt by ocean
            tmpscal1=r_QbyATM_open(I,J)+r_QbyOCN(i,j) * 
     &            (1.0 _d 0 - AREApreTH(i,J))
c           Penetrative shortwave flux beyond first layer
c           that is therefore not available to ice growth/melt
            tmpscal2=SWFRACB * a_QSWbyATM_open(I,J)
#ifdef SEAICE_DO_OPEN_WATER_MELT
C           allow not only growth but also melt by open ocean heat flux
            tmpscal3=MAX(tmpscal1-tmpscal2, -HEFF(I,J,bi,bj))
#else
            tmpscal3=MAX(tmpscal1-tmpscal2, 0. _d 0)
#endif /* SEAICE_DO_OPEN_WATER_MELT */
          d_HEFFbyATMonOCN_open(I,J)=tmpscal3
          d_HEFFbyATMonOCN(I,J)=d_HEFFbyATMonOCN(I,J)+tmpscal3
          r_QbyATM_open(I,J)=r_QbyATM_open(I,J)-tmpscal3
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + tmpscal3
          
#endif /* SEAICE_DO_OPEN_WATER_GROWTH */

#ifdef ALLOW_DIAGNOSTICS
            DIAGarrayA(I,J) = maskC(I,J,kSurface,bi,bj)
     &           * d_HEFFbyATMonOCN_open(I,J)
#endif
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIdHbATO',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIdHbATO',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW DIAGNOSTICS */
     
#endif /* SEAICE_GROWTH_LEGACY */

C convert snow to ice if submerged.
C =================================

#ifndef SEAICE_GROWTH_LEGACY
C note: in legacy, this process is done at the end
#ifdef ALLOW_SEAICE_FLOODING
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        IF ( SEAICEuseFlooding ) THEN
         DO J=1,sNy
          DO I=1,sNx
           hDraft = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)/rhoConst
           tmpscal1 = MAX( 0. _d 0, hDraft - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */
#endif /* SEAICE_GROWTH_LEGACY */


C ===================================================================
C ==========PART 4: determine ice cover fraction increments=========-
C ===================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE a_QbyATM_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE heffActual = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREApreTH = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HEFF(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE HSNOW(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE AREA(:,:,bi,bj) = comlev1_bibj,key=iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx

C         compute ice melt due to ATM (and OCN) heat stocks
#ifdef FENTY_AREA_EXPANSION_CONTRACTION

#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = ZERO
          DIAGarrayB(I,J) = ZERO
          DIAGarrayC(I,J) = ZERO
          DIAGarray(I,J) = ZERO
#endif

c         the various thickness tendency terms
          tmpscal1 = d_HEFFbyATMOnOCN_open(I,J)
          tmpscal2 = d_HEFFbyATMOnOCN_cover(I,J)
          tmpscal3 = d_HEFFbyOCNonICE(I,J)

c         Part 1: Treat expansion/contraction of ice cover in open-water areas

C         All new ice cover is created from divergent air-sea heat fluxes.  Divergent 
c         air-sea heat fluxes must exceed the potential convergent ocean-ice heat fluxes
c         for ice to form.  
          IF (tmpscal1 .GT. ZERO) then
            IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
             d_AREAbyATM(I,J)=tmpscal1/HO_south
            ELSE
             d_AREAbyATM(I,J)=tmpscal1/HO
            ENDIF

c         If there are convergent air-sea heat fluxes and convergent air-sea 
c         heat fluxes are permitted to melt ice, tmpscal1 < 0.
          ELSEIF (AREApreTH(I,J) .GT. ZERO) THEN
             d_AREAbyATM(i,J) = HALF*tmpscal1*recip_heffActual(I,J)
          ENDIF

c         Part 2: Reduce ice concentration from ice thinning from above
 
c         Ice concentrations are reduced whenever existing ice thins from surface 
c         heat flux convergence.
          if (tmpscal2 .LE. ZERO) then
             d_AREAbyICE(I,J) = HALF * tmpscal2 *recip_heffActual(I,J)
          endif

c         Part 3: Reduce ice concentration from ice thinning from below

c         Sensible heat transfer from the ocean to the sea ice thins it and
c         reduces concentrations
          if (tmpscal3 .LE.ZERO) then
             d_AREAbyOCN(I,J) = HALF * tmpscal3 *recip_heffActual(I,J)
          endif

#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = d_AREAbyATM(I,J) 
          DIAGarrayB(I,J) = d_AREAbyICE(I,J) 
          DIAGarrayC(I,J) = d_AREAbyOCN(I,J)
          DIAGarray(I,J) = d_AREAbyICE(I,J) + d_AREAbyATM(I,J) 
     &                     + d_AREAbyOCN(I,J)
#endif

#else /* FENTY_AREA_EXPANSION_CONTRACTION */

#ifdef SEAICE_GROWTH_LEGACY

C compute heff after ice melt by ocn:
          tmpscal0=HEFF(I,J,bi,bj)
     &            - d_HEFFbyATMonOCN(I,J) - d_HEFFbyFLOODING(I,J)
C compute available heat left after snow melt by atm:
          tmpscal1= a_QbyATM_open(I,J)+a_QbyATM_cover(I,J)
     &            - d_HSNWbyATMonSNW(I,J)/ICE2SNOW
C (cannot melt more than all the ice)
          tmpscal2 = MAX(-tmpscal0,tmpscal1)
          tmpscal3 = MIN(ZERO,tmpscal2)
#ifdef ALLOW_DIAGNOSTICS
          DIAGarray(I,J) = tmpscal2
#endif
C gain of new ice over open water
          tmpscal4 = MAX(ZERO,a_QbyATM_open(I,J))

#else /* SEAICE_GROWTH_LEGACY */

# ifdef SEAICE_OCN_MELT_ACT_ON_AREA
C ice cover reduction by joint OCN+ATM melt
          tmpscal3 = MIN( 0. _d 0 ,
     &              d_HEFFbyATMonOCN(I,J)+d_HEFFbyOCNonICE(I,J) )
# else
C ice cover reduction by ATM melt only -- as in legacy code
          tmpscal3 = MIN( 0. _d 0 , d_HEFFbyATMonOCN(I,J) )
# endif
C gain of new ice over open water

# ifdef SEAICE_DO_OPEN_WATER_GROWTH
C the one effectively used to increment HEFF
          tmpscal4 = d_HEFFbyATMonOCN_open(I,J)
# else
C the virtual one -- as in legcy code
          tmpscal4 = MAX(ZERO,a_QbyATM_open(I,J))
# endif
#endif /* SEAICE_GROWTH_LEGACY */

C compute cover fraction tendency
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           d_AREAbyATM(I,J)=tmpscal4/HO_south
          ELSE
           d_AREAbyATM(I,J)=tmpscal4/HO
          ENDIF
          d_AREAbyATM(I,J)=d_AREAbyATM(I,J)
#ifdef SEAICE_GROWTH_LEGACY
     &         +HALF*tmpscal3*AREApreTH(I,J)
     &         /(tmpscal0+.00001 _d 0)
#else
     &         +HALF*tmpscal3*recip_heffActual(I,J)
#endif

#endif /* FENTY_AREA_EXPANSION_CONTRACTION */

C apply tendency
          IF ( (HEFF(i,j,bi,bj).GT.0. _d 0).OR.
     &        (HSNOW(i,j,bi,bj).GT.0. _d 0) ) THEN
           AREA(I,J,bi,bj)=max(0. _d 0 , min( 1. _d 0,
     &                     AREA(I,J,bi,bj)+d_AREAbyATM(I,J) 
#ifdef FENTY_AREA_EXPANSION_CONTRACTION
     &                   + d_AREAbyOCN(I,J) + d_AREAbyICE(I,J)
#endif
     &                                       ))
          ELSE
           AREA(I,J,bi,bj)=0. _d 0
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIdA    ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarray,
     &      'SIdA    ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIdAbATO',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayA,
     &      'SIdAbATO',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIdAbATC',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayB,
     &      'SIdAbATC',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIdAbOCN',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarrayC,
     &      'SIdAbOCN',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_MODIFY_GROWTH_ADJ
Cgf 'bulk' linearization of area=f(HEFF) 
      if ( SEAICEadjMODE.GE.1 ) then
        DO J=1,sNy
         DO I=1,sNx
C            AREA(I,J,bi,bj) = 0.1 _d 0 * HEFF(I,J,bi,bj)
            AREA(I,J,bi,bj) = AREApreTH(I,J) + 0.1 _d 0 *
     &               ( HEFF(I,J,bi,bj) - HEFFpreTH(I,J) )
         ENDDO
        ENDDO
      endif
#endif
#endif

#ifdef SEAICE_GROWTH_LEGACY
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIfice  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIfice  ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif
#endif /* SEAICE_GROWTH_LEGACY */


C ===================================================================
C =============PART 5: determine ice salinity increments=============
C ===================================================================

#ifndef SEAICE_VARIABLE_SALINITY
# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef ALLOW_SALT_PLUME
CADJ STORE d_HEFFbyNEG = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyOCNonICE = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_open = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyATMonOCN_cover = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE d_HEFFbyFLOODING = comlev1_bibj,key=iicekey,byte=isbyte
CADJ STORE salt(:,:,kSurface,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#  endif /* ALLOW_SALT_PLUME */
# endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
Cgf note: flooding does count negatively
          tmpscal1 = d_HEFFbyNEG(I,J) + d_HEFFbyOCNonICE(I,J) + 
     &               d_HEFFbyATMonOCN(I,J) - d_HEFFbyFLOODING(I,J)
          tmpscal2 = tmpscal1 * SIsal0 * HEFFM(I,J,bi,bj)
     &            /SEAICE_deltaTtherm * SEAICE_rhoIce
          saltFlux(I,J,bi,bj) = tmpscal2
#ifdef ALLOW_SALT_PLUME
          tmpscal3 = tmpscal1*salt(I,j,kSurface,bi,bj)*HEFFM(I,J,bi,bj)
     &            /SEAICE_deltaTtherm * SEAICE_rhoIce
          saltPlumeFlux(I,J,bi,bj) = MAX( tmpscal3-tmpscal2 , 0. _d 0)
#endif /* ALLOW_SALT_PLUME */
         ENDDO
        ENDDO
#endif

#ifdef ALLOW_ATM_TEMP
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
     &            HSALT(I,J,bi,bj) / SEAICE_deltaTtherm
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

C recompute HEFF before thermodyncamic updates (which is not AREApreTH in legacy code)
         tmpscal2=HEFF(I,J,bi,bj)-tmpscal1-d_HEFFbyFLOODING(I,J)
C tmpscal1 > 0 : m of sea ice that is created
          IF ( tmpscal1 .GE. 0.0 ) THEN
             saltFlux(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm
     &            *SIsalFRAC*salt(I,j,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
#ifdef ALLOW_SALT_PLUME
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm
     &            *(1-SIsalFRAC)*salt(I,j,kSurface,bi,bj)
     &            *tmpscal1*SEAICE_rhoIce
C if SaltPlumeSouthernOcean=.FALSE. turn off salt plume in Southern Ocean
             IF ( .NOT. SaltPlumeSouthernOcean ) THEN
              IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 )
     &             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
             ENDIF
#endif /* ALLOW_SALT_PLUME */

C tmpscal1 < 0 : m of sea ice that is melted
          ELSE
             saltFlux(I,J,bi,bj) =
     &         HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm
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
     &            HEFFM(I,J,bi,bj) * HSALT(I,J,bi,bj) /
     &            SEAICE_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0 _d 0
#ifdef ALLOW_SALT_PLUME
             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
#endif /* ALLOW_SALT_PLUME */
          ENDIF
#endif /* SEAICE_GROWTH_LEGACY */
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */
#endif /* ALLOW_ATM_TEMP */


C =======================================================================
C =====LEGACY PART 5.5: treat pathological cases, then do flooding ======
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
C NOW SET AREA(I,J,bi,bj)=0 WHERE NO ICE IS
          AREA(I,J,bi,bj)=MIN(AREA(I,J,bi,bj)
     &                         ,HEFF(I,J,bi,bj)/.0001 _d 0)
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

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIthdgrh',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            tmparr1(I,J) = (HEFF(I,J,bi,bj)-HEFFpreTH(I,J))
     &           /SEAICE_deltaTtherm
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(tmparr1,'SIthdgrh',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C convert snow to ice if submerged.
C =================================

#ifdef ALLOW_SEAICE_FLOODING
        IF ( SEAICEuseFlooding ) THEN
         DO J=1,sNy
          DO I=1,sNx
           hDraft     = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)/rhoConst
           tmpscal1 = MAX( 0. _d 0, hDraft - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIsnwice',myThid) ) THEN
           DO J=1,sNy
            DO I=1,sNx
             tmparr1(I,J) = d_HEFFbyFLOODING(I,J)/SEAICE_deltaTtherm
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(tmparr1,'SIsnwice',0,1,3,bi,bj,myThid)
          ENDIF
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */

#endif /* SEAICE_GROWTH_LEGACY */


C ===================================================================
C ===============PART 6: determine ice age increments================
C ===================================================================

#ifdef SEAICE_AGE
C     Sources and sinks for sea ice age:
C     assume that a) freezing: new ice fraction forms with zero age
C                 b) melting: ice fraction vanishes with current age
        DO J=1,sNy
         DO I=1,sNx
C--   increase age as time passes
                IceAgeTr(i,j,bi,bj,1)=IceAgeTr(i,j,bi,bj,1)
     &            +SEAICE_deltaTtherm*AREApreTH(i,j)
                IceAgeTr(i,j,bi,bj,2)=IceAgeTr(i,j,bi,bj,2)
     &            +SEAICE_deltaTtherm*HEFFpreTH(i,j)
C--   account for ice melt
               IF (AREApreTH(i,j).GT.AREA(i,j,bi,bj)) THEN
                    IceAgeTr(i,j,bi,bj,1)=IceAgeTr(i,j,bi,bj,1)
     &              *AREA(i,j,bi,bj)/AREApreTH(i,j)
               ENDIF
               IF (HEFFpreTH(i,j).GT.HEFF(i,j,bi,bj)) THEN
                    IceAgeTr(i,j,bi,bj,2)=IceAgeTr(i,j,bi,bj,2)
     &              *HEFF(i,j,bi,bj)/HEFFpreTH(i,j)
               ENDIF
         ENDDO
        ENDDO

C #ifdef ALLOW_DIAGNOSTICS
C        IF ( useDiagnostics ) THEN
C         IF ( DIAGNOSTICS_IS_ON('SIage03 ',myThid) ) THEN
C          DO J=1,sNy
C           DO I=1,sNx
C            DIAGarray(I,J) = IceAgeTr(i,j,bi,bj,1)/AREA(i,j,bi,bj) 
C           ENDDO
C          ENDDO
C          CALL DIAGNOSTICS_FILL(DIAGarray,'SIage03 ',0,1,3,bi,bj,myThid)
C         ENDIF
C         IF ( DIAGNOSTICS_IS_ON('SIage04 ',myThid) ) THEN
C          DO J=1,sNy
C           DO I=1,sNx
C            DIAGarray(I,J) = IceAgeTr(i,j,bi,bj,2)/HEFF(i,j,bi,bj) 
C           ENDDO
C          ENDDO
C          CALL DIAGNOSTICS_FILL(DIAGarray,'SIage04 ',0,1,3,bi,bj,myThid)
C         ENDIF
C        ENDIF
C #endif /* ALLOW_DIAGNOSTICS */

#endif /* SEAICE_AGE */


C ===================================================================
C ==============PART 7: determine ocean model forcing================
C ===================================================================

C compute net heat flux leaving/entering the ocean,
C accounting for the part used in melt/freeze processes
C =====================================================

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = r_QbyATM_cover(I,J) + r_QbyATM_open(I,J)
#ifndef SEAICE_GROWTH_LEGACY 
C in principle a_QSWbyATM_cover should always be included here, however    
C for backward compatibility it is left out of the LEGACY branch
     &         +   a_QSWbyATM_cover(I,J)
#endif /* SEAICE_GROWTH_LEGACY */          
     &         - ( d_HEFFbyOCNonICE(I,J) +
     &             d_HSNWbyOCNonSNW(I,J)/ICE2SNOW +
     &             d_HEFFbyNEG(I,J) +
     &             d_HSNWbyNEG(I,J)/ICE2SNOW )
     &         * maskC(I,J,kSurface,bi,bj)
          QSW(I,J,bi,bj)  = a_QSWbyATM_cover(I,J) + a_QSWbyATM_open(I,J)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIqneto ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = r_QbyATM_open(I,J) * convertHI2Q
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneto ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIqneti ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = r_QbyATM_cover(I,J) * convertHI2Q
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneti ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C switch heat fluxes from 'effective' ice meters to W/m2
C ======================================================

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)*convertHI2Q
          QSW(I,J,bi,bj)  = QSW(I,J,bi,bj)*convertHI2Q
         ENDDO
        ENDDO

C compute net fresh water flux leaving/entering
C the ocean, accounting for fresh/salt water stocks.
C ==================================================

#ifdef ALLOW_ATM_TEMP
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1= d_HSNWbyATMonSNW(I,J)/ICE2SNOW
     &             +d_HFRWbyRAIN(I,J)
     &             +d_HSNWbyOCNonSNW(I,J)/ICE2SNOW
     &             +d_HEFFbyOCNonICE(I,J)
     &             +d_HEFFbyATMonOCN(I,J)
     &             +d_HEFFbyNEG(I,J)
     &             +d_HSNWbyNEG(I,J)/ICE2SNOW
#ifdef SEAICE_ADD_SUBLIMATION_TO_FWBUDGET
     &             +a_FWbySublim(I,J)
#endif /* SEAICE_ADD_SUBLIMATION_TO_FWBUDGET */
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif /* ALLOW_RUNOFF */
     &         + tmpscal1*convertHI2PRECIP
     &         )*rhoConstFresh
         ENDDO
        ENDDO

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
        DO J=1,sNy
         DO I=1,sNx
          frWtrAtm(I,J,bi,bj) = maskC(I,J,kSurface,bi,bj)*(
     &         PRECIP(I,J,bi,bj)
     &         - EVAP(I,J,bi,bj)
     &         *( ONE - AREApreTH(I,J) )
     &         + RUNOFF(I,J,bi,bj)
     &         )*rhoConstFresh
         ENDDO
        ENDDO
#endif
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
           tmpscal2 = min(tmpscal1,heffTooHeavy*rhoConst)
#else
           tmpscal2 = HEFF(I,J,bi,bj)*SEAICE_rhoIce
     &              + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
#endif
           sIceLoad(i,j,bi,bj) = tmpscal2
          ENDDO
         ENDDO
        ENDIF

C close bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
