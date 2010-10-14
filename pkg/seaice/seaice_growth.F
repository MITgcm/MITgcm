#define SEAICE_GROWTH_LEGACY
C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.87 2010/10/14 16:29:44 gforget Exp $
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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface
C     constants
      _RL TBC, ICE2SNOW
      _RL QI, QS, Lfusion

C note: for all heat stocks, the sign convention is positive towards the atmosphere
C
C     a_QbyATM_cover :: available heat (in W/m^2) due to the interaction of  
C             the atmosphere and the ocean surface - for ice covered water
C     a_QbyATM_open :: same but for open water
C     r_QbyATM_cover :: residual of a_QbyATM_cover after freezing/melting processes
      _RL a_QbyATM_cover      (1:sNx,1:sNy)
      _RL a_QbyATM_open       (1:sNx,1:sNy)
      _RL r_QbyATM_cover      (1:sNx,1:sNy)
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

c conversion factors to go from Q (W/m2) to HEFF (ice meters)
      _RL convertQ2HI, convertHI2Q
c conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      _RL convertPRECIP2HI, convertHI2PRECIP

c FRWfromSNW :: flag that states whether the atmostpheric conditions are 
c     prone to generate SNOW (FRWfromSNW=1) or fresh water (FRWfromSNW=2)
      integer FRWfromSNW      (1:sNx,1:sNy)

c ICE/SNOW stocks tendencies associated with the various melt/freeze processes
      _RL d_AREAbyATM         (1:sNx,1:sNy)
c
      _RL d_HEFFbyNEG         (1:sNx,1:sNy)
      _RL d_HEFFbyOCNonICE    (1:sNx,1:sNy)
      _RL d_HEFFbyATMonOCN    (1:sNx,1:sNy)
      _RL d_HEFFbyFLOODING    (1:sNx,1:sNy)
c
      _RL d_HSNWbyNEG         (1:sNx,1:sNy)
      _RL d_HSNWbyATMonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyOCNonSNW    (1:sNx,1:sNy)
      _RL d_HSNWbyRAIN        (1:sNx,1:sNy)
c
      _RL d_HFRWbyRAIN        (1:sNx,1:sNy)

C     actual ice thickness with upper and lower limit
      _RL heffActual          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hsnowActual         (1:sNx,1:sNy)

C     AREA_PRE :: hold sea-ice fraction field before any seaice-thermo update
      _RL AREApreTH           (1:sNx,1:sNy)
      _RL HEFFpreTH           (1:sNx,1:sNy)
      _RL HSNWpreTH           (1:sNx,1:sNy)

C     wind speed
      _RL UG                  (1:sNx,1:sNy)
      _RL SPEED_SQ
C temporary variables used to compute/regularize "actual" thicknesses
      _RL areaMin,hiceMin

c temporary variables available for the various computations
      _RL tmpscal1, tmpscal2, tmpscal3, tmpscal4
      _RL tmparr1             (1:sNx,1:sNy)


#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft
#endif /* ALLOW_SEAICE_FLOODING */
      
#ifdef SEAICE_SALINITY
      _RL saltFluxAdjust      (1:sNx,1:sNy)
#endif

#ifdef SEAICE_MULTICATEGORY
      INTEGER it
      INTEGER ilockey
      _RL RK
      _RL heffActualP         (1:sNx,1:sNy)
      _RL a_QbyATMmult_cover  (1:sNx,1:sNy)
      _RL a_QSWbyATMmult_cover(1:sNx,1:sNy)
#endif

#ifdef ALLOW_DIAGNOSTICS
      _RL DIAGarray     (1:sNx,1:sNy)
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     MINIMUM AREA USED TO REGULARIZE SEAICE_SOLVE4TEMP COMPUTATION
      areaMin=A22
C     The default of A22 = 0.15 is a common threshold for defining
C     the ice edge. This ice concentration usually does not occur
C     due to thermodynamics but due to advection.
C     MINIMUM THICKNESS USED TO REGULARIZE SEAICE_SOLVE4TEMP COMPUTATION
      hiceMin=0.05 _d +00

C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze

C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SEAICE_rhoIce/SEAICE_rhoSnow

C     FRESH WATER LATENT HEAT OF FUSION
      Lfusion       = 3.34 _d +05
C     HEAT OF FUSION OF ICE (J/m^3)
      QI           = SEAICE_rhoIce*Lfusion
C     HEAT OF FUSION OF SNOW (J/m^3)
      QS           = SEAICE_rhoSnow*Lfusion
C
C     note that QI/QS=ICE2SNOW -- except it wasnt in old code



c conversion factors to go from Q (W/m2) to HEFF (ice meters)
      convertQ2HI=SEAICE_deltaTtherm/QI
      convertHI2Q=1/convertQ2HI
c conversion factors to go from precip (m/s) unit to HEFF (ice meters)
      convertPRECIP2HI=SEAICE_deltaTtherm*rhoConstFresh/SEAICE_rhoIce
      convertHI2PRECIP=1./convertPRECIP2HI

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
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


C array initializations
C =====================

        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM_cover (I,J)      = 0.0 _d 0
          a_QbyATM_open(I,J)      = 0.0 _d 0
          r_QbyATM_cover (I,J)      = 0.0 _d 0
c
          a_QSWbyATM_open (I,J)      = 0.0 _d 0
          a_QSWbyATM_cover (I,J)      = 0.0 _d 0
c
          a_QbyOCN (I,J)      = 0.0 _d 0
          r_QbyOCN (I,J)      = 0.0 _d 0
c
          FRWfromSNW (I,J)    = 0
c
          d_AREAbyATM(I,J)      = 0.0 _d 0
c
          d_HEFFbyOCNonICE(I,J)      = 0.0 _d 0
          d_HEFFbyATMonOCN(I,J)      = 0.0 _d 0
          d_HEFFbyFLOODING(I,J)      = 0.0 _d 0
c
          d_HSNWbyATMonSNW(I,J)      = 0.0 _d 0
          d_HSNWbyOCNonSNW(I,J)      = 0.0 _d 0
          d_HSNWbyRAIN(I,J)      = 0.0 _d 0
c
          d_HFRWbyRAIN(I,J)      = 0.0 _d 0
c
          tmparr1(I,J)      = 0.0 _d 0
c
#ifdef SEAICE_SALINITY
          saltFluxAdjust(I,J) = 0.0 _d 0
#endif
#ifdef SEAICE_MULTICATEGORY
          a_QbyATMmult_cover(I,J)      = 0.0 _d 0
          a_QSWbyATMmult_cover(I,J)      = 0.0 _d 0
#endif
         ENDDO
        ENDDO
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
        DO J=1-oLy,sNy+oLy
         DO I=1-oLx,sNx+oLx
          frWtrAtm(I,J,bi,bj)   = 0.0 _d 0
         ENDDO
        ENDDO
#endif


c ===================================================================
c =============PART 1: regularization, after advdiff, etc.===========
c ===================================================================

#ifndef SEAICE_GROWTH_LEGACY

c 1) treat the case of negative values:
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          d_HEFFbyNEG(I,J)=MAX(-HEFF(I,J,bi,bj),0._d 0)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj)+d_HEFFbyNEG(I,J)
          d_HSNWbyNEG(I,J)=MAX(-HSNOW(I,J,bi,bj),0._d 0)
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)+d_HSNWbyNEG(I,J)
          AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),0. _d 0)
         ENDDO
        ENDDO

c 2) treat the case of very small area:
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ((HEFF(i,j,bi,bj).GT.0).OR.(HSNOW(i,j,bi,bj).GT.0)) 
     &     AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),areaMin)
         ENDDO
        ENDDO

c 3) store regularized values of heff, hsnow, area at the onset of thermo.
        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J)=HEFF(I,J,bi,bj)
          HSNWpreTH(I,J)=HSNOW(I,J,bi,bj)
          AREApreTH(I,J)=AREA(I,J,bi,bj)
         ENDDO
        ENDDO

c 4) treat sea ice salinity pathological cases
#ifdef SEAICE_SALINITY
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
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
#endif /* SEAICE_SALINITY */

c 5) treat sea ice age pathological cases
c ...
        
#else
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          HEFFpreTH(I,J)=HEFFNM1(I,J,bi,bj)
          HSNWpreTH(I,J)=HSNOW(I,J,bi,bj)
          AREApreTH(I,J)=AREANM1(I,J,bi,bj)
          d_HEFFbyNEG(I,J)=0. _d 0
          d_HSNWbyNEG(I,J)=0. _d 0
         ENDDO
        ENDDO
#endif /* SEAICE_GROWTH_LEGACY */


c 4) COMPUTE ACTUAL ICE/SNOW THICKNESS; PUT MINIMUM ON HEFFACTUAL
c    TO REGULARIZE SEAICE_SOLVE4TEMP AND d_AREA COMPUTATIONS
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1         = MAX(areaMin,AREApreTH(I,J))
          hsnowActual(I,J) = HSNWpreTH(I,J)/tmpscal1
          tmpscal2         = HEFFpreTH(I,J)/tmpscal1
          heffActual(I,J)  = MAX(tmpscal2,hiceMin)
C     Capping the actual ice thickness effectively enforces a
C     minimum of heat flux through the ice and helps getting rid of
C     very thick ice.
cdm actually, this does exactly the opposite, i.e., ice is thicker
cdm when heffActual is capped, so I am commenting out
cdm          heffActual(I,J)    = MIN(heffActual(I,J),9.0 _d +00)
         ENDDO
        ENDDO



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

        CALL SEAICE_BUDGET_OCEAN(
     I       UG,
     U       TMIX,
     O       a_QbyATM_open, a_QSWbyATM_open,
     I       bi, bj, myTime, myIter, myThid )


C determine available heat due to the atmosphere -- for ice covered water
C =======================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tice   = comlev1, key = ikey_dynamics, byte = isbyte
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices  = comlev1, key = ikey_dynamics, byte = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

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

#ifdef SEAICE_MULTICATEGORY
C--  Start loop over muli-categories
        DO IT=1,MULTDIM
#ifdef ALLOW_AUTODIFF_TAMC
         ilockey = (iicekey-1)*MULTDIM + IT
CADJ STORE tices(:,:,it,bi,bj) = comlev1_multdim,
CADJ &                           key = ilockey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         RK=REAL(IT)
         DO J=1,sNy
          DO I=1,sNx
           heffActualP(I,J)=
     &        (heffActual(I,J)/MULTDIM)*((2.0 _d 0*RK)-1.0 _d 0)
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_SOLVE4TEMP(
     I        UG, heffActualP, hsnowActual,
     U        TICE,
     O        a_QbyATMmult_cover, a_QSWbyATMmult_cover,
     I        bi, bj, myTime, myIter, myThid )
         DO J=1,sNy
          DO I=1,sNx
C     average over categories
           a_QbyATM_cover (I,J) = 
     &       a_QbyATM_cover(I,J) + a_QbyATMmult_cover(I,J)/MULTDIM
           a_QSWbyATM_cover (I,J) = 
     &       a_QSWbyATM_cover(I,J) + a_QSWbyATMmult_cover(I,J)/MULTDIM
           TICES(I,J,IT,bi,bj) = TICE(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C--  End loop over multi-categories
#else  /* SEAICE_MULTICATEGORY */
        CALL SEAICE_SOLVE4TEMP(
     I       UG, heffActual, hsnowActual,
     U       TICE,
     O       a_QbyATM_cover, a_QSWbyATM_cover,
     I       bi, bj, myTime, myIter, myThid )
#endif /* SEAICE_MULTICATEGORY */

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIatmQnt',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj) * (
     &           a_QbyATM_cover(I,J) * AREApreTH(I,J) +
     &           a_QbyATM_open(I,J) * ( ONE - AREApreTH(I,J) ) )
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmQnt',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

c switch heat fluxes from W/m2 to 'effective' ice meters
        DO J=1,sNy
         DO I=1,sNx
           a_QbyATM_cover(I,J) = a_QbyATM_cover(I,J)  
     &         * convertQ2HI * AREApreTH(I,J)
           a_QSWbyATM_cover(I,J) = a_QSWbyATM_cover(I,J) 
     &         * convertQ2HI * AREApreTH(I,J)
           a_QbyATM_open(I,J) = a_QbyATM_open(I,J) 
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
           a_QSWbyATM_open(I,J) = a_QSWbyATM_open(I,J) 
     &         * convertQ2HI * ( ONE - AREApreTH(I,J) )
c and initialize r_QbyATM_cover
           r_QbyATM_cover(I,J)=a_QbyATM_cover(I,J)
         ENDDO
        ENDDO

C determine whether the atmostpheric conditions are prone 
C to generate SNOW (FRWfromSNW=1) or fresh water (FRWfromSNW=2)
C ==============================================================

        DO J=1,sNy
         DO I=1,sNx
          IF (a_QbyATM_cover(I,J).LT.ZERO.AND.
     &        AREApreTH(I,J).GT.ZERO) THEN
            FRWfromSNW(I,J)=2
          ELSE
            FRWfromSNW(I,J)=1
          ENDIF
         ENDDO
        ENDDO         


C determine available heat due to the ice pack tying the 
C underlying surface water temperature to freezing point
C ======================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj,
CADJ &                          key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)    = comlev1_bibj,
CADJ &                          key = iicekey, byte = isbyte
CADJ STORE FRWfromSNW           = comlev1_bibj,
CADJ &                          key = iicekey, byte = isbyte
#endif

        DO J=1,sNy
         DO I=1,sNx
          IF ( .NOT. inAdMode ) THEN
#ifdef SEAICE_VARIABLE_FREEZING_POINT
           TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
           IF ( theta(I,J,kSurface,bi,bj) .GE. TBC ) THEN
              a_QbyOCN(i,j) = -SEAICE_availHeatFrac
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) *
     &             (HeatCapacity_Cp*rhoConst/QI)
           ELSE
              a_QbyOCN(i,j) = -SEAICE_availHeatFracFrz
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) *
     &             (HeatCapacity_Cp*rhoConst/QI)
           ENDIF
          ELSE
           a_QbyOCN(i,j) = 0.
          ENDIF
cgf heat and water conservation: ok -- since rid of 72.0764 factor
         ENDDO
        ENDDO


C compute ice thickness tendency due to ice-ocean interaction
C ===========================================================

        DO J=1,sNy
         DO I=1,sNx
          d_HEFFbyOCNonICE(I,J)=MAX(a_QbyOCN(i,j), -HEFF(I,J,bi,bj))
c apply tendency
          r_QbyOCN(I,J)=a_QbyOCN(I,J)-d_HEFFbyOCNonICE(I,J)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj) + d_HEFFbyOCNonICE(I,J)
cgf heat and water conservation: ok
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIyneg  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(d_HEFFbyOCNonICE,
     &      'SIyneg  ',0,1,1,bi,bj,myThid)
         ENDIF
        ENDIF
#endif


C compute snow thickness tendency due to snow-atmosphere interaction
C ==================================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_cover(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
c convert a_QbyATM_cover to m of snow         
          IF ( FRWfromSNW(I,J).EQ.2 ) THEN
            tmpscal1=
     &        MAX(r_QbyATM_cover(I,J)*ICE2SNOW, -HSNOW(I,J,bi,bj))
            d_HSNWbyATMonSNW(I,J)= tmpscal1
          ELSE 
c this process is inactive under cold conditions
            d_HSNWbyATMonSNW(I,J)=0. _d 0
          ENDIF
c apply tendency
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbyATMonSNW(I,J) 
          r_QbyATM_cover(I,J)= r_QbyATM_cover(I,J) -
     &                         d_HSNWbyATMonSNW(I,J)/ICE2SNOW
cgf heat and water conservation: ok
         ENDDO
        ENDDO


C compute ice cover fraction tendency from a_QbyATM
C =================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE r_QbyATM_cover(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_cover(:,:)        = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_open(:,:)        = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_cover(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QSWbyATM_open(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          tmpscal1= a_QbyATM_open(I,J)+a_QbyATM_cover(I,J)
     &            - d_HSNWbyATMonSNW(I,J)/ICE2SNOW
c (cannot melt more than all the ice)
          tmpscal2 = MAX(-HEFF(I,J,bi,bj),tmpscal1)
          tmpscal3 = MIN(ZERO,tmpscal2)
#ifdef ALLOW_DIAGNOSTICS
          DIAGarray(I,J) = tmpscal2
#endif
C gain of new ice over open water (>0 by definition)
          tmpscal4 = MAX(ZERO,a_QbyATM_open(I,J))
c compute cover fraction tendency
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           d_AREAbyATM(I,J)=tmpscal4/HO_south
#ifndef SEAICE_GROWTH_LEGACY
     &          +HALF*tmpscal3/heffActual(I,J)
#else
     &          +HALF*tmpscal3*AREApreTH(I,J)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
#endif
          ELSE
           d_AREAbyATM(I,J)=tmpscal4/HO
#ifndef SEAICE_GROWTH_LEGACY
     &          +HALF*tmpscal3/heffActual(I,J)
#else
     &          +HALF*tmpscal3*AREApreTH(I,J)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
#endif
          ENDIF
c apply tendency
           AREA(I,J,bi,bj)=max(0. _d 0 , min( 1. _d 0, 
     &                     AREA(I,J,bi,bj)+d_AREAbyATM(I,J) ) )
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIfice  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIfice  ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif


C compute ice thickness tendency due to the atmosphere.
C Freezing ocean water/melting ice also affects 
C the salt water and heat stocks.
C =================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif

cgf note: this block is not actually tested by lab_sea
cgf where all experiments start in January. So even though
cgf the v1.81=>v1.82 revision would change results in
cgf warming conditions, the lab_sea results were not changed.

        DO J=1,sNy
         DO I=1,sNx
C (cannot melt more than all the ice)
          tmpscal2 = MAX(-HEFF(I,J,bi,bj),r_QbyATM_cover(I,J))
          d_HEFFbyATMonOCN(I,J)= tmpscal2
c apply tendency
          r_QbyATM_cover(I,J)=r_QbyATM_cover(I,J)-d_HEFFbyATMonOCN(I,J)
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + d_HEFFbyATMonOCN(I,J)
cgf heat and water conservation: ok
         ENDDO
        ENDDO


C attribute precip to fresh water or snow stock, 
C depending on atmospheric conditions.
C =================================================
#ifdef ALLOW_ATM_TEMP
        DO J=1,sNy
         DO I=1,sNx
          IF ( FRWfromSNW(I,J).EQ.1 ) THEN
C           add precip as snow
            d_HFRWbyRAIN(I,J)=0. _d 0
            d_HSNWbyRAIN(I,J)=convertPRECIP2HI*ICE2SNOW*
     &            PRECIP(I,J,bi,bj)*AREApreTH(I,J)
          ELSE 
c           add precip to the fresh water bucket
            d_HFRWbyRAIN(I,J)=-convertPRECIP2HI*
     &            PRECIP(I,J,bi,bj)*AREApreTH(I,J)
            d_HSNWbyRAIN(I,J)=0. _d 0
          ENDIF
c apply tendency
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWbyRAIN(I,J) 
         ENDDO
        ENDDO
cgf heat and water conservation: ok -- the air heat gain
cgf to turn rain to snow is not a surface process.
#endif /* ALLOW_ATM_TEMP */


C compute snow tendency due to heat available from ocean.
C can only reduce HSNOW. melted snow is added to fresh water stock.
C =================================================================

cph( very sensitive bit here by JZ
#ifndef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
        DO J=1,sNy
         DO I=1,sNx
c convert r_QbyOCN to ice meters
          IF( r_QbyOCN(i,j) .LT. ZERO .AND.
     &       HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           tmpscal2= MAX(r_QbyOCN(i,j)*ICE2SNOW, -HSNOW(I,J,bi,bj))
          ELSE
           tmpscal2= 0. _d 0
          ENDIF
          d_HSNWbyOCNonSNW(I,J) = tmpscal2
c apply tendency
          r_QbyOCN(I,J)=r_QbyOCN(I,J)
     &                               -d_HSNWbyOCNonSNW(I,J)/ICE2SNOW
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+d_HSNWbyOCNonSNW(I,J)
         ENDDO
        ENDDO
cgf heat and water conservation: ok
#endif /* SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING */
cph)

#ifndef SEAICE_GROWTH_LEGACY
C convert snow to ice if submerged.
C =================================

#ifdef ALLOW_SEAICE_FLOODING
        IF ( SEAICEuseFlooding ) THEN
         DO J=1,sNy
          DO I=1,sNx
           hDraft     = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)/rhoConst
cgf - use of 1000 instead of rhoConst (or rhoConstFresh?)
           tmpscal1 = MAX( 0. _d 0, hDraft - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
c apply tendency
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW  
cgf heat and water conservation: ok -- since ice and snow
cgf are treated as having a consistent latent heat of fusion
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIsnwice',myThid) ) THEN
C     turn tmparr1 into a rate
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



C compute net fresh water flux leaving/entering 
C the ocean, accounting for fresh/salt water stocks.
C ==================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
# ifdef SEAICE_SALINITY
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
# endif /* SEAICE_SALINITY */
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_ATM_TEMP
        DO J=1,sNy
         DO I=1,sNx
         tmpscal1= d_HSNWbyATMonSNW(I,J)/ICE2SNOW
     &            +d_HFRWbyRAIN(I,J)
     &            +d_HSNWbyOCNonSNW(I,J)/ICE2SNOW
     &            +d_HEFFbyOCNonICE(I,J)
     &            +d_HEFFbyATMonOCN(I,J)
     &            +d_HEFFbyNEG(I,J) 
     &            +d_HSNWbyNEG(I,J)/ICE2SNOW
C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREApreTH(I,J) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif
     &         + tmpscal1*convertHI2PRECIP
     &         )*rhoConstFresh
cgf heat and water conservation: ok -- since rid of ICE2WATR
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIatmFW ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj)*(
     &           PRECIP(I,J,bi,bj)
     &           - EVAP(I,J,bi,bj)
     &           *( ONE - AREApreTH(I,J) )
     &           + RUNOFF(I,J,bi,bj)
     &           )*rhoConstFresh
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmFW ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif
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


C COMPUTE SURFACE SALT FLUX AND ADJUST ICE SALINITY
C ==================================================

#ifdef SEAICE_SALINITY

#ifdef SEAICE_GROWTH_LEGACY
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
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
c sum up the terms that affect the salt content of the ice pack
         tmpscal1=d_HEFFbyOCNonICE(I,J)+d_HEFFbyATMonOCN(I,J)
c recompute HEFF before thermodyncamic updates (which is not AREApreTH in legacy code)
         tmpscal2=HEFF(I,J,bi,bj)-tmpscal1-d_HEFFbyFLOODING(I,J)
C tmpscal1 > 0 : m of sea ice that is created
          IF ( tmpscal1 .GE. 0.0 ) THEN
             saltFlux(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm
     &            *SEAICE_salinity*salt(I,j,kSurface,bi,bj)
     &            *tmpscal1*ICE2WATR*rhoConstFresh
#ifdef ALLOW_SALT_PLUME
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm
     &            *(1-SEAICE_salinity)*salt(I,j,kSurface,bi,bj)
     &            *tmpscal1*ICE2WATR*rhoConstFresh
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
#endif /* SEAICE_SALINITY */
#endif /* ALLOW_ATM_TEMP */


C compute net heat flux leaving/entering the ocean, 
C accounting for the part used in melt/freeze processes
C =====================================================

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = r_QbyATM_cover(I,J) + a_QbyATM_open(I,J)
C account for contribution due to ocean-ice interaction.
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
            DIAGarray(I,J) = a_QbyATM_open(I,J) * (ONE-AREApreTH(I,J))
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneto ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIqneti ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = r_QbyATM_cover(I,J) * AREApreTH(I,J)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneti ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif


#ifdef SEAICE_GROWTH_LEGACY

C treat values of ice cover fraction oustide 
C the [0 1] range, and other such issues.
C ===========================================

cgf heat and water conservation: NOT OK

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
          HEFF(I,J,bi,bj)=MIN(MAX_HEFF,HEFF(I,J,bi,bj))
#endif /* SEAICE_CAP_HEFF */
          HSNOW(I,J,bi,bj)  = HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIthdgrh',myThid) ) THEN
C     use (abuse) tmparr1 to diagnose the total thermodynamic growth rate
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
cgf - use of 1000 instead of rhoConst (or rhoConstFresh?)
           tmpscal1 = MAX( 0. _d 0, hDraft - HEFF(I,J,bi,bj))
           d_HEFFbyFLOODING(I,J)=tmpscal1
c apply tendency
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFbyFLOODING(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-
     &                           d_HEFFbyFLOODING(I,J)*ICE2SNOW  
cgf heat and water conservation: ok -- since ice and snow
cgf are treated as having a consistent latent heat of fusion
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIsnwice',myThid) ) THEN
C     turn tmparr1 into a rate
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

C Sea Ice Load on the sea surface.
C =================================

        IF ( useRealFreshWaterFlux ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sIceLoad(i,j,bi,bj) = HEFF(I,J,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
          ENDDO
         ENDDO
        ENDIF


C Sea Ice Age Tracer.
C ===================

#ifdef SEAICE_AGE
# ifndef SEAICE_AGE_VOL
C     Sources and sinks for sea ice age:
C     assume that a) freezing: new ice fraction forms with zero age
C                 b) melting: ice fraction vanishes with current age
        DO J=1,sNy
         DO I=1,sNx
          IF ( AREA(I,J,bi,bj) .GT. 0.15 ) THEN
           IF ( AREA(i,j,bi,bj) .LT. AREApreTH(i,j) ) THEN
C--   scale effective ice-age to account for ice-age sink associated with melting
            IceAge(i,j,bi,bj) = IceAge(i,j,bi,bj)
     &         *AREA(i,j,bi,bj)/AREApreTH(i,j)
           ENDIF
C--   account for aging:
           IceAge(i,j,bi,bj) = IceAge(i,j,bi,bj)
     &        + AREA(i,j,bi,bj) * SEAICE_deltaTtherm
          ELSE
           IceAge(i,j,bi,bj) = ZERO
          ENDIF
         ENDDO
        ENDDO
# else /* ifdef SEAICE_AGE_VOL */
C     Sources and sinks for sea ice age:
C     assume that a) freezing: new ice volume forms with zero age
C                 b) melting: ice volume vanishes with current age
        DO J=1,sNy
         DO I=1,sNx
C--   compute actual age from effective age:
          IF (AREApreTH(i,j).GT.0. _d 0) THEN
           tmpscal1=IceAge(i,j,bi,bj)/AREApreTH(i,j)
          ELSE
           tmpscal1=0. _d 0
          ENDIF         
          IF ( (HEFFpreTH(i,j).LT.HEFF(i,j,bi,bj)).AND.
     &         (AREA(i,j,bi,bj).GT.0.15) ) THEN
           tmpscal2=tmpscal1*HEFFpreTH(i,j)/
     &          HEFF(i,j,bi,bj)+SEAICE_deltaTtherm
          ELSEIF (AREA(i,j,bi,bj).LE.0.15) THEN
           tmpscal2=0. _d 0
          ELSE
           tmpscal2=tmpscal1+SEAICE_deltaTtherm
          ENDIF
C--   re-scale to effective age:
          IceAge(i,j,bi,bj) = tmpscal2*AREA(i,j,bi,bj)
         ENDDO
        ENDDO
# endif /* SEAICE_AGE_VOL */
#endif /* SEAICE_AGE */

c switch heat fluxes from 'effective' ice meters to W/m2

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)*convertHI2Q
          QSW(I,J,bi,bj)  = QSW(I,J,bi,bj)*convertHI2Q
         ENDDO
        ENDDO
        
#ifdef SEAICE_DEBUG
       CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
#endif /* SEAICE_DEBUG */


       ENDDO
      ENDDO

      RETURN
      END
