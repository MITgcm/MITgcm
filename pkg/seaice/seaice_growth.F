C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.66 2010/06/27 20:19:16 jmc Exp $
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
      _RL TBC, SDF, ICE2SNOW
      _RL QI, recip_QI, QS
C     auxillary variables
      _RL snowEnergy
      _RL growthHEFF, growthNeg
#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft
#endif /* ALLOW_SEAICE_FLOODING */
      _RL availHeat     (1:sNx,1:sNy)
      _RL hEffOld       (1:sNx,1:sNy)
      _RL GAREA         (1:sNx,1:sNy)
      _RL GHEFF         (1:sNx,1:sNy)
C     RESID_HEAT is residual heat above freezing in equivalent m of ice
      _RL RESID_HEAT    (1:sNx,1:sNy)
#ifdef SEAICE_SALINITY
      _RL saltFluxAdjust(1:sNx,1:sNy)
#endif

C     FICE  - thermodynamic ice growth rate over sea ice in W/m^2
C             >0 causes ice growth, <0 causes snow and sea ice melt
C     FHEFF - effective thermodynamic ice growth rate over sea ice in W/m^2
C             >0 causes ice growth, <0 causes snow and sea ice melt
C     QNETO  - thermodynamic ice growth rate over open water in W/m^2
C             ( = surface heat flux )
C             >0 causes ice growth, <0 causes snow and sea ice melt
C     QNETI  - net surface heat flux under ice in W/m^2
C     QSWO   - short wave heat flux over ocean in W/m^2
C     QSWI   - short wave heat flux under ice in W/m^2
      _RL FHEFF         (1:sNx,1:sNy)
      _RL FICE          (1:sNx,1:sNy)
      _RL QNETO         (1:sNx,1:sNy)
      _RL QNETI         (1:sNx,1:sNy)
      _RL QSWO          (1:sNx,1:sNy)
      _RL QSWI          (1:sNx,1:sNy)
C
      _RL HCORR         (1:sNx,1:sNy)
C     actual ice thickness with upper and lower limit
      _RL HICE          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hSnwLoc       (1:sNx,1:sNy)
C     wind speed
      _RL UG            (1:sNx,1:sNy)
      _RL SPEED_SQ
C     local copy of AREA
      _RL areaLoc

#ifdef SEAICE_MULTICATEGORY
      INTEGER it
      INTEGER ilockey
      _RL RK
      _RL HICEP         (1:sNx,1:sNy)
      _RL FICEP         (1:sNx,1:sNy)
      _RL QSWIP         (1:sNx,1:sNy)
#endif

#ifdef SEAICE_AGE
C     old_AREA :: hold sea-ice fraction field before any seaice-thermo update
      _RL old_AREA     (1:sNx,1:sNy)
#endif /* SEAICE_AGE */

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

C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze
C     RATIO OF WATER DESITY TO SNOW DENSITY
      SDF          = 1000.0 _d 0/SEAICE_rhoSnow
C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SDF * ICE2WATR
C     HEAT OF FUSION OF ICE (J/m^3)
      QI           = 302.0 _d +06
      recip_QI     = 1.0 _d 0 / QI
C     HEAT OF FUSION OF SNOW (J/m^3)
      QS           = 1.1 _d +08

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
C
C     initialise a few fields
C
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)  = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          FHEFF(I,J)      = 0.0 _d 0
          FICE (I,J)      = 0.0 _d 0
          QNETO(I,J)      = 0.0 _d 0
          QNETI(I,J)      = 0.0 _d 0
          QSWO (I,J)      = 0.0 _d 0
          QSWI (I,J)      = 0.0 _d 0
          HCORR(I,J)      = 0.0 _d 0
          RESID_HEAT(I,J) = 0.0 _d 0
#ifdef SEAICE_SALINITY
          saltFluxAdjust(I,J) = 0.0 _d 0
#endif
#ifdef SEAICE_MULTICATEGORY
          FICEP(I,J)      = 0.0 _d 0
          QSWIP(I,J)      = 0.0 _d 0
#endif
         ENDDO
        ENDDO
        DO J=1-oLy,sNy+oLy
         DO I=1-oLx,sNx+oLx
          saltWtrIce(I,J,bi,bj) = 0.0 _d 0
          frWtrIce(I,J,bi,bj)   = 0.0 _d 0
#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
          frWtrAtm(I,J,bi,bj)   = 0.0 _d 0
#endif
         ENDDO
        ENDDO

#ifdef SEAICE_AGE
C     store the initial ice fraction over the domain
        DO J=1,sNy
         DO I=1,sNx
           old_AREA(i,j) = AREA(I,J,bi,bj)
         ENDDO
        ENDDO
#endif /* SEAICE_AGE */


#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     COMPUTE ACTUAL ICE THICKNESS AND PUT MINIMUM/MAXIMUM
C     ON ICE THICKNESS FOR BUDGET COMPUTATION
C     The default of A22 = 0.15 is a common threshold for defining
C     the ice edge. This ice concentration usually does not occur
C     due to thermodynamics but due to advection.
          areaLoc      = MAX(A22,AREANm1(I,J,bi,bj))
          HICE(I,J)    = HEFFNm1(I,J,bi,bj)/areaLoc
C     Do we know what this is for?
          HICE(I,J)    = MAX(HICE(I,J),0.05 _d +00)
C     Capping the actual ice thickness effectively enforces a
C     minimum of heat flux through the ice and helps getting rid of
C     very thick ice.
cdm actually, this does exactly the opposite, i.e., ice is thicker
cdm when HICE is capped, so I am commenting out
cdm          HICE(I,J)    = MIN(HICE(I,J),9.0 _d +00)
          hSnwLoc(I,J) = HSNOW(I,J,bi,bj)/areaLoc
         ENDDO
        ENDDO

C NOW DETERMINE MIXED LAYER TEMPERATURE
        DO J=1,sNy
         DO I=1,sNx
          TMIX(I,J,bi,bj)=theta(I,J,kSurface,bi,bj)+273.16 _d +00
#ifdef SEAICE_DEBUG
          TMIX(I,J,bi,bj)=MAX(TMIX(I,J,bi,bj),271.2 _d +00)
#endif
         ENDDO
        ENDDO

C THERMAL WIND OF ATMOSPHERE
        DO J=1,sNy
         DO I=1,sNx
C     copy the wind speed computed in exf_wind.F to UG
          UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
CML   this is the old code, which does the same
CML          SPEED_SQ = UWIND(I,J,bi,bj)**2 + VWIND(I,J,bi,bj)**2
CML          IF ( SPEED_SQ .LE. SEAICE_EPS_SQ ) THEN
CML             UG(I,J)=SEAICE_EPS
CML          ELSE
CML             UG(I,J)=SQRT(SPEED_SQ)
CML          ENDIF
         ENDDO
        ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics, byte = isbyte
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics, byte = isbyte
cphCADJ STORE uwind  = comlev1, key = ikey_dynamics, byte = isbyte
cphCADJ STORE vwind  = comlev1, key = ikey_dynamics, byte = isbyte
c
CADJ STORE tice   = comlev1, key = ikey_dynamics, byte = isbyte
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices  = comlev1, key = ikey_dynamics, byte = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW DETERMINE GROWTH RATES
C FIRST DO OPEN WATER
        CALL SEAICE_BUDGET_OCEAN(
     I       UG,
     U       TMIX,
     O       QNETO, QSWO,
     I       bi, bj, myTime, myIter, myThid )

C NOW DO ICE
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
           HICEP(I,J)=(HICE(I,J)/MULTDIM)*((2.0 _d 0*RK)-1.0 _d 0)
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_BUDGET_ICE(
     I        UG, HICEP, hSnwLoc,
     U        TICE,
     O        FICEP, QSWIP,
     I        bi, bj, myTime, myIter, myThid )
         DO J=1,sNy
          DO I=1,sNx
C     average surface heat fluxes/growth rates
           FICE (I,J)          = FICE(I,J) + FICEP(I,J)/MULTDIM
           QSWI (I,J)          = QSWI(I,J) + QSWIP(I,J)/MULTDIM
           TICES(I,J,IT,bi,bj) = TICE(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C--  End loop over multi-categories
#else  /* SEAICE_MULTICATEGORY */
        CALL SEAICE_BUDGET_ICE(
     I       UG, HICE, hSnwLoc,
     U       TICE,
     O       FICE, QSWI,
     I       bi, bj, myTime, myIter, myThid )
#endif /* SEAICE_MULTICATEGORY */

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIatmQnt',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = maskC(I,J,kSurface,bi,bj) * (
     &           FICE(I,J) * areaNm1(I,J,bi,bj) +
     &           QNETO(I,J) * ( ONE - areaNm1(I,J,bi,bj) ) )
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmQnt',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj,
CADJ &                          key = iicekey, byte = isbyte
CADJ STORE heff(:,:,bi,bj)    = comlev1_bibj,
CADJ &                          key = iicekey, byte = isbyte
#endif
C
C--   compute and apply ice growth due to oceanic heat flux from below
C
        DO J=1,sNy
         DO I=1,sNx
C--   Create or melt sea-ice so that first-level oceanic temperature
C     is approximately at the freezing point when there is sea-ice.
C     Initially the units of YNEG/availHeat are m of sea-ice.
C     The factor dRf(1)/72.0764, used to convert temperature
C     change in deg K to m of sea-ice, is approximately:
C     dRf(1) * (sea water heat capacity = 3996 J/kg/K)
C        * (density of sea-water = 1026 kg/m^3)
C        / (latent heat of fusion of sea-ice = 334000 J/kg)
C        / (density of sea-ice = 910 kg/m^3)
C     Negative YNEG/availHeat leads to ice growth.
C     Positive YNEG/availHeat leads to ice melting.
          IF ( .NOT. inAdMode ) THEN
#ifdef SEAICE_VARIABLE_FREEZING_POINT
           TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
           IF ( theta(I,J,kSurface,bi,bj) .GE. TBC ) THEN
              availHeat(i,j) = SEAICE_availHeatFrac
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) / 72.0764 _d 0
           ELSE
              availHeat(i,j) = SEAICE_availHeatFracFrz
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) / 72.0764 _d 0
           ENDIF
          ELSE
           availHeat(i,j) = 0.
          ENDIF
C     local copy of old effective ice thickness
          hEffOld(I,J)      = HEFF(I,J,bi,bj)
C     Melt (availHeat>0) or create (availHeat<0) sea ice
          HEFF(I,J,bi,bj) = MAX(ZERO,HEFF(I,J,bi,bj)-availHeat(I,J))
C
          YNEG(I,J,bi,bj)   = hEffOld(I,J)    - HEFF(I,J,bi,bj)
C
          saltWtrIce(I,J,bi,bj)   = saltWtrIce(I,J,bi,bj)
     &                              - YNEG(I,J,bi,bj)
          RESID_HEAT(I,J)   = availHeat(I,J)  - YNEG(I,J,bi,bj)
C     YNEG now contains m of ice melted (>0) or created (<0)
C     saltWtrIce contains m of ice melted (<0) or created (>0)
C     RESID_HEAT is residual heat above freezing in equivalent m of ice
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIyneg  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(YNEG,'SIyneg  ',0,1,1,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

cph(
#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics, byte = isbyte
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics, byte = isbyte
#endif
cph)
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE fice(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
cph)
C
C--   compute and apply ice growth due to atmospheric fluxes from above
C
        DO J=1,sNy
         DO I=1,sNx
C NOW CALCULATE CORRECTED effective growth in J/m^2 (>0=melt)
          GHEFF(I,J)=-SEAICE_deltaTtherm*FICE(I,J)*AREANm1(I,J,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fice(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          IF(FICE(I,J).LT.ZERO.AND.AREANm1(I,J,bi,bj).GT.ZERO) THEN
C     use FICE to melt snow and CALCULATE CORRECTED GROWTH
C     effective snow thickness in J/m^2
           snowEnergy=HSNOW(I,J,bi,bj)*QS
           IF(GHEFF(I,J).LE.snowEnergy) THEN
C     not enough heat to melt all snow; use up all heat flux FICE
            HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)-GHEFF(I,J)/QS
C     SNOW CONVERTED INTO WATER AND THEN INTO equivalent m of ICE melt
C     The factor 1/ICE2SNOW converts m of snow to m of sea-ice
            frWtrIce(I,J,bi,bj) =
     &       frWtrIce(I,J,bi,bj) - GHEFF(I,J)/(QS*ICE2SNOW)
            FICE    (I,J) = ZERO
           ELSE
C     enought heat to melt snow completely;
C     compute remaining heat flux that will melt ice
            FICE(I,J)=-(GHEFF(I,J)-snowEnergy)/
     &           SEAICE_deltaTtherm/AREANm1(I,J,bi,bj)
C     convert all snow to melt water (fresh water flux)
            frWtrIce(I,J,bi,bj) = frWtrIce(I,J,bi,bj)
     &           -HSNOW(I,J,bi,bj)/ICE2SNOW
            HSNOW(I,J,bi,bj)=0.0 _d 0
           END IF
          END IF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fice(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
C     now get cell averaged growth rate in W/m^2, >0 causes ice growth
          FHEFF(I,J)= FICE(I,J)  * AREANm1(I,J,bi,bj)
     &              + QNETO(I,J) * (ONE-AREANm1(I,J,bi,bj))
         ENDDO
        ENDDO
cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE fice(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE fheff(:,:)        = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qneto(:,:)        = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qswi(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qswo(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
cph)
C
C     First update (freeze or melt) ice area
C
        DO J=1,sNy
         DO I=1,sNx
C     negative growth in meters of ice (>0 for melting)
          growthNeg  = -SEAICE_deltaTtherm*FHEFF(I,J)*recip_QI
C     negative growth must not exceed effective ice thickness (=volume)
C     (that is, cannot melt more than all the ice)
          growthHEFF = -ONE*MIN(HEFF(I,J,bi,bj),growthNeg)
#ifdef ALLOW_DIAGNOSTICS
          DIAGarray(I,J) = growthHEFF
#endif
C     growthHEFF < 0 means melting
          HCORR(I,J) = MIN(ZERO,growthHEFF)
C     gain of new effective ice thickness over open water (>0 by definition)
          GAREA(I,J) = MAX(ZERO,SEAICE_deltaTtherm*QNETO(I,J)*recip_QI)
CML   removed these loops and moved TAMC store directive up
CML         ENDDO
CML        ENDDO
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CMLCADJ &                       key = iicekey, byte = isbyte
CML#endif
CML        DO J=1,sNy
CML         DO I=1,sNx
C     Here we finally compute the new AREA
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           AREA(I,J,bi,bj)=AREA(I,J,bi,bj)+
     &          (ONE-AREANm1(I,J,bi,bj))*GAREA(I,J)/HO_south
     &          +HALF*HCORR(I,J)*AREANm1(I,J,bi,bj)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
          ELSE
           AREA(I,J,bi,bj)=AREA(I,J,bi,bj)+
     &          (ONE-AREANm1(I,J,bi,bj))*GAREA(I,J)/HO
     &          +HALF*HCORR(I,J)*AREANm1(I,J,bi,bj)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIfice  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIfice  ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif
C
C     now update (freeze or melt) HEFF
C
        DO J=1,sNy
         DO I=1,sNx
C     negative growth (>0 for melting) of existing ice in meters
          growthNeg       = -SEAICE_deltaTtherm*
     &                      FICE(I,J)*recip_QI*AREANm1(I,J,bi,bj)
C     negative growth must not exceed effective ice thickness (=volume)
C     (that is, cannot melt more than all the ice)
          growthHEFF      = -ONE*MIN(HEFF(I,J,bi,bj),growthNeg)
C     growthHEFF < 0 means melting
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + growthHEFF
C     add effective growth to fresh water of ice
          saltWtrIce(I,J,bi,bj) = saltWtrIce(I,J,bi,bj) + growthHEFF

C     now calculate QNETI under ice (if any) as the difference between
C     the available "heat flux" growthNeg and the actual growthHEFF;
C     keep in mind that growthNeg and growthHEFF have different signs
C     by construction
          QNETI(I,J) =  (growthHEFF + growthNeg)*QI/SEAICE_deltaTtherm

C     now update other things

#ifdef ALLOW_ATM_TEMP
          IF(FICE(I,J).GT.ZERO) THEN
C     freezing, add precip as snow
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+SEAICE_deltaTtherm*
     &            PRECIP(I,J,bi,bj)*AREANm1(I,J,bi,bj)*SDF
          ELSE
C     add precip as rain, water converted into equivalent m of
C     ice by 1/ICE2WATR.
C     Do not get confused by the sign:
C     precip   > 0 for downward flux of fresh water
C     frWtrIce > 0 for more ice (corresponds to an upward "fresh water flux"),
C     so that here the rain is added *as if* it is melted ice (which is not
C     true, but just a trick; physically the rain just runs as water
C     through the ice into the ocean)
           frWtrIce(I,J,bi,bj) = frWtrIce(I,J,bi,bj)
     &            -PRECIP(I,J,bi,bj)*AREANm1(I,J,bi,bj)*
     &            SEAICE_deltaTtherm/ICE2WATR
          ENDIF
#endif /* ALLOW_ATM_TEMP */

         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
cphCADJ &                        key = iicekey, byte = isbyte
#endif

cph( very sensitive bit here by JZ
#ifndef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
        DO J=1,sNy
         DO I=1,sNx
C     Now melt snow if there is residual heat left in surface level
C     Note that units of YNEG and frWtrIce are m of ice
          IF( RESID_HEAT(I,J) .GT. ZERO .AND.
     &       HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           GHEFF(I,J)       = MIN( HSNOW(I,J,bi,bj)/SDF/ICE2WATR,
     &                             RESID_HEAT(I,J) )
           YNEG(I,J,bi,bj)  = YNEG(I,J,bi,bj) +GHEFF(I,J)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
          IF( RESID_HEAT(I,J) .GT. ZERO .AND.
     &       HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-GHEFF(I,J)*SDF*ICE2WATR
           frWtrIce(I,J,bi,bj)    = frWtrIce(I,J,bi,bj)-GHEFF(I,J)
          ENDIF
         ENDDO
        ENDDO
#endif
cph)

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
C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREANm1(I,J,bi,bj) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif
     &         + frWtrIce(I,J,bi,bj)*ICE2WATR/SEAICE_deltaTtherm
     &         + saltWtrIce(I,J,bi,bj)*ICE2WATR/SEAICE_deltaTtherm
     &         )*rhoConstFresh
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
     &           *( ONE - AREANm1(I,J,bi,bj) )
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
     &         *( ONE - AREANm1(I,J,bi,bj) )
     &         + RUNOFF(I,J,bi,bj)
     &         )*rhoConstFresh
         ENDDO
        ENDDO
#endif

C COMPUTE SURFACE SALT FLUX AND ADJUST ICE SALINITY

#ifdef SEAICE_SALINITY

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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj) = comlev1_bibj,
CADJ &                        key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
C saltWtrIce > 0 : m of sea ice that is created
          IF ( saltWtrIce(I,J,bi,bj) .GE. 0.0 ) THEN
             saltFlux(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)*saltWtrIce(I,J,bi,bj)*
     &            ICE2WATR*rhoConstFresh*SEAICE_salinity*
     &            salt(I,j,kSurface,bi,bj)/SEAICE_deltaTtherm
#ifdef ALLOW_SALT_PLUME
C saltPlumeFlux is defined only during freezing:
             saltPlumeFlux(I,J,bi,bj)=
     &            HEFFM(I,J,bi,bj)*saltWtrIce(I,J,bi,bj)*
     &            ICE2WATR*rhoConstFresh*(1-SEAICE_salinity)*
     &            salt(I,j,kSurface,bi,bj)/SEAICE_deltaTtherm
#endif /* ALLOW_SALT_PLUME */
C saltWtrIce < 0 : m of sea ice that is melted
          ELSE
             saltFlux(I,J,bi,bj) =
     &            HEFFM(I,J,bi,bj)*saltWtrIce(I,J,bi,bj)*
     &            HSALT(I,J,bi,bj)/
     &            (HEFF(I,J,bi,bj)-saltWtrIce(I,J,bi,bj))/
     &            SEAICE_deltaTtherm
#ifdef ALLOW_SALT_PLUME
             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
#endif /* ALLOW_SALT_PLUME */
          ENDIF
C update HSALT based on surface saltFlux
          HSALT(I,J,bi,bj) = HSALT(I,J,bi,bj) +
     &         saltFlux(I,J,bi,bj) * SEAICE_deltaTtherm
          saltFlux(I,J,bi,bj) =
     &         saltFlux(I,J,bi,bj) + saltFluxAdjust(I,J)
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
         ENDDO
        ENDDO
#endif /* SEAICE_SALINITY */
#endif /* ALLOW_ATM_TEMP */

        DO J=1,sNy
         DO I=1,sNx
C NOW GET TOTAL QNET AND QSW
          QNET(I,J,bi,bj) = QNETI(I,J) * AREANm1(I,J,bi,bj)
     &                     +QNETO(I,J) * (ONE-AREANm1(I,J,bi,bj))
          QSW(I,J,bi,bj)  = QSWI(I,J)  * AREANm1(I,J,bi,bj)
     &                     +QSWO(I,J)  * (ONE-AREANm1(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIqneto ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = QNETO(I,J) * (ONE-areaNm1(I,J,bi,bj))
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneto ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIqneti ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = QNETI(I,J) * areaNm1(I,J,bi,bj)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneti ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE yneg(:,:,bi,bj) = comlev1_bibj,
cphCADJ &                       key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
C Now convert YNEG back to deg K.
          YNEG(I,J,bi,bj) = YNEG(I,J,bi,bj)*recip_dRf(kSurface) *
     &                      recip_hFacC(i,j,kSurface,bi,bj)*72.0764 _d 0
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE yneg(:,:,bi,bj) = comlev1_bibj,
CADJ &                       key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
C Add YNEG contribution to QNET
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)
     &         +YNEG(I,J,bi,bj)/SEAICE_deltaTtherm
     &         *maskC(I,J,kSurface,bi,bj)
     &         *HeatCapacity_Cp*rUnit2mass
     &         *drF(kSurface)*hFacC(i,j,kSurface,bi,bj)
         ENDDO
        ENDDO

#ifdef SEAICE_DEBUG
       CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
#endif /* SEAICE_DEBUG */

crg Added by Ralf Giering: do we need DO_WE_NEED_THIS ?
#define DO_WE_NEED_THIS
C NOW ZERO OUTSIDE POINTS
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
#ifdef DO_WE_NEED_THIS
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
#endif /* DO_WE_NEED_THIS */
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
C     use (abuse) GHEFF to diagnose the total thermodynamic growth rate
          DO J=1,sNy
           DO I=1,sNx
            GHEFF(I,J) = (HEFF(I,J,bi,bj)-HEFFNm1(I,J,bi,bj))
     &           /SEAICE_deltaTtherm
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(GHEFF,'SIthdgrh',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_SEAICE_FLOODING
        IF ( SEAICEuseFlooding ) THEN
C     convert snow to ice if submerged
         DO J=1,sNy
          DO I=1,sNx
           hDraft     = (HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)/1000. _d 0
C     here GEFF is the gain of ice due to flooding
           GHEFF(I,J) = hDraft - MIN(hDraft,HEFF(I,J,bi,bj))
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + GHEFF(I,J)
           HSNOW(I,J,bi,bj)  = MAX(0. _d 0,
     &          HSNOW(I,J,bi,bj)-GHEFF(I,J)*ICE2SNOW)
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIsnwice',myThid) ) THEN
C     turn GHEFF into a rate
           DO J=1,sNy
            DO I=1,sNx
             GHEFF(I,J) = GHEFF(I,J)/SEAICE_deltaTtherm
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(GHEFF,'SIsnwice',0,1,3,bi,bj,myThid)
          ENDIF
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */

        IF ( useRealFreshWaterFlux ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sIceLoad(i,j,bi,bj) = HEFF(I,J,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(I,J,bi,bj)*SEAICE_rhoSnow
          ENDDO
         ENDDO
        ENDIF

#ifdef SEAICE_AGE
C     Sources and sinks for sea ice age:
C     assume that a) freezing: new ice fraction forms with zero age
C                 b) melting: ice fraction vanishes with current age
        DO J=1,sNy
         DO I=1,sNx
          IF ( AREA(I,J,bi,bj) .GT. 0.15 ) THEN
           IF ( AREA(i,j,bi,bj) .LT. old_AREA(i,j) ) THEN
C--   scale effective ice-age to account for ice-age sink associated with melting
            IceAge(i,j,bi,bj) = IceAge(i,j,bi,bj)
     &         *AREA(i,j,bi,bj)/old_AREA(i,j)
           ENDIF
C--   account for aging:
           IceAge(i,j,bi,bj) = IceAge(i,j,bi,bj)
     &        + AREA(i,j,bi,bj) * SEAICE_deltaTtherm
          ELSE
           IceAge(i,j,bi,bj) = ZERO
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_AGE */

       ENDDO
      ENDDO

      RETURN
      END
