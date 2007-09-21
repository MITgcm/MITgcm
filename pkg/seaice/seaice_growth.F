C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.27 2007/09/21 00:18:48 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_GROWTH( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE seaice_growth                                 |
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
#include "SEAICE_FFIELDS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
CEndOfInterface

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
C     number of surface interface layer
      INTEGER kSurface
C     constants
      _RL TBC, SDF, ICE2SNOW
      _RL QI, recip_QI, QS, recip_QS
C     auxillary variables
      _RL availHeat, hEffOld, snowEnergy, snowAsIce
      _RL growthHEFF, growthNeg, saltFluxAdjust
#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft, hFlood
#endif /* ALLOW_SEAICE_FLOODING */
      _RL GAREA      ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
      _RL GHEFF      ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )
C     RESID_HEAT is residual heat above freezing in equivalent m of ice
      _RL RESID_HEAT ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

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
      _RL FHEFF  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FICE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QNETO  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QNETI  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSWO   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QSWI   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     
      _RL HCORR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     saltWtrIce contains m of salty ice melted (<0) or created (>0)
      _RL saltWtrIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     frWtrIce contains m of freshwater ice melted (<0) or created (>0)
C     that is, ice due to precipitation or snow
      _RL frWtrIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     frWtrAtm contains freshwater flux from the atmosphere
      _RL frWtrAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     actual ice thickness with upper and lower limit
      _RL HICE   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
C     actual snow thickness
      _RL hSnwLoc(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
C     wind speed
      _RL UG     (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL SPEED_SQ
C     local copy of AREA
      _RL areaLoc

#ifdef SEAICE_MULTICATEGORY
      INTEGER it
      INTEGER ilockey
      _RL RK
      _RL HICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL FICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL QSWIP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
#endif

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface        = Nr 
      else
       kSurface        = 1
      endif

C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze
C     RATIO OF WATER DESITY TO SNOW DENSITY
      SDF          = 1000.0 _d 0/330.0 _d 0
C     RATIO OF SEA ICE DENSITY to SNOW DENSITY
      ICE2SNOW     = SDF * ICE2WATR
C     HEAT OF FUSION OF ICE (m^3/J)
      QI           = 302.0 _d +06
      recip_QI     = 1.0 _d 0 / QI
C     HEAT OF FUSION OF SNOW (J/m^3)
      QS           = 1.1 _d +08
      recip_QS     = 1.1 _d 0 / QS

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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)    = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          FHEFF(I,J)      = 0.0 _d 0
          FICE (I,J)      = 0.0 _d 0
#ifdef SEAICE_MULTICATEGORY
          FICEP(I,J)      = 0.0 _d 0
          QSWIP(I,J)      = 0.0 _d 0
#endif
          FHEFF(I,J)      = 0.0 _d 0
          FICE (I,J)      = 0.0 _d 0
          QNETO(I,J)      = 0.0 _d 0
          QNETI(I,J)      = 0.0 _d 0
          QSWO (I,J)      = 0.0 _d 0
          QSWI (I,J)      = 0.0 _d 0
          HCORR(I,J)      = 0.0 _d 0
          saltWtrIce(I,J) = 0.0 _d 0
          frWtrIce(I,J)   = 0.0 _d 0
          frWtrAtm(I,J)   = 0.0 _d 0
          RESID_HEAT(I,J) = 0.0 _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     COMPUTE ACTUAL ICE THICKNESS AND PUT MINIMUM/MAXIMUM 
C     ON ICE THICKNESS FOR BUDGET COMPUTATION
C     The default of A22 = 0.15 is a common threshold for defining 
C     the ice edge. This ice concentration usually does not occur 
C     due to thermodynamics but due to advection.
          areaLoc      = MAX(A22,AREA(I,J,2,bi,bj))
          HICE(I,J)    = HEFF(I,J,2,bi,bj)/areaLoc
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
CML#ifdef SEAICE_EXTERNAL_FORCING
CMLC     this seems to be more natural as we do compute the wind speed in 
CMLC     pkg/exf/exf_wind.F, but it changes the results
CML          UG(I,J) = MAX(SEAICE_EPS,wspeed(I,J,bi,bj))
CML#else
          SPEED_SQ = UWIND(I,J,bi,bj)**2 + VWIND(I,J,bi,bj)**2
          IF ( SPEED_SQ .LE. SEAICE_EPS_SQ ) THEN
             UG(I,J)=SEAICE_EPS
          ELSE
             UG(I,J)=SQRT(SPEED_SQ)
          ENDIF
CML#endif /* SEAICE_EXTERNAL_FORCING */
         ENDDO
        ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics
cphCADJ STORE uwind  = comlev1, key = ikey_dynamics
cphCADJ STORE vwind  = comlev1, key = ikey_dynamics
c
CADJ STORE tice   = comlev1, key = ikey_dynamics
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices  = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW DETERMINE GROWTH RATES
C FIRST DO OPEN WATER
        CALL SEAICE_BUDGET_OCEAN(
     I       UG, 
     U       TMIX, 
     O       QNETO, QSWO, 
     I       bi, bj, myThid )

C NOW DO ICE
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
     I        bi, bj, myThid )
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
     I       bi, bj, myThid )
#endif /* SEAICE_MULTICATEGORY */
     
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)= comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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
           availHeat = SEAICE_availHeatFrac
     &          * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &          * hFacC(i,j,kSurface,bi,bj) / 72.0764 _d 0
          ELSE
           availHeat = 0.
          ENDIF
C     local copy of old effective ice thickness
          hEffOld           = HEFF(I,J,1,bi,bj)
C     Melt (availHeat>0) or create (availHeat<0) sea ice
          HEFF(I,J,1,bi,bj) = MAX(ZERO,HEFF(I,J,1,bi,bj)-availHeat)
C     
          YNEG(I,J,bi,bj)   = hEffOld         - HEFF(I,J,1,bi,bj)
C     
          saltWtrIce(I,J)   = saltWtrIce(I,J) - YNEG(I,J,bi,bj)
          RESID_HEAT(I,J)   = availHeat       - YNEG(I,J,bi,bj)
C     YNEG now contains m of ice melted (>0) or created (<0)
C     saltWtrIce contains m of ice melted (<0) or created (>0)
C     RESID_HEAT is residual heat above freezing in equivalent m of ice
         ENDDO
        ENDDO

cph(
#ifdef ALLOW_AUTODIFF_TAMC
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics
#endif
cph)
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
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
          GHEFF(I,J)=-SEAICE_deltaTtherm*FICE(I,J)*AREA(I,J,2,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fice(:,:)         = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          IF(FICE(I,J).LT.ZERO.AND.AREA(I,J,2,bi,bj).GT.ZERO) THEN
C     use FICE to melt snow and CALCULATE CORRECTED GROWTH
C     effective snow thickness in J/m^2
           snowEnergy=HSNOW(I,J,bi,bj)*QS 
           IF(GHEFF(I,J).LE.snowEnergy) THEN
C     not enough heat to melt all snow; use up all heat flux FICE
            HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)-GHEFF(I,J)/QS
C     SNOW CONVERTED INTO WATER AND THEN INTO equivalent m of ICE melt
C     The factor 1/ICE2SNOW converts m of snow to m of sea-ice
            frWtrIce(I,J) = frWtrIce(I,J) - GHEFF(I,J)/(QS*ICE2SNOW)
            FICE    (I,J) = ZERO
           ELSE
C     enought heat to melt snow completely;
C     compute remaining heat flux that will melt ice
            FICE(I,J)=-(GHEFF(I,J)-snowEnergy)/
     &           SEAICE_deltaTtherm/AREA(I,J,2,bi,bj)
C     convert all snow to melt water (fresh water flux)
            frWtrIce(I,J) = frWtrIce(I,J)
     &           -HSNOW(I,J,bi,bj)/ICE2SNOW
            HSNOW(I,J,bi,bj)=0.0
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
          FHEFF(I,J)= FICE(I,J)  * AREA(I,J,2,bi,bj)
     &              + QNETO(I,J) * (ONE-AREA(I,J,2,bi,bj))
         ENDDO
        ENDDO
cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
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
          growthHEFF = -ONE*MIN(HEFF(I,J,1,bi,bj),growthNeg)
C     growthHEFF < 0 means melting
          HCORR(I,J) = MIN(ZERO,growthHEFF)
C     gain of new effective ice thickness over open water (>0 by definition)
          GAREA(I,J) = MAX(ZERO,SEAICE_deltaTtherm*QNETO(I,J)*recip_QI)
CML   removed these loops and moved TAMC store directive up
CML         ENDDO
CML        ENDDO
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLCADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CMLCADJ &                         key = iicekey, byte = isbyte
CML#endif
CML        DO J=1,sNy
CML         DO I=1,sNx
C     Here we finally compute the new AREA
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)+
     &         (ONE-AREA(I,J,2,bi,bj))*GAREA(I,J)/HO
     &         +HALF*HCORR(I,J)*AREA(I,J,2,bi,bj)
     &         /(HEFF(I,J,1,bi,bj)+.00001 _d 0)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif
C     
C     now update (freeze or melt) HEFF
C        
        DO J=1,sNy
         DO I=1,sNx
C     negative growth (>0 for melting) of existing ice in meters
          growthNeg        = -SEAICE_deltaTtherm*
     &                       FICE(I,J)*recip_QI*AREA(I,J,2,bi,bj)
C     negative growth must not exceed effective ice thickness (=volume)
C     (that is, cannot melt more than all the ice)
          growthHEFF       = -ONE*MIN(HEFF(I,J,1,bi,bj),growthNeg)
C     growthHEFF < 0 means melting
          HEFF(I,J,1,bi,bj)= HEFF(I,J,1,bi,bj) + growthHEFF
C     add effective growth to fresh water of ice
          saltWtrIce(I,J)  = saltWtrIce(I,J)   + growthHEFF

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
     &            PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*SDF
          ELSE
C     add precip as rain, water converted into equivalent m of 
C     ice by 1/ICE2WATR. 
C     Do not get confused by the sign: 
C     precip   > 0 for downward flux of fresh water
C     frWtrIce > 0 for more ice (corresponds to an upward "fresh water flux"),
C     so that here the rain is added *as if* it is melted ice (which is not 
C     true, but just a trick; physically the rain just runs as water 
C     through the ice into the ocean)
           frWtrIce(I,J) = frWtrIce(I,J)
     &            -PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*
     &            SEAICE_deltaTtherm/ICE2WATR
          ENDIF
#else /* ALLOW_ATM_TEMP */
          STOP 'ABNORMAL END: S/R THSICE_GROWTH: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */

         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
C     Now melt snow if there is residual heat left in surface level
C     Note that units of YNEG and frWtrIce are m of ice
cph( very sensitive bit here by JZ
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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx

          IF( RESID_HEAT(I,J) .GT. ZERO .AND.
     &       HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-GHEFF(I,J)*SDF*ICE2WATR
           frWtrIce(I,J)    = frWtrIce(I,J)   -GHEFF(I,J)
          ENDIF
cph)

         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                        key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx

#ifdef ALLOW_ATM_TEMP

C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         ( EVAP(I,J,bi,bj)-PRECIP(I,J,bi,bj) )
     &         * ( ONE - AREA(I,J,2,bi,bj) )
#ifdef ALLOW_RUNOFF
     &         - RUNOFF(I,J,bi,bj)
#endif
     &         + frWtrIce(I,J)*ICE2WATR/SEAICE_deltaTtherm
     &         + saltWtrIce(I,J)*ICE2WATR/SEAICE_deltaTtherm
     &         )
#ifdef ALLOW_DIAGNOSTICS
          frWtrAtm(I,J)   = maskC(I,J,kSurface,bi,bj)*(
     &           PRECIP(I,J,bi,bj)
     &         - EVAP(I,J,bi,bj)
     &               *( ONE - AREA(I,J,2,bi,bj) )
     &         + RUNOFF(I,J,bi,bj)
     &         )
#endif

C COMPUTE SURFACE SALT FLUX AND ADJUST ICE SALINITY
#ifdef SEAICE_SALINITY
C set HSALT = 0 if HSALT < 0 and compute salt to remove from ocean
          IF ( HSALT(I,J,bi,bj) .LT. 0.0 ) THEN
             saltFluxAdjust = - HEFFM(I,J,bi,bj) *
     &            HSALT(I,J,bi,bj) / SEAICE_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0
          ENDIF
C saltWtrIce > 0 : m of sea ice that is created
          IF ( saltWtrIce(I,J) .GE. 0.0 ) THEN
             saltFlux(I,J,bi,bj) = HEFFM(I,J,bi,bj)*saltWtrIce(I,J)*
     &            ICE2WATR*rhoConstFresh*SEAICE_salinity/
     &            SEAICE_deltaTtherm
C saltWtrIce < 0 : m of sea ice that is melted
          ELSE
             saltFlux(I,J,bi,bj) = HEFFM(I,J,bi,bj)*saltWtrIce(I,J)*
     &            HSALT(I,J,bi,bj)/(HEFF(I,J,1,bi,bj)-saltWtrIce(I,J))/
     &            SEAICE_deltaTtherm
          ENDIF
C update HSALT based on surface saltFlux
          HSALT(I,J,bi,bj) = HSALT(I,J,bi,bj) +
     &         saltFlux(I,J,bi,bj) * SEAICE_deltaTtherm
          saltFlux(I,J,bi,bj) = saltFlux(I,J,bi,bj) + saltFluxAdjust
C set HSALT = 0 if HEFF = 0 and compute salt to dump into ocean
          IF ( HEFF(I,J,1,bi,bj) .EQ. 0.0 ) THEN
             saltFlux(I,J,bi,bj) = saltFlux(I,J,bi,bj) -
     &            HEFFM(I,J,bi,bj) * HSALT(I,J,bi,bj) /
     &            SEAICE_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0
          ENDIF
#endif /* SEAICE_SALINITY */

#else /* ALLOW_ATM_TEMP */
          STOP 'ABNORMAL END: S/R THSICE_GROWTH: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */

C NOW GET TOTAL QNET AND QSW
          QNET(I,J,bi,bj) = QNETI(I,J) * AREA(I,J,2,bi,bj)
     &                     +QNETO(I,J) * (ONE-AREA(I,J,2,bi,bj))
          QSW(I,J,bi,bj)  = QSWI(I,J)  * AREA(I,J,2,bi,bj)
     &                     +QSWO(I,J)  * (ONE-AREA(I,J,2,bi,bj))

         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE yneg(:,:,bi,bj) = comlev1_bibj, 
CADJ &                       key = iicekey, byte = isbyte
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

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(frWtrAtm,'SIatmFW ',0,1 ,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef SEAICE_DEBUG
c      CALL PLOT_FIELD_XYRS( UWIND,'Current UWIND ', myIter, myThid )
c      CALL PLOT_FIELD_XYRS( VWIND,'Current VWIND ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( GWATX,'Current GWATX ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( GWATY,'Current GWATY ', myIter, myThid )
CML       CALL PLOT_FIELD_XYRL( FO,'Current FO ', myIter, myThid )
CML       CALL PLOT_FIELD_XYRL( FHEFF,'Current FHEFF ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
CML       DO j=1-OLy,sNy+OLy
CML        DO i=1-OLx,sNx+OLx
CML         GHEFF(I,J)=SQRT(UICE(I,J,1,bi,bj)**2+VICE(I,J,1,bi,bj)**2)
CML         GAREA(I,J)=HEFF(I,J,1,bi,bj)
CML         print*,'I J QNET:',I, J, QNET(i,j,bi,bj), QSW(I,J,bi,bj)
CML        ENDDO
CML       ENDDO
CML       CALL PLOT_FIELD_XYRL( GHEFF,'Current UICE ', myIter, myThid )
CML       CALL PLOT_FIELD_XYRL( GAREA,'Current HEFF ', myIter, myThid )
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          if(HEFF(i,j,1,bi,bj).gt.1.) then
             print '(A,2i4,3f10.2)','#### i j heff theta yneg',i,j,
     &         HEFF(i,j,1,bi,bj),theta(I,J,1,bi,bj),yneg(I,J,bi,bj)
             print '(A,3f10.2)','QSW, QNET before/after correction',
     &         QSW(I,J,bi,bj),QNETI(I,J)*AREA(I,J,2,bi,bj)+
     &         (ONE-AREA(I,J,2,bi,bj))*QNETO(I,J), QNET(I,J,bi,bj)
          endif
         ENDDO
        ENDDO
#endif /* SEAICE_DEBUG */

crg Added by Ralf Giering: do we need DO_WE_NEED_THIS ?
#define DO_WE_NEED_THIS
C NOW ZERO OUTSIDE POINTS
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C NOW SET AREA(I,J,1,bi,bj)=0 WHERE NO ICE IS
          AREA(I,J,1,bi,bj)=MIN(AREA(I,J,1,bi,bj)
     &                         ,HEFF(I,J,1,bi,bj)/.0001 _d 0)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C NOW TRUNCATE AREA
#ifdef DO_WE_NEED_THIS
          AREA(I,J,1,bi,bj)=MIN(ONE,AREA(I,J,1,bi,bj))
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          AREA(I,J,1,bi,bj) = MAX(ZERO,AREA(I,J,1,bi,bj))
          HSNOW(I,J,bi,bj)  = MAX(ZERO,HSNOW(I,J,bi,bj))
#endif /* DO_WE_NEED_THIS */
          AREA(I,J,1,bi,bj) = AREA(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,1,bi,bj) = HEFF(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
#ifdef SEAICE_CAP_HEFF
          HEFF(I,J,1,bi,bj)=MIN(MAX_HEFF,HEFF(I,J,1,bi,bj))
#endif /* SEAICE_CAP_HEFF */
          HSNOW(I,J,bi,bj)  = HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

#ifdef ALLOW_SEAICE_FLOODING
        IF ( SEAICEuseFlooding ) THEN
C     convert snow to ice if submerged
         DO J=1,sNy
          DO I=1,sNx
           hDraft = (HSNOW(I,J,bi,bj)*330. _d 0
     &              +HEFF(I,J,1,bi,bj)*SEAICE_rhoIce)/1000. _d 0
           hFlood = hDraft - MIN(hDraft,HEFF(I,J,1,bi,bj))
           HEFF(I,J,1,bi,bj) = HEFF(I,J,1,bi,bj) + hFlood
           HSNOW(I,J,bi,bj)  = MAX(0. _d 0,
     &          HSNOW(I,J,bi,bj)-hFlood*ICE2SNOW)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */

        IF ( useRealFreshWaterFlux ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sIceLoad(i,j,bi,bj) = HEFF(I,J,1,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(I,J,bi,bj)* 330. _d 0
          ENDDO
         ENDDO
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END
