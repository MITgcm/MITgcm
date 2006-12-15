C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.4 2006/12/15 15:48:23 heimbach Exp $
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
      _RL TBC, salinity_ice, SDF, ICE_DENS, Q0, QS
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
C     SEAICE_SALT contains m of ice melted (<0) or created (>0)
      _RL SEAICE_SALT(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     actual ice thickness
      _RL HICE   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
C     actual snow thickness
      _RL hSnwLoc(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
C     wind speed
      _RL UG     (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL SPEED_SQ
C     local copy of AREA
      _RL areaLoc(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

#ifdef SEAICE_MULTILEVEL
      INTEGER it
      INTEGER ilockey
      _RL RK
      _RL HICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL FICEP(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
#endif

      if ( buoyancyRelation .eq. 'OCEANICP' ) then
       kSurface        = Nr 
      else
       kSurface        = 1
      endif

C     ICE SALINITY (g/kg)
      salinity_ice = 4.0 _d 0
C     FREEZING TEMP. OF SEA WATER (deg C)
      TBC          = SEAICE_freeze
C     RATIO OF WATER DESITY TO SNOW DENSITY
      SDF          = 1000.0 _d 0/330.0 _d 0
C     RATIO OF SEA ICE DESITY TO WATER DENSITY
      ICE_DENS     = 0.920 _d 0
C     INVERSE HEAT OF FUSION OF ICE (m^3/J)
      Q0           = 1.0 _d -06 / 302.0 _d +00
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
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qnet(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE qsw(:,:,bi,bj)    = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          areaLoc(I,J)     = MAX(A22,AREA(I,J,2,bi,bj))
          FHEFF(I,J)       = 0.0 _d 0
          FICE (I,J)       = 0.0 _d 0
#ifdef SEAICE_MULTILEVEL
          FICEP(I,J)       = 0.0 _d 0
#endif
          FHEFF(I,J)       = 0.0 _d 0
          FICE (I,J)       = 0.0 _d 0
          QNETO(I,J)       = 0.0 _d 0
          QNETI(I,J)       = 0.0 _d 0
          QSWO (I,J)       = 0.0 _d 0
          QSWI (I,J)       = 0.0 _d 0
          HCORR(I,J)       = 0.0 _d 0
          SEAICE_SALT(I,J) = 0.0 _d 0
          RESID_HEAT (I,J) = 0.0 _d 0
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
cph need to adjoint-store AREA again before using it in further init.
cph (all these initialisations involving AREA are nasty "non-linear")
          HICE(I,J)    = HEFF(I,J,2,bi,bj)/areaLoc(I,J)
          hSnwLoc(I,J) = HSNOW(I,J,bi,bj)/areaLoc(I,J)
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
# ifdef SEAICE_MULTILEVEL
CADJ STORE tices  = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW DETERMINE GROWTH RATES
C FIRST DO OPEN WATER
        CALL SEAICE_BUDGET_OCEAN(
     I       UG, 
     U       TMIX, 
     O       QNETO, QSWO, 
     I       bi, bj)

C NOW DO ICE
#ifdef SEAICE_MULTILEVEL
C--  Start loop over muli-levels
        DO IT=1,MULTDIM
#ifdef ALLOW_AUTODIFF_TAMC
         ilockey = (iicekey-1)*MULTDIM + IT
CADJ STORE tices(:,:,it,bi,bj) = comlev1_multdim, 
CADJ &                           key = ilockey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO J=1,sNy
          DO I=1,sNx
           RK=IT*1.0
           HICEP(I,J)=(HICE(I,J)/7.0 _d 0)*((2.0 _d 0*RK)-1.0 _d 0)
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_BUDGET_ICE(
     I        UG, HICE, hSnwLoc, 
     U        TICE, 
     O        FICE, QSWI, 
     I        bi, bj)
         DO J=1,sNy
          DO I=1,sNx
           FICEP(I,J)=(FICE(I,J)/7.0 _d 0)+FICEP(I,J)
           TICES(I,J,IT,bi,bj)=TICE(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C--  End loop over muli-levels
        DO J=1,sNy
         DO I=1,sNx
          FICE(I,J)=FICEP(I,J)
         ENDDO
        ENDDO
#else  /* SEAICE_MULTILEVEL */
        CALL SEAICE_BUDGET_ICE(
     I       UG, HICE, hSnwLoc, 
     U       TICE, 
     O       FICE, QSWI, 
     I       bi, bj)
#endif /* SEAICE_MULTILEVEL */
     
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)= comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C--   Create or melt sea-ice so that first-level oceanic temperature
C     is approximately at the freezing point when there is sea-ice.
C     Initially the units of YNEG are m of sea-ice.
C     The factor dRf(1)/72.0764, used to convert temperature
C     change in deg K to m of sea-ice, is approximately:
C     dRf(1) * (sea water heat capacity = 3996 J/kg/K)
C        * (density of sea-water = 1026 kg/m^3)
C        / (latent heat of fusion of sea-ice = 334000 J/kg)
C        / (density of sea-ice = 910 kg/m^3)
C     Negative YNEG leads to ice growth.
C     Positive YNEG leads to ice melting.
          IF ( .NOT. inAdMode ) THEN
#ifdef SEAICE_VARIABLE_FREEZING_POINT
           TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
           YNEG(I,J,bi,bj) = (theta(I,J,kSurface,bi,bj)-TBC)
     &          *dRf(1)/72.0764 _d 0
          ELSE
           YNEG(I,J,bi,bj)= 0.
          ENDIF
          GHEFF(I,J)=HEFF(I,J,1,bi,bj)
C     Melt (YNEG>0) or create (YNEG<0) sea ice
          HEFF(I,J,1,bi,bj)=MAX(ZERO,HEFF(I,J,1,bi,bj)-YNEG(I,J,bi,bj))
          RESID_HEAT(I,J)  = YNEG(I,J,bi,bj)
          YNEG(I,J,bi,bj)  = GHEFF(I,J)-HEFF(I,J,1,bi,bj)
          SEAICE_SALT(I,J) = SEAICE_SALT(I,J)-YNEG(I,J,bi,bj)
          RESID_HEAT(I,J)  =  RESID_HEAT(I,J)-YNEG(I,J,bi,bj)
C     YNEG now contains m of ice melted (>0) or created (<0)
C     SEAICE_SALT contains m of ice melted (<0) or created (>0)
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
C use FICE to melt snow and CALCULATE CORRECTED GROWTH
           GAREA(I,J)=HSNOW(I,J,bi,bj)*QS ! effective snow thickness in J/m^2
           IF(GHEFF(I,J).LE.GAREA(I,J)) THEN
C not enough heat to melt all snow; use up all heat flux FICE
            HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)-GHEFF(I,J)/QS
C SNOW CONVERTED INTO WATER AND THEN INTO equivalent m of ICE melt
C The factor 1/SDF/ICE_DENS converts m of snow to m of sea-ice
            SEAICE_SALT(I,J)=SEAICE_SALT(I,J)
     &           -GHEFF(I,J)/QS/SDF/ICE_DENS
            FICE(I,J)=ZERO
           ELSE
C enought heat to melt snow completely;
C compute remaining heat flux that will melt ice
            FICE(I,J)=-(GHEFF(I,J)-GAREA(I,J))/
     &           SEAICE_deltaTtherm/AREA(I,J,2,bi,bj)
C     convert all snow to melt water (fresh water flux)
            SEAICE_SALT(I,J)=SEAICE_SALT(I,J)
     &           -HSNOW(I,J,bi,bj)/SDF/ICE_DENS
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
C NOW GET TOTAL GROWTH RATE in W/m^2, >0 causes ice growth
          FHEFF(I,J)= FICE(I,J)  * AREA(I,J,2,bi,bj)
     &              + QNETO(I,J) * (ONE-AREA(I,J,2,bi,bj))
         ENDDO
        ENDDO
cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
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
        DO J=1,sNy
         DO I=1,sNx
C NOW UPDATE AREA
          GHEFF(I,J) = -SEAICE_deltaTtherm*FHEFF(I,J)*Q0
          GAREA(I,J) =  SEAICE_deltaTtherm*QNETO(I,J)*Q0
          GHEFF(I,J) = -ONE*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
          GAREA(I,J) = MAX(ZERO,GAREA(I,J))
          HCORR(I,J) = MIN(ZERO,GHEFF(I,J))
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx
          GAREA(I,J)=(ONE-AREA(I,J,2,bi,bj))*GAREA(I,J)/HO
     &    +HALF*HCORR(I,J)*AREA(I,J,2,bi,bj)
     &    /(HEFF(I,J,1,bi,bj)+.00001 _d 0)
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)+GAREA(I,J)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif
        DO J=1,sNy
         DO I=1,sNx

C NOW UPDATE HEFF
          GHEFF(I,J)       = -SEAICE_deltaTtherm*
     &                       FICE(I,J)*Q0*AREA(I,J,2,bi,bj)
          GHEFF(I,J)       = -ONE*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
          HEFF(I,J,1,bi,bj)= HEFF(I,J,1,bi,bj)+GHEFF(I,J)
          SEAICE_SALT(I,J) = SEAICE_SALT(I,J)+GHEFF(I,J)

C NOW CALCULATE QNETI UNDER ICE IF ANY
          QNETI(I,J)       = (GHEFF(I,J)-SEAICE_deltaTtherm*
     &       FICE(I,J)*Q0*AREA(I,J,2,bi,bj))/Q0/SEAICE_deltaTtherm

C NOW UPDATE OTHER THINGS

          IF(FICE(I,J).GT.ZERO) THEN
C FREEZING, PRECIP ADDED AS SNOW
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+SEAICE_deltaTtherm*
     &            PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*SDF
          ELSE
C ADD PRECIP AS RAIN, WATER CONVERTED INTO equivalent m of ICE BY 1/ICE_DENS
           SEAICE_SALT(I,J) = SEAICE_SALT(I,J)
     &            -PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*
     &            SEAICE_deltaTtherm/ICE_DENS
          ENDIF

C Now add in precip over open water directly into ocean as negative salt
          SEAICE_SALT(I,J) = SEAICE_SALT(I,J)
     &         -PRECIP(I,J,bi,bj)*(ONE-AREA(I,J,2,bi,bj))
     &         *SEAICE_deltaTtherm/ICE_DENS

C Now melt snow if there is residual heat left in surface level
C Note that units of YNEG and SEAICE_SALT are m of ice
cph( very sensitive bit here by JZ
          IF( RESID_HEAT(I,J) .GT. ZERO
     &        .AND. HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           GHEFF(I,J)       = MIN( HSNOW(I,J,bi,bj)/SDF/ICE_DENS,
     &                             RESID_HEAT(I,J) )
           YNEG(I,J,bi,bj)  = YNEG(I,J,bi,bj)+GHEFF(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)-GHEFF(I,J)*SDF*ICE_DENS
           SEAICE_SALT(I,J) = SEAICE_SALT(I,J)-GHEFF(I,J)
          ENDIF
cph)

C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)  = maskC(I,J,kSurface,bi,bj)*(
     &         EVAP(I,J,bi,bj)*(ONE-AREA(I,J,2,bi,bj))
     &         -RUNOFF(I,J,bi,bj)
     &         +SEAICE_SALT(I,J)*ICE_DENS/SEAICE_deltaTtherm
     &         )

C NOW GET TOTAL QNET AND QSW
          QNET(I,J,bi,bj) = QNETI(I,J) * AREA(I,J,2,bi,bj)
     &                     +QNETO(I,J) * (ONE-AREA(I,J,2,bi,bj))
          QSW(I,J,bi,bj)  = QSWI(I,J)  * AREA(I,J,2,bi,bj)
     &                     +QSWO(I,J)  * (ONE-AREA(I,J,2,bi,bj))

C Now convert YNEG back to deg K.
          YNEG(I,J,bi,bj) = YNEG(I,J,bi,bj)*recip_dRf(1)*72.0764 _d 0

C Add YNEG contribution to QNET
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)
     &         +YNEG(I,J,bi,bj)/SEAICE_deltaTtherm
     &         *maskC(I,J,kSurface,bi,bj)
     &         *HeatCapacity_Cp*recip_horiVertRatio*rhoConst
     &         *drF(kSurface)*hFacC(i,j,kSurface,bi,bj)

         ENDDO
        ENDDO

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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GHEFF(I,J)=SQRT(UICE(I,J,1,bi,bj)**2+VICE(I,J,1,bi,bj)**2)
          GAREA(I,J)=HEFF(I,J,1,bi,bj)
          print*,'I J QNET:',I, J, QNET(i,j,bi,bj), QSW(I,J,bi,bj)
         ENDDO
        ENDDO
       CALL PLOT_FIELD_XYRL( GHEFF,'Current UICE ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( GAREA,'Current HEFF ', myIter, myThid )
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
#endif
          AREA(I,J,1,bi,bj) = AREA(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,1,bi,bj) = HEFF(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
#ifdef DO_WE_NEED_THIS
c          HEFF(I,J,1,bi,bj)=MIN(MAX_HEFF,HEFF(I,J,1,bi,bj))
#endif
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
           HSNOW(I,J,bi,bj)  = MAX(0. _d 0,HSNOW(I,J,bi,bj)-hFlood/SDF)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */

#ifdef ATMOSPHERIC_LOADING
        IF ( useRealFreshWaterFlux ) THEN
         DO J=1,sNy
          DO I=1,sNx
           sIceLoad(i,j,bi,bj) = HEFF(I,J,1,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(I,J,bi,bj)* 330. _d 0
          ENDDO
         ENDDO
        ENDIF
#endif

       ENDDO
      ENDDO

      RETURN
      END
