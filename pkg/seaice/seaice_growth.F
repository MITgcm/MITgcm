C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_growth.F,v 1.75 2010/10/01 21:36:45 gforget Exp $
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

C note: for all heat stocks, the sign convention is positive towards the atmosphere
C
C     a_QbyATM_cover :: available heat (in W/m^2) due to the interaction of  
C             the atmosphere and the ocean surface - for ice covered water
C     a_QbyATM_open :: same but for open water
C     a_QbyATM :: weighted average (depending on the ice cover fraction)
C             of this available heat over the grid cell.
C     r_QbyATM_cover :: residual of a_QbyATM_cover after freezing/melting 
C             processes have been accounted for
      _RL a_QbyATM_cover          (1:sNx,1:sNy)
      _RL a_QbyATM_open         (1:sNx,1:sNy)
      _RL a_QbyATM         (1:sNx,1:sNy)
      _RL r_QbyATM_cover          (1:sNx,1:sNy)
C     a_QSWbyATM_open   - short wave heat flux over ocean in W/m^2
C     a_QSWbyATM_cover  - short wave heat flux under ice in W/m^2
      _RL a_QSWbyATM_open          (1:sNx,1:sNy)
      _RL a_QSWbyATM_cover          (1:sNx,1:sNy)
C     a_QbyICE :: available heat (in equivalent m of ice) due to   
C             the interaction of the ice pack and the ocean surface
C     r_QbyICE :: residual of a_QbyICE after freezing/melting 
C             processes have been accounted for
      _RL a_QbyICE    (1:sNx,1:sNy)
      _RL r_QbyICE    (1:sNx,1:sNy)

c FRWfromSNW :: flag that states whether the atmostpheric conditions are 
c     prone to generate SNOW (FRWfromSNW=1) or fresh water (FRWfromSNW=2)
      integer FRWfromSNW (1:sNx,1:sNy)

C Available Heat tendencies associated with melt/freeze processes
      _RL d_QbyICE    (1:sNx,1:sNy)
      _RL d_QbySNW    (1:sNx,1:sNy)
      _RL d_QbyATMonOCN    (1:sNx,1:sNy)
      _RL d_QbyATMonSNW    (1:sNx,1:sNy)

c ICE/SNOW stocks tendencies associated with the various melt/freeze processes
      _RL d_AREAbyATM    (1:sNx,1:sNy)
c
      _RL d_HEFFbyICEonOCN    (1:sNx,1:sNy)
      _RL d_HEFFbyATMonOCN    (1:sNx,1:sNy)
      _RL d_HEFFfromSNWflood  (1:sNx,1:sNy)
c
      _RL d_HSNWfromFRW    (1:sNx,1:sNy)
      _RL d_HSNWbyOCNonSNW    (1:sNx,1:sNy)
      _RL d_SNWintoICEflood  (1:sNx,1:sNy)
      _RL d_HFRWfromSNW    (1:sNx,1:sNy)

C     actual ice thickness with upper and lower limit
      _RL HICE          (1:sNx,1:sNy)
C     actual snow thickness
      _RL hSnwLoc       (1:sNx,1:sNy)
C     wind speed
      _RL UG            (1:sNx,1:sNy)
      _RL SPEED_SQ
C     local copy of AREA
      _RL areaLoc

c temporary variables available for the various computations
      _RL tmpscal1, tmpscal2, tmpscal3, tmpscal4
      _RL tmparr1         (1:sNx,1:sNy)

C auxillary variables used for specific processes
      _RL snowEnergy

#ifdef ALLOW_SEAICE_FLOODING
      _RL hDraft
#endif /* ALLOW_SEAICE_FLOODING */
      
#ifdef SEAICE_SALINITY
      _RL saltFluxAdjust(1:sNx,1:sNy)
#endif

#ifdef SEAICE_MULTICATEGORY
      INTEGER it
      INTEGER ilockey
      _RL RK
      _RL HICEP         (1:sNx,1:sNy)
      _RL a_QbyATMmult_cover         (1:sNx,1:sNy)
      _RL a_QSWbyATMmult_cover         (1:sNx,1:sNy)
#endif

#ifdef SEAICE_AGE
C     old_AREA :: hold sea-ice fraction field before any seaice-thermo update
      _RL old_AREA     (1:sNx,1:sNy)
# ifdef SEAICE_AGE_VOL
C     old_HEFF :: hold sea-ice effective thickness field before any seaice-thermo update
      _RL old_HEFF     (1:sNx,1:sNy)
      _RL age_actual
# endif /* SEAICE_AGE_VOL */
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
          a_QbyATM(I,J)      = 0.0 _d 0
          r_QbyATM_cover (I,J)      = 0.0 _d 0
c
          a_QSWbyATM_open (I,J)      = 0.0 _d 0
          a_QSWbyATM_cover (I,J)      = 0.0 _d 0
c
          a_QbyICE (I,J)      = 0.0 _d 0
          r_QbyICE (I,J)      = 0.0 _d 0
c
          FRWfromSNW (I,J)    = 0
c
          d_QbyICE (I,J)      = 0.0 _d 0
          d_QbySNW (I,J)      = 0.0 _d 0
          d_QbyATMonOCN (I,J)      = 0.0 _d 0
          d_QbyATMonSNW (I,J)      = 0.0 _d 0
c
          d_AREAbyATM(I,J)      = 0.0 _d 0
c
          d_HEFFbyICEonOCN(I,J)      = 0.0 _d 0
          d_HEFFbyATMonOCN(I,J)      = 0.0 _d 0
          d_HEFFfromSNWflood(I,J)      = 0.0 _d 0
c
          d_HSNWfromFRW(I,J)      = 0.0 _d 0
          d_HSNWbyOCNonSNW(I,J)      = 0.0 _d 0
          d_SNWintoICEflood(I,J)      = 0.0 _d 0
          d_HFRWfromSNW(I,J)      = 0.0 _d 0
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
# ifdef SEAICE_AGE_VOL
           old_HEFF(i,j) = HEFF(I,J,bi,bj)
# endif
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


C determine available heat due to the atmosphere -- for open water
C ================================================================

C ocean surface/mixed layer temperature
        DO J=1,sNy
         DO I=1,sNx
          TMIX(I,J,bi,bj)=theta(I,J,kSurface,bi,bj)+273.16 _d +00
#ifdef SEAICE_DEBUG
          TMIX(I,J,bi,bj)=MAX(TMIX(I,J,bi,bj),271.2 _d +00)
#endif
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
           HICEP(I,J)=(HICE(I,J)/MULTDIM)*((2.0 _d 0*RK)-1.0 _d 0)
           TICE(I,J,bi,bj)=TICES(I,J,IT,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_SOLVE4TEMP(
     I        UG, HICEP, hSnwLoc,
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
     I       UG, HICE, hSnwLoc,
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
     &           a_QbyATM_cover(I,J) * areaNm1(I,J,bi,bj) +
     &           a_QbyATM_open(I,J) * ( ONE - areaNm1(I,J,bi,bj) ) )
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIatmQnt',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif


C determine whether the atmostpheric conditions are prone 
C to generate SNOW (FRWfromSNW=1) or fresh water (FRWfromSNW=2)
C ==============================================================

        DO J=1,sNy
         DO I=1,sNx
          IF (a_QbyATM_cover(I,J).LT.ZERO.AND.
     &        AREANm1(I,J,bi,bj).GT.ZERO) THEN
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

cgf a_QbyICE: available heat that may be extracted by the ICE acting 
cgf on the OCN surface/mixed layer to bring it back to freezig point 
cgf (in m, negative out of the ocean -- different convention than a_QbyATM)
cgf
cgf Using the same convention (W/m2, positive out of the ocean) as 
cgf for the other heat stocks breaks the lab_sea test at this point.
cgf We should revise these conventions in a later revision though.
c
        DO J=1,sNy
         DO I=1,sNx
          IF ( .NOT. inAdMode ) THEN
#ifdef SEAICE_VARIABLE_FREEZING_POINT
           TBC = -0.0575 _d 0*salt(I,J,kSurface,bi,bj) + 0.0901 _d 0
#endif /* SEAICE_VARIABLE_FREEZING_POINT */
           IF ( theta(I,J,kSurface,bi,bj) .GE. TBC ) THEN
              a_QbyICE(i,j) = SEAICE_availHeatFrac
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) / 72.0764 _d 0 
#ifdef SEAICE_QBYICE_IS_WPERM2
c using W/m2 as the Q unit
     &             * ( - QI / SEAICE_deltaTtherm )
#endif
           ELSE
              a_QbyICE(i,j) = SEAICE_availHeatFracFrz
     &             * (theta(I,J,kSurface,bi,bj)-TBC) * dRf(kSurface)
     &             * hFacC(i,j,kSurface,bi,bj) / 72.0764 _d 0
#ifdef SEAICE_QBYICE_IS_WPERM2
     &             * ( - QI / SEAICE_deltaTtherm )
#endif
           ENDIF
          ELSE
           a_QbyICE(i,j) = 0.
          ENDIF
         ENDDO
        ENDDO


C compute ice thickness tendency due to ice-ocean interaction
C ===========================================================

        DO J=1,sNy
         DO I=1,sNx
         tmpscal1=a_QbyICE(i,j)
#ifdef SEAICE_QBYICE_IS_WPERM2
     &             / ( - QI / SEAICE_deltaTtherm )
#endif
          d_HEFFbyICEonOCN(I,J) = 
     &     MAX(ZERO, HEFF(I,J,bi,bj)-tmpscal1)- HEFF(I,J,bi,bj)
          d_QbyICE(I,J)=d_HEFFbyICEonOCN(I,J)
#ifdef SEAICE_QBYICE_IS_WPERM2
     &             * ( - QI / SEAICE_deltaTtherm )
#endif
c apply tendency
          r_QbyICE(I,J)=a_QbyICE(I,J)+d_QbyICE(I,J)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj) + d_HEFFbyICEonOCN(I,J)
          saltWtrIce(I,J,bi,bj)   = saltWtrIce(I,J,bi,bj)
     &                              + d_HEFFbyICEonOCN(I,J)
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIyneg  ',myThid) ) THEN
          CALL DIAGNOSTICS_FILL(d_HEFFbyICEonOCN,
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
          tmpscal1=-SEAICE_deltaTtherm*
     &                a_QbyATM_cover(I,J)*AREANm1(I,J,bi,bj)
          IF ( FRWfromSNW(I,J).EQ.2 ) THEN
           snowEnergy=HSNOW(I,J,bi,bj)*QS
           IF(tmpscal1.LE.snowEnergy) THEN
C not enough heat to melt all snow; use up all of a_QbyATM_cover
            d_HSNWfromFRW(I,J)=-tmpscal1/QS
C SNOW CONVERTED INTO WATER AND THEN INTO equivalent m of ICE melt
C The factor 1/ICE2SNOW converts m of snow to m of sea-ice
            d_HFRWfromSNW(I,J)= - tmpscal1/(QS*ICE2SNOW)
            d_QbyATMonSNW(I,J) = -a_QbyATM_cover(I,J)
           ELSE
C enough heat to melt snow completely;
C compute remaining heat that will melt ice
            d_QbyATMonSNW(I,J)=-(tmpscal1-snowEnergy)/
     &        SEAICE_deltaTtherm/AREANm1(I,J,bi,bj)-a_QbyATM_cover(I,J)
C convert all snow to melt water (fresh water flux)
            d_HFRWfromSNW(I,J)=-HSNOW(I,J,bi,bj)/ICE2SNOW
            d_HSNWfromFRW(I,J)=-HSNOW(I,J,bi,bj)
           END IF
          ELSE 
c this process is inactive under cold conditions
            d_QbyATMonSNW(I,J)=0. _d 0
            d_HFRWfromSNW(I,J)=0. _d 0
            d_HSNWfromFRW(I,J)=0. _d 0
          ENDIF
c apply tendency
          frWtrIce(I,J,bi,bj) = frWtrIce(I,J,bi,bj) + 
     &                         d_HFRWfromSNW(I,J) 
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWfromFRW(I,J) 
          a_QbyATM_cover(I,J)= a_QbyATM_cover(I,J) + d_QbyATMonSNW(I,J)
         ENDDO
        ENDDO


C compute heat due to the atmosphere that 
C remain available after melt/freeze processes
C ============================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE a_QbyATM_cover(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1,sNy
         DO I=1,sNx
          a_QbyATM(I,J)= a_QbyATM_cover(I,J) * AREANm1(I,J,bi,bj)
     &              + a_QbyATM_open(I,J) * (ONE-AREANm1(I,J,bi,bj))
         ENDDO
        ENDDO


C compute ice cover fraction tendency from a_QbyATM
C =================================================

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)   = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QbyATM_cover(:,:)         = comlev1_bibj,
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE a_QbyATM(:,:)        = comlev1_bibj,
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
          tmpscal1  = -SEAICE_deltaTtherm*a_QbyATM(I,J)*recip_QI
c (cannot melt more than all the ice)
          tmpscal2 = -ONE*MIN(HEFF(I,J,bi,bj),tmpscal1)
          tmpscal3 = MIN(ZERO,tmpscal2)
#ifdef ALLOW_DIAGNOSTICS
          DIAGarray(I,J) = tmpscal2
#endif
C gain of new ice over open water (>0 by definition)
          tmpscal4 = MAX(ZERO,SEAICE_deltaTtherm*
     &               a_QbyATM_open(I,J)*recip_QI)
c compute cover fraction tendency
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           d_AREAbyATM(I,J)=
     &          (ONE-AREANm1(I,J,bi,bj))*tmpscal4/HO_south
     &          +HALF*tmpscal3*AREANm1(I,J,bi,bj)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
          ELSE
           d_AREAbyATM(I,J)=
     &          (ONE-AREANm1(I,J,bi,bj))*tmpscal4/HO
     &          +HALF*tmpscal3*AREANm1(I,J,bi,bj)
     &          /(HEFF(I,J,bi,bj)+.00001 _d 0)
          ENDIF
c apply tendency
           AREA(I,J,bi,bj)=AREA(I,J,bi,bj)+d_AREAbyATM(I,J)
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

        DO J=1,sNy
         DO I=1,sNx
          tmpscal1       = SEAICE_deltaTtherm*
     &      a_QbyATM_cover(I,J)*recip_QI*AREANm1(I,J,bi,bj)
C (cannot melt more than all the ice)
          tmpscal2      = MAX(-HEFF(I,J,bi,bj),tmpscal1)
          d_HEFFbyATMonOCN(I,J)=tmpscal2
C compute the r_QbyATM_cover residual as the difference between
C the available heat tmpscal1 and the used tmpscal2;
          d_QbyATMonOCN(I,J)=(tmpscal2 - tmpscal1)
     &                *QI/SEAICE_deltaTtherm-a_QbyATM_cover(I,J)
c apply tendency
          r_QbyATM_cover(I,J) = a_QbyATM_cover(I,J)+d_QbyATMonOCN(I,J)
          HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) + d_HEFFbyATMonOCN(I,J)
          saltWtrIce(I,J,bi,bj) = saltWtrIce(I,J,bi,bj) + tmpscal2
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
            d_HFRWfromSNW(I,J)=0. _d 0
            d_HSNWfromFRW(I,J)=SEAICE_deltaTtherm*
     &            PRECIP(I,J,bi,bj)*AREANm1(I,J,bi,bj)*SDF
          ELSE 
c           add precip to the fresh water bucket
            d_HFRWfromSNW(I,J)=-PRECIP(I,J,bi,bj)*AREANm1(I,J,bi,bj)*
     &            SEAICE_deltaTtherm/ICE2WATR
            d_HSNWfromFRW(I,J)=0. _d 0
          ENDIF
c apply tendency
          frWtrIce(I,J,bi,bj) = frWtrIce(I,J,bi,bj) + 
     &                         d_HFRWfromSNW(I,J) 
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj) + d_HSNWfromFRW(I,J) 
         ENDDO
        ENDDO
#endif /* ALLOW_ATM_TEMP */


C compute snow tendency due to heat available from atmosphere
C can only reduce HSNOW. melted snow is added to fresh water stock.
C =================================================================

cph( very sensitive bit here by JZ
#ifndef SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
        DO J=1,sNy
         DO I=1,sNx
         tmpscal1=r_QbyICE(i,j)
#ifdef SEAICE_QBYICE_IS_WPERM2
     &             / ( - QI / SEAICE_deltaTtherm )
#endif
          IF( tmpscal1 .GT. ZERO .AND.
     &       HSNOW(I,J,bi,bj) .GT. ZERO ) THEN
           d_HSNWbyOCNonSNW(I,J) = 
     &       - MIN( HSNOW(I,J,bi,bj)/SDF/ICE2WATR , tmpscal1 )
          ELSE
           d_HSNWbyOCNonSNW(I,J)       = 0. _d 0
          ENDIF
          d_QbySNW(I,J)=d_HSNWbyOCNonSNW(I,J)
#ifdef SEAICE_QBYICE_IS_WPERM2
     &             * ( - QI / SEAICE_deltaTtherm )
#endif
c apply tendency
          r_QbyICE(I,J)=a_QbyICE(I,J)+d_QbySNW(I,J)
          HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)
     &        +d_HSNWbyOCNonSNW(I,J)*SDF*ICE2WATR
          frWtrIce(I,J,bi,bj) = frWtrIce(I,J,bi,bj) 
     &        +d_HSNWbyOCNonSNW(I,J)
         ENDDO
        ENDDO
#endif /* SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING */
cph)


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
C ==================================================

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
C if SaltPlumeSouthernOcean=.FALSE. turn off salt plume in Southern Ocean
             IF ( .NOT. SaltPlumeSouthernOcean ) THEN
              IF ( YC(I,J,bi,bj) .LT. 0.0 _d 0 )
     &             saltPlumeFlux(i,j,bi,bj) = 0.0 _d 0
             ENDIF

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


C compute net heat flux leaving/entering the ocean, 
C accounting for the part used in melt/freeze processes
C =====================================================

        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj) = r_QbyATM_cover(I,J) * AREANm1(I,J,bi,bj)
     &        +a_QbyATM_open(I,J) * (ONE-AREANm1(I,J,bi,bj))
          QSW(I,J,bi,bj)  = a_QSWbyATM_cover(I,J) * AREANm1(I,J,bi,bj)
     &        +a_QSWbyATM_open(I,J)  * (ONE-AREANm1(I,J,bi,bj))
         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         IF ( DIAGNOSTICS_IS_ON('SIqneto ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = a_QbyATM_open(I,J) * 
     &         (ONE-areaNm1(I,J,bi,bj))
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneto ',0,1,3,bi,bj,myThid)
         ENDIF
         IF ( DIAGNOSTICS_IS_ON('SIqneti ',myThid) ) THEN
          DO J=1,sNy
           DO I=1,sNx
            DIAGarray(I,J) = r_QbyATM_cover(I,J) * areaNm1(I,J,bi,bj)
           ENDDO
          ENDDO
          CALL DIAGNOSTICS_FILL(DIAGarray,'SIqneti ',0,1,3,bi,bj,myThid)
         ENDIF
        ENDIF
#endif


C account for contribution due to ocean-ice interaction.
C ======================================================

        DO J=1,sNy
         DO I=1,sNx
#ifdef SEAICE_QBYICE_IS_WPERM2
          tmpscal1=
     &         -maskC(I,J,kSurface,bi,bj)
     &         *(HeatCapacity_Cp*rUnit2mass/QI)
     &         *72.0764 _d 0
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)
     &         - ( d_QbyICE(I,J) + d_QbySNW(I,J) )
     &         * tmpscal1
#else
          tmpscal1 = 
     &       - ( d_HEFFbyICEonOCN(I,J)+d_HSNWbyOCNonSNW(I,J) )
     &       *recip_dRf(kSurface)*recip_hFacC(i,j,kSurface,bi,bj)
     &       *72.0764 _d 0
          QNET(I,J,bi,bj) = QNET(I,J,bi,bj)
     &         +tmpscal1
     &         /SEAICE_deltaTtherm*maskC(I,J,kSurface,bi,bj)
     &         *HeatCapacity_Cp*rUnit2mass
     &         *drF(kSurface)*hFacC(i,j,kSurface,bi,bj)
#endif

         ENDDO
        ENDDO

#ifdef SEAICE_DEBUG
       CALL PLOT_FIELD_XYRL( QSW,'Current QSW ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( QNET,'Current QNET ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( EmPmR,'Current EmPmR ', myIter, myThid )
#endif /* SEAICE_DEBUG */


C treat values of ice cover fraction oustide 
C the [0 1] range, and other such issues.
C ===========================================

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
            tmparr1(I,J) = (HEFF(I,J,bi,bj)-HEFFNm1(I,J,bi,bj))
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
     &              +HEFF(I,J,bi,bj)*SEAICE_rhoIce)/1000. _d 0
           tmparr1(I,J) = hDraft - MIN(hDraft,HEFF(I,J,bi,bj))
C
           d_HEFFfromSNWflood(I,J)=tmparr1(I,J)
           d_SNWintoICEflood(I,J)=MAX(0. _d 0, HSNOW(I,J,bi,bj)
     &      -tmparr1(I,J)*ICE2SNOW )   - HSNOW(I,J,bi,bj)
c apply tendency
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj)+d_HEFFfromSNWflood(I,J)
           HSNOW(I,J,bi,bj) = HSNOW(I,J,bi,bj)+d_SNWintoICEflood(I,J)
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          IF ( DIAGNOSTICS_IS_ON('SIsnwice',myThid) ) THEN
C     turn tmparr1 into a rate
           DO J=1,sNy
            DO I=1,sNx
             tmparr1(I,J) = tmparr1(I,J)/SEAICE_deltaTtherm
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(tmparr1,'SIsnwice',0,1,3,bi,bj,myThid)
          ENDIF
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */
        ENDIF
#endif /* ALLOW_SEAICE_FLOODING */


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
# else /* ifdef SEAICE_AGE_VOL */
C     Sources and sinks for sea ice age:
C     assume that a) freezing: new ice volume forms with zero age
C                 b) melting: ice volume vanishes with current age
        DO J=1,sNy
         DO I=1,sNx
C--   compute actual age from effective age:
          IF (OLD_AREA(i,j).GT.0. _d 0) THEN
           age_actual=IceAge(i,j,bi,bj)/OLD_AREA(i,j)
          ELSE
           age_actual=0. _d 0
          ENDIF         
          IF ( (OLD_HEFF(i,j).LT.HEFF(i,j,bi,bj)).AND.
     &         (AREA(i,j,bi,bj).GT.0.15) ) THEN
           age_actual=age_actual*OLD_HEFF(i,j)/
     &          HEFF(i,j,bi,bj)+SEAICE_deltaTtherm
          ELSEIF (AREA(i,j,bi,bj).LE.0.15) THEN
           age_actual=0. _d 0
          ELSE
           age_actual=age_actual+SEAICE_deltaTtherm
          ENDIF
C--   re-scale to effective age:
          IceAge(i,j,bi,bj) = age_actual*AREA(i,j,bi,bj)
         ENDDO
        ENDDO
# endif /* SEAICE_AGE_VOL */
#endif /* SEAICE_AGE */

       ENDDO
      ENDDO

      RETURN
      END
