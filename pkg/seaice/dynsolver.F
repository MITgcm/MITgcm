C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE dynsolver( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE dynsolver                                     |
C     | o Ice dynamics solver                                    |
C     |  See Zhang and Hibler,   JGR, 102, 8691-8702, 1997       |
C     |      Zhang and Rothrock, JGR, 105, 3325-3338, 2000       |
C     |  and Hibler,             JPO,   9,  815- 846, 1979       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_GRID.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_FFIELDS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj, kii
      _RL DWAT, DAIR, RHOICE, RHOAIR, SINWIN, COSWIN, SINWAT, COSWAT
      _RL GRAV, ECCEN, ECM2, GMIN, RADIUS, DELT1, DELT2, PSTAR, AAA
      _RL TEMPVAR

      _RL PRESS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DAIRN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DWATN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FORCEX0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL FORCEY0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL E11        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL E22        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL E12        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL COR_ICE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL ZMAX       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL ZMIN       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)

C--   FIRST SET UP BASIC CONSTANTS
      DWAT=0.59 _d 0
      DAIR=0.01462 _d 0
      RHOICE=0.91 _d +03
      RHOAIR=1.3 _d 0
      GRAV=9.832 _d 0
      ECCEN=TWO
      ECM2=ONE/(ECCEN**2)
      GMIN=1.0 _d -20
      RADIUS=6370. _d 3
      PSTAR=SEAICE_strength

C--   25 DEG GIVES SIN EQUAL TO 0.4226
      SINWIN=0.4226 _d 0
      COSWIN=0.9063 _d 0
      SINWAT=0.4226 _d 0
      COSWAT=0.9063 _d 0

C--   Do not introduce turning angle
      SINWIN=ZERO
      COSWIN=ONE
      SINWAT=ZERO
      COSWAT=ONE

C--   NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          AMASS(I,J,bi,bj)=RHOICE*QUART*(HEFF(i,j,1,bi,bj)
     &           +HEFF(i-1,j,1,bi,bj)
     &           +HEFF(i,j-1,1,bi,bj)
     &           +HEFF(i-1,j-1,1,bi,bj))
          COR_ICE(I,J,bi,bj)=AMASS(I,J,bi,bj)
     &         *TWO*OMEGA*SINEICE(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   NOW SET UP FORCING FIELDS

      IF (SEAICEwindOnCgrid) THEN
C--   Wind stress computed here from wind on South-West C-grid
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           AAA=(HALF*(UWIND(I,J,bi,bj)+UWIND(I,J-1,bi,bj)))**2+
     &         (HALF*(VWIND(I,J,bi,bj)+VWIND(I-1,J,bi,bj)))**2
           IF ( AAA .LT. SEAICE_EPS_SQ ) THEN
              AAA=SEAICE_EPS
           ELSE
              AAA=SQRT(AAA)
           ENDIF
           DAIRN(I,J,bi,bj)=RHOAIR*SEAICE_drag*
     &          (2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
           FORCEX(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
     &          (COSWIN*HALF*(UWIND(I,J,bi,bj)+UWIND(I,J-1,bi,bj))
     &          -SINWIN*HALF*(VWIND(I,J,bi,bj)+VWIND(I-1,J,bi,bj)))
           FORCEY(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
     &          (SINWIN*HALF*(UWIND(I,J,bi,bj)+UWIND(I,J-1,bi,bj))
     &          +COSWIN*HALF*(VWIND(I,J,bi,bj)+VWIND(I-1,J,bi,bj)))
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSE
C--   Wind stress computed here from wind on South-West B-grid
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           AAA=UWIND(I,J,bi,bj)**2+VWIND(I,J,bi,bj)**2
           IF ( AAA .LT. SEAICE_EPS_SQ ) THEN
              AAA=SEAICE_EPS
           ELSE
              AAA=SQRT(AAA)
           ENDIF
           DAIRN(I,J,bi,bj)=RHOAIR*SEAICE_drag*
     &          (2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
           FORCEX(I,J,bi,bj)=DAIRN(I,J,bi,bj)*(COSWIN*UWIND(I,J,bi,bj)
     &          -SINWIN*VWIND(I,J,bi,bj))
           FORCEY(I,J,bi,bj)=DAIRN(I,J,bi,bj)*(SINWIN*UWIND(I,J,bi,bj)
     &          +COSWIN*VWIND(I,J,bi,bj))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C--   STORE WIND ONLY STRESS 
          WINDX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
          WINDY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
C--   NOW ADD IN TILT
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &         -COR_ICE(I,J,bi,bj)*GWATY(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &         +COR_ICE(I,J,bi,bj)*GWATX(I,J,bi,bj)
C--   NOW SET UP ICE PRESSURE AND VISCOSITIES
          PRESS(I,J,bi,bj)=PSTAR*HEFF(I,J,1,bi,bj)
     &         *EXP(-20.0 _d 0*(ONE-AREA(I,J,1,bi,bj)))
          ZMAX(I,J,bi,bj)=(5.0 _d +12/(2.0 _d +04))*PRESS(I,J,bi,bj)
          ZMIN(I,J,bi,bj)=4.0 _d +08
          PRESS(I,J,bi,bj)=PRESS(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS ) THEN

C--   Update overlap regions for PRESS
      _EXCH_XY_R8(PRESS, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C NOW CALCULATE PRESSURE FORCE AND ADD TO EXTERNAL FORCE
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &          -(QUART/(DXUICE(I,J,bi,bj)*CSUICE(I,J,bi,bj)))
     &          *(PRESS(I,J,bi,bj)+PRESS(I,J-1,bi,bj)
     &          -PRESS(I-1,J,bi,bj)-PRESS(I-1,J-1,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)-QUART/DYUICE(I,J,bi,bj)
     &          *(PRESS(I,J,bi,bj)+PRESS(I-1,J,bi,bj)
     &          -PRESS(I,J-1,bi,bj)-PRESS(I-1,J-1,bi,bj))
C NOW KEEP FORCEX0
          FORCEX0(I,J,bi,bj)=FORCEX(I,J,bi,bj)
          FORCEY0(I,J,bi,bj)=FORCEY(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C DO PSEUDO-TIMESTEPS TO OBTAIN AN ACCURATE VISCOUS-PLASTIC SOLUTION
C A RANGE OF 5-300 PSEUDO-TIMESTEPS IS SUGGESTED DEPENDING ON ACCURACY
C REQUIREMENTS OF SPECIFIC APPLICATION
C NPSEUDO is now set in data.seaice input file
C TIMESTEP FOR PSEUDO-TIMESTEPPING
      SEAICE_DT = DELTAT/NPSEUDO

crg what about DWAIN,DRAGS,DRAGA,ETA,ZETA

crg later c$taf loop = iteration uice,vice

      DO 5000 KII=1,NPSEUDO

cdmc$taf store uice,vice = comlev1_seaice_ds,
cdmc$taf&                key = kii + (ikey_dynamics-1)*NPSEUDO

C NOW DO PREDICTOR TIME STEP
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(I,J,2,bi,bj)=UICE(I,J,1,bi,bj)
          VICE(I,J,2,bi,bj)=VICE(I,J,1,bi,bj)
          UICEC(I,J,bi,bj)=UICE(I,J,1,bi,bj)
          VICEC(I,J,bi,bj)=VICE(I,J,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C NOW SET UP NON-LINEAR WATER DRAG, FORCEX, FORCEY
          TEMPVAR=(UICE(I,J,1,bi,bj)-GWATX(I,J,bi,bj))**2
     &           +(VICE(I,J,1,bi,bj)-GWATY(I,J,bi,bj))**2
          IF ( TEMPVAR .LT. SEAICE_EPS_SQ ) THEN
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SEAICE_EPS
          ELSE
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
          ENDIF
          DWATN(I,J,bi,bj)=MAX(DWATN(I,J,bi,bj),QUART) 
C NOW SET UP SYMMETTRIC DRAG
          DRAGS(I,J,bi,bj)=DWATN(I,J,bi,bj)*COSWAT
C NOW SET UP ANTI SYMMETTRIC DRAG PLUS CORIOLIS
          DRAGA(I,J,bi,bj)=DWATN(I,J,bi,bj)*SINWAT+COR_ICE(I,J,bi,bj)
C NOW ADD IN CURRENT FORCE
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &                     *(COSWAT*GWATX(I,J,bi,bj)
     &                     -SINWAT*GWATY(I,J,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &                     *(SINWAT*GWATX(I,J,bi,bj)
     &                     +COSWAT*GWATY(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C NOW EVALUATE STRAIN RATES
          E11(I,J,bi,bj)=HALF/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(UICE(I+1,J+1,1,bi,bj)+UICE(I+1,J,1,bi,bj)
     &                  -UICE(I,J+1,1,bi,bj)-UICE(I,J,1,bi,bj))
     &                  -QUART*(VICE(I+1,J+1,1,bi,bj)
     &                  +VICE(I,J+1,1,bi,bj)
     &                  +VICE(I,J,1,bi,bj)+VICE(I+1,J,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS
          E22(I,J,bi,bj)=HALF/DYTICE(I,J,bi,bj)
     &                  *(VICE(I+1,J+1,1,bi,bj)+VICE(I,J+1,1,bi,bj)
     &                  -VICE(I+1,J,1,bi,bj)-VICE(I,J,1,bi,bj))
          E12(I,J,bi,bj)=HALF*(HALF/DYTICE(I,J,bi,bj)
     &                  *(UICE(I+1,J+1,1,bi,bj)+UICE(I,J+1,1,bi,bj)
     &                  -UICE(I+1,J,1,bi,bj)-UICE(I,J,1,bi,bj))
     &                  +HALF/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(VICE(I+1,J+1,1,bi,bj)+VICE(I+1,J,1,bi,bj)
     &                  -VICE(I,J+1,1,bi,bj)-VICE(I,J,1,bi,bj))
     &                  +QUART*(UICE(I+1,J+1,1,bi,bj)
     &                  +UICE(I,J+1,1,bi,bj)
     &                  +UICE(I,J,1,bi,bj)+UICE(I+1,J,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS)
C  NOW EVALUATE VISCOSITIES
          DELT1=(E11(I,J,bi,bj)**2+E22(I,J,bi,bj)**2)*(ONE+ECM2)
     &        +4.0 _d 0*ECM2*E12(I,J,bi,bj)**2
     1        +TWO*E11(I,J,bi,bj)*E22(I,J,bi,bj)*(ONE-ECM2)
          IF ( DELT1 .LT. SEAICE_EPS_SQ ) THEN
             DELT2=SEAICE_EPS
          ELSE
             DELT2=SQRT(DELT1)
          ENDIF
          DELT2=MAX(GMIN,DELT2)
          ZETA(I,J,bi,bj)=HALF*PRESS(I,J,bi,bj)/DELT2
C NOW PUT MIN AND MAX VISCOSITIES IN
          ZETA(I,J,bi,bj)=MIN(ZMAX(I,J,bi,bj),ZETA(I,J,bi,bj))
          ZETA(I,J,bi,bj)=MAX(ZMIN(I,J,bi,bj),ZETA(I,J,bi,bj))
C NOW SET VISCOSITIES TO ZERO AT HEFFMFLOW PTS 
          ZETA(I,J,bi,bj)=ZETA(I,J,bi,bj)*HEFFM(I,J,bi,bj) 
          ETA(I,J,bi,bj)=ECM2*ZETA(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(ETA, myThid)
      _EXCH_XY_R8(ZETA, myThid)

C NOW ADI SCHEME (ZHANG-J/ROTHROCK 1999,bi,bj)
#ifdef ALLOW_AUTODIFF_TAMC
      CALL ADI( myThid )
#else /* ALLOW_AUTODIFF_TAMC */
      IF ( SEAICEuseLSR ) THEN
         CALL LSR( myThid )
      ELSE
         CALL ADI( myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW DO MODIFIED EULER STEP
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(I,J,1,bi,bj)=HALF*(UICE(I,J,1,bi,bj)+UICE(I,J,2,bi,bj))
          VICE(I,J,1,bi,bj)=HALF*(VICE(I,J,1,bi,bj)+VICE(I,J,2,bi,bj))
          UICEC(I,J,bi,bj)=UICE(I,J,1,bi,bj)
          VICEC(I,J,bi,bj)=VICE(I,J,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C NOW SET UP NON-LINEAR WATER DRAG
          TEMPVAR=(UICE(I,J,1,bi,bj)-GWATX(I,J,bi,bj))**2
     &           +(VICE(I,J,1,bi,bj)-GWATY(I,J,bi,bj))**2
          IF ( TEMPVAR .LT. SEAICE_EPS_SQ ) THEN
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SEAICE_EPS
          ELSE
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
          ENDIF
          DWATN(I,J,bi,bj)=MAX(DWATN(I,J,bi,bj),QUART) 
C NOW SET UP SYMMETTRIC DRAG
          DRAGS(I,J,bi,bj)=DWATN(I,J,bi,bj)*COSWAT
C NOW SET UP ANTI SYMMETTRIC DRAG PLUS CORIOLIS
          DRAGA(I,J,bi,bj)=DWATN(I,J,bi,bj)*SINWAT+COR_ICE(I,J,bi,bj)
C NOW ADD IN CURRENT FORCE
          FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &                     *(COSWAT*GWATX(I,J,bi,bj)
     &                     -SINWAT*GWATY(I,J,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+DWATN(I,J,bi,bj)
     &                     *(SINWAT*GWATX(I,J,bi,bj)
     &                     +COSWAT*GWATY(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
C NOW EVALUATE STRAIN RATES
          E11(I,J,bi,bj)=HALF/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(UICE(I+1,J+1,1,bi,bj)+UICE(I+1,J,1,bi,bj)
     &                  -UICE(I,J+1,1,bi,bj)-UICE(I,J,1,bi,bj))
     &                  -QUART*(VICE(I+1,J+1,1,bi,bj)
     &                  +VICE(I,J+1,1,bi,bj)
     &                  +VICE(I,J,1,bi,bj)+VICE(I+1,J,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS
          E22(I,J,bi,bj)=HALF/DYTICE(I,J,bi,bj)
     &                  *(VICE(I+1,J+1,1,bi,bj)+VICE(I,J+1,1,bi,bj)
     &                  -VICE(I+1,J,1,bi,bj)-VICE(I,J,1,bi,bj))
          E12(I,J,bi,bj)=HALF*(HALF/DYTICE(I,J,bi,bj)
     &                  *(UICE(I+1,J+1,1,bi,bj)+UICE(I,J+1,1,bi,bj)
     &                  -UICE(I+1,J,1,bi,bj)-UICE(I,J,1,bi,bj))
     &                  +HALF/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(VICE(I+1,J+1,1,bi,bj)+VICE(I+1,J,1,bi,bj)
     &                  -VICE(I,J+1,1,bi,bj)-VICE(I,J,1,bi,bj))
     &                  +QUART*(UICE(I+1,J+1,1,bi,bj)
     &                  +UICE(I,J+1,1,bi,bj)
     &                  +UICE(I,J,1,bi,bj)+UICE(I+1,J,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS)
C  NOW EVALUATE VISCOSITIES
          DELT1=(E11(I,J,bi,bj)**2+E22(I,J,bi,bj)**2)*(ONE+ECM2)
     &        +4. _d 0*ECM2*E12(I,J,bi,bj)**2
     1        +TWO*E11(I,J,bi,bj)*E22(I,J,bi,bj)*(ONE-ECM2)
          IF ( DELT1 .LT. SEAICE_EPS_SQ ) THEN
             DELT2=SEAICE_EPS
          ELSE
             DELT2=SQRT(DELT1)
          ENDIF
          DELT2=MAX(GMIN,DELT2)
          ZETA(I,J,bi,bj)=HALF*PRESS(I,J,bi,bj)/DELT2
C NOW PUT MIN AND MAX VISCOSITIES IN
          ZETA(I,J,bi,bj)=MIN(ZMAX(I,J,bi,bj),ZETA(I,J,bi,bj))
          ZETA(I,J,bi,bj)=MAX(ZMIN(I,J,bi,bj),ZETA(I,J,bi,bj))
C NOW SET VISCOSITIES TO ZERO AT HEFFMFLOW PTS 
          ZETA(I,J,bi,bj)=ZETA(I,J,bi,bj)*HEFFM(I,J,bi,bj) 
          ETA(I,J,bi,bj)=ECM2*ZETA(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(ETA, myThid)
      _EXCH_XY_R8(ZETA, myThid)

C GET READY FOR SECOND CALL OF ADI
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(I,J,2,bi,bj)=UICEC(I,J,bi,bj)
          VICE(I,J,2,bi,bj)=VICEC(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C NOW ADI SCHEME (ZHANG-J/ROTHROCK 1999)
#ifdef ALLOW_AUTODIFF_TAMC
      CALL ADI( myThid )
#else /* ALLOW_AUTODIFF_TAMC */
      IF ( SEAICEuseLSR ) THEN
         CALL LSR( myThid )
      ELSE
         CALL ADI( myThid )
      ENDIF
#endif /* ALLOW_AUTODIFF_TAMC */

5000  CONTINUE

cdm c$taf store uice,vice = comlev1, key=ikey_dynamics

      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

C Calculate ocean surface stress
      CALL OSTRES ( DWATN, COR_ICE, myThid )

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS ) THEN

c Put a cap on ice velocity
c limit velocity to 0.40 m s-1 to avoid potential CFL violations
c in open water areas (drift of zero thickness ice)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef SEAICE_DEBUG
c          write(*,'(2i4,2i2,f7.1,7f12.3)')
c     &      i,j,bi,bj,UVM(I,J,bi,bj),amass(i,j,bi,bj)
c     &     ,gwatx(I,J,bi,bj),gwaty(i,j,bi,bj)
c     &     ,forcex(I,J,bi,bj),forcey(i,j,bi,bj)
c     &     ,uice(i,j,1,bi,bj)
c     &     ,vice(i,j,1,bi,bj)
#endif /* SEAICE_DEBUG */
          UICE(i,j,1,bi,bj)=min(UICE(i,j,1,bi,bj),0.40 _d +00)
          VICE(i,j,1,bi,bj)=min(VICE(i,j,1,bi,bj),0.40 _d +00)
          UICE(i,j,1,bi,bj)=max(UICE(i,j,1,bi,bj),-0.40 _d +00)
          VICE(i,j,1,bi,bj)=max(VICE(i,j,1,bi,bj),-0.40 _d +00)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

#endif /* ALLOW_SEAICE */

      RETURN
      END
