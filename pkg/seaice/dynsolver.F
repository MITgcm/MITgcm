C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE dynsolver( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE dynsolver                                     |
C     | o Ice dynamics solver                                    |
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
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj, kii
      _RL  DWAT, DAIR, RHOICE, RHOAIR, SINWIN, COSWIN, SINWAT, COSWAT
      _RL  GRAV, ECCEN, ECM2, GMIN, RADIUS, DELT1, DELT2, PSTAR
      _RL  AAA

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

C FIRST SET UP BASIC CONSTANTS
      DWAT=0.59
      DAIR=0.01462
      RHOICE=0.91D+03
      RHOAIR=1.3
C   25 DEG GIVES SIN EQUAL TO 0.4226
      SINWIN=0.4226
      COSWIN=0.9063
      SINWAT=0.4226
      COSWAT=0.9063
c do not introduce truning angle
      SINWIN=0.0
      COSWIN=1.0
      SINWAT=0.0
      COSWAT=1.0

      GRAV=9.832

      ECCEN=2.0
      ECM2=1.0/(ECCEN**2)
      GMIN=1.0D-20
      RADIUS=6370.E3

      PSTAR=SEAICE_strength

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

C NOW SET UP MASS PER UNIT AREA AND CORIOLIS TERM
          AMASS(I,J,bi,bj)=RHOICE*0.25*(HEFF(i,j,1,bi,bj)
     &                                 +HEFF(i+1,j,1,bi,bj)
     &                                 +HEFF(i,j+1,1,bi,bj)
     &                                 +HEFF(i+1,j+1,1,bi,bj))
          COR_ICE(I,J,bi,bj)=AMASS(I,J,bi,bj)
     &                      *2.0*OMEGA*SINEICE(I,J,bi,bj)
C NOW SET UP FORCING FIELD
C FIRST WIND
C IF WIND ON B GRID
          AAA=SQRT(GAIRX(I,J,bi,bj)**2+GAIRY(I,J,bi,bj)**2)
C IF WIND ON C GRID
C          AAA=SQRT((0.5*(GAIRX(I+1,J,bi,bj)+GAIRX(I+1,J+1,bi,bj)))**2
C     &       +(0.5*(GAIRY(I,J+1,bi,bj)+GAIRY(I+1,J+1,bi,bj)))**2)

          DAIRN(I,J,bi,bj)=RHOAIR*SEAICE_drag*
     &                     (2.70+0.142*AAA+0.0764*AAA*AAA)

C IF WIND ON B GRID
          FORCEX(I,J,bi,bj)=DAIRN(I,J,bi,bj)*(COSWIN*GAIRX(I,J,bi,bj)
     &                                       -SINWIN*GAIRY(I,J,bi,bj))
          FORCEY(I,J,bi,bj)=DAIRN(I,J,bi,bj)*(SINWIN*GAIRX(I,J,bi,bj)
     &                                       +COSWIN*GAIRY(I,J,bi,bj))
C IF WIND ON C GRID
C          FORCEX(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
C     &          (COSWIN*0.5*(GAIRX(I+1,J,bi,bj)+GAIRX(I+1,J+1,bi,bj))
C     &          -SINWIN*0.5*(GAIRY(I,J+1,bi,bj)+GAIRY(I+1,J+1,bi,bj)))
C          FORCEY(I,J,bi,bj)=DAIRN(I,J,bi,bj)*
C     &          (SINWIN*0.5*(GAIRX(I+1,J,bi,bj)+GAIRX(I+1,J+1,bi,bj))
C     &          +COSWIN*0.5*(GAIRY(I,J+1,bi,bj)+GAIRY(I+1,J+1,bi,bj)))

C STORE WIND ONLY STRESS 
          WINDX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
          WINDY(I,J,bi,bj)=FORCEY(I,J,bi,bj)

C NOW ADD IN TILT
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &                    -COR_ICE(I,J,bi,bj)*GWATY(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &                    +COR_ICE(I,J,bi,bj)*GWATX(I,J,bi,bj)
C NOW SET UP ICE PRESSURE AND VISCOSITIES
          PRESS(I,J,bi,bj)=PSTAR*HEFF(I,J,1,bi,bj)
     &                    *EXP(-20.0*(1.0-AREA(I,J,1,bi,bj)))
          ZMAX(I,J,bi,bj)=(5.0D+12/(2.0D+04))*PRESS(I,J,bi,bj)
          ZMIN(I,J,bi,bj)=4.0E+08
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
     &          -(0.25/(DXUICE(I,J,bi,bj)*CSUICE(I,J,bi,bj)))
     &          *(PRESS(I+1,J,bi,bj)+PRESS(I+1,J+1,bi,bj)
     &          -PRESS(I,J,bi,bj)-PRESS(I,J+1,bi,bj))
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)-0.25/DYUICE(I,J,bi,bj)
     &          *(PRESS(I,J+1,bi,bj)+PRESS(I+1,J+1,bi,bj)
     &          -PRESS(I,J,bi,bj)-PRESS(I+1,J,bi,bj))
C NOW KEEP FORCEX0
          FORCEX0(I,J,bi,bj)=FORCEX(I,J,bi,bj)
          FORCEY0(I,J,bi,bj)=FORCEY(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C DO PSEUDO-TIMESTEPS TO OBTAIN AN ACCURATE VISCOUS-PLASTIC SOLUTION
C 10 PSEUDO-TIMESTEPS OR MORE ARE SUGGESTED FOR HIGH-RESOLUTION (~10KM)
C 1 PSEUDO-TIMESTEP CAN BE USED FOR LOW-RESOLUTION GLOBAL MODELING
C NPSEUDO is now set in data.seaice input file
C TIMESTEP FOR PSEUDO-TIMESTEPPING
      DELTAT=DELTAT/NPSEUDO
      DO 5000 KII=1,NPSEUDO

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
C NOW SET UP NON-LINEAR WATER DRAG
          DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT((UICE(I,J,1,bi,bj)
     &                    -GWATX(I,J,bi,bj))**2
     &                    +(VICE(I,J,1,bi,bj)-GWATY(I,J,bi,bj))**2)
          DWATN(I,J,bi,bj)=MAX(DWATN(I,J,bi,bj),0.25 d 0) 
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
          E11(I,J,bi,bj)=0.5/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(UICE(I,J,1,bi,bj)+UICE(I,J-1,1,bi,bj)
     &                  -UICE(I-1,J,1,bi,bj)-UICE(I-1,J-1,1,bi,bj))
     &                  -0.25*(VICE(I,J,1,bi,bj)+VICE(I-1,J,1,bi,bj)
     &                  +VICE(I-1,J-1,1,bi,bj)+VICE(I,J-1,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS
          E22(I,J,bi,bj)=0.5/DYTICE(I,J,bi,bj)
     &                  *(VICE(I,J,1,bi,bj)+VICE(I-1,J,1,bi,bj)
     &                  -VICE(I,J-1,1,bi,bj)-VICE(I-1,J-1,1,bi,bj))
          E12(I,J,bi,bj)=0.5*(0.5/DYTICE(I,J,bi,bj)
     &                  *(UICE(I,J,1,bi,bj)+UICE(I-1,J,1,bi,bj)
     &                  -UICE(I,J-1,1,bi,bj)-UICE(I-1,J-1,1,bi,bj))
     &                  +0.5/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(VICE(I,J,1,bi,bj)+VICE(I,J-1,1,bi,bj)
     &                  -VICE(I-1,J,1,bi,bj)-VICE(I-1,J-1,1,bi,bj))
     &                  +0.25*(UICE(I,J,1,bi,bj)+UICE(I-1,J,1,bi,bj)
     &                  +UICE(I-1,J-1,1,bi,bj)+UICE(I,J-1,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS)
C  NOW EVALUATE VISCOSITIES
          DELT1=(E11(I,J,bi,bj)**2+E22(I,J,bi,bj)**2)*(1.0+ECM2)
     &        +4.0*ECM2*E12(I,J,bi,bj)**2
     1        +2.0*E11(I,J,bi,bj)*E22(I,J,bi,bj)*(1.0-ECM2)
          DELT2=SQRT(DELT1)
          DELT2=MAX(GMIN,DELT2)
          ZETA(I,J,bi,bj)=0.5*PRESS(I,J,bi,bj)/DELT2
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
      IF ( SEAICEuseLSR ) THEN
         CALL LSR( myThid )
      ELSE
         CALL ADI( myThid )
      ENDIF

C NOW DO MODIFIED EULER STEP
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(I,J,1,bi,bj)=0.5*(UICE(I,J,1,bi,bj)+UICE(I,J,2,bi,bj))
          VICE(I,J,1,bi,bj)=0.5*(VICE(I,J,1,bi,bj)+VICE(I,J,2,bi,bj))
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
          DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT((UICE(I,J,1,bi,bj)
     &                    -GWATX(I,J,bi,bj))**2
     &                    +(VICE(I,J,1,bi,bj)-GWATY(I,J,bi,bj))**2)
          DWATN(I,J,bi,bj)=MAX(DWATN(I,J,bi,bj),0.25 d 0) 
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
          E11(I,J,bi,bj)=0.5/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(UICE(I,J,1,bi,bj)+UICE(I,J-1,1,bi,bj)
     &                  -UICE(I-1,J,1,bi,bj)-UICE(I-1,J-1,1,bi,bj))
     &                  -0.25*(VICE(I,J,1,bi,bj)+VICE(I-1,J,1,bi,bj)
     &                  +VICE(I-1,J-1,1,bi,bj)+VICE(I,J-1,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS
          E22(I,J,bi,bj)=0.5/DYTICE(I,J,bi,bj)
     &                  *(VICE(I,J,1,bi,bj)+VICE(I-1,J,1,bi,bj)
     &                  -VICE(I,J-1,1,bi,bj)-VICE(I-1,J-1,1,bi,bj))
          E12(I,J,bi,bj)=0.5*(0.5/DYTICE(I,J,bi,bj)
     &                  *(UICE(I,J,1,bi,bj)+UICE(I-1,J,1,bi,bj)
     &                  -UICE(I,J-1,1,bi,bj)-UICE(I-1,J-1,1,bi,bj))
     &                  +0.5/(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
     &                  *(VICE(I,J,1,bi,bj)+VICE(I,J-1,1,bi,bj)
     &                  -VICE(I-1,J,1,bi,bj)-VICE(I-1,J-1,1,bi,bj))
     &                  +0.25*(UICE(I,J,1,bi,bj)+UICE(I-1,J,1,bi,bj)
     &                  +UICE(I-1,J-1,1,bi,bj)+UICE(I,J-1,1,bi,bj))
     &                  *TNGTICE(I,J,bi,bj)/RADIUS)
C  NOW EVALUATE VISCOSITIES
          DELT1=(E11(I,J,bi,bj)**2+E22(I,J,bi,bj)**2)*(1.0+ECM2)
     &        +4.0*ECM2*E12(I,J,bi,bj)**2
     1        +2.0*E11(I,J,bi,bj)*E22(I,J,bi,bj)*(1.0-ECM2)
          DELT2=SQRT(DELT1)
          DELT2=MAX(GMIN,DELT2)
          ZETA(I,J,bi,bj)=0.5*PRESS(I,J,bi,bj)/DELT2
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
      IF ( SEAICEuseLSR ) THEN
         CALL LSR( myThid )
      ELSE
         CALL ADI( myThid )
      ENDIF

5000  CONTINUE

      ENDIF
#endif SEAICE_ALLOW_DYNAMICS

C Calculate ocean surface stress
      CALL OSTRES ( DWATN, COR_ICE, myThid )

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS ) THEN

C NOW BACK TO NORMAL TIMESTEP
      DELTAT=DELTAT*NPSEUDO

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
#endif SEAICE_DEBUG
          UICE(i,j,1,bi,bj)=min(UICE(i,j,1,bi,bj),0.40D+00)
          VICE(i,j,1,bi,bj)=min(VICE(i,j,1,bi,bj),0.40D+00)
          UICE(i,j,1,bi,bj)=max(UICE(i,j,1,bi,bj),-0.40D+00)
          VICE(i,j,1,bi,bj)=max(VICE(i,j,1,bi,bj),-0.40D+00)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      ENDIF
#endif SEAICE_ALLOW_DYNAMICS

#endif ALLOW_SEAICE

      RETURN
      END
