C $Header:

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE growth( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE growth                                        |
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
#include "SEAICE_EXTERNAL.h"

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid
CEndOfInterface

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      _RL  ZERO, ONE, TBC, salinity_ice, SDF, Q0, QS
      _RL  theta_old

      _RL GAREA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL GHEFF( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL AR   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )

      ZERO=0.0
      ONE=1.0
      salinity_ice=4.0     ! ICE SALINITY
      TBC=271.2-273.16     ! FREEZING TEMP. OF SEA WATER
      SDF=1000.0/330.0     ! RATIO OF WATER DESITY AND SNOW DENSITY
      Q0=1.0D-06/302.0D+00 ! INVERSE HEAT OF FUSION OF ICE
      QS=1.1D+08           ! HEAT OF FUSION OF SNOW

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          SEAICE_SALT(I,J,bi,bj)=0.0
          WATR(I,J,bi,bj)=0.0
          AR(I,J,bi,bj)=MIN(AREA(I,J,2,bi,bj),HEFF(I,J,2,bi,bj)*1.0E+04)
C NOW BALANCE THE HEAT IN OCEAN FIRT LEVEL
          YNEG(I,J,bi,bj)=(theta(I,J,1,bi,bj)-TBC)*dRf(1)/72.0764
          IF(YNEG(I,J,bi,bj).LE.ZERO) THEN
C SUPERCOOLING, CONVERT TO ICE
           HEFF(I,J,1,bi,bj)=HEFF(I,J,1,bi,bj)-YNEG(I,J,bi,bj)
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)-YNEG(I,J,bi,bj)
C HERE YNEG IS THE OCEAN TEMP. DIFFERENCE WITH SUPER-COOLING
           YNEG(I,J,bi,bj)=TBC-theta(I,J,1,bi,bj)
           theta(I,J,1,bi,bj)=TBC
          ELSE
C NOW CORRECT ICE THICKNESS
           GHEFF(I,J)=HEFF(I,J,1,bi,bj)
           HEFF(I,J,3,bi,bj)=HEFF(I,J,1,bi,bj)-YNEG(I,J,bi,bj)
           HEFF(I,J,1,bi,bj)=MAX(0.0,HEFF(I,J,3,bi,bj))
C NOW CORRECT YNEG
           YNEG(I,J,bi,bj)=HEFF(I,J,1,bi,bj)-HEFF(I,J,3,bi,bj)
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &                          +(HEFF(I,J,1,bi,bj)-GHEFF(I,J))
           theta_old=theta(I,J,1,bi,bj)
           theta(I,J,1,bi,bj)=
     &                    (YNEG(I,J,bi,bj)*72.0764/dRf(1)+TBC)
C HERE YNEG IS THE OCEAN TEMP. DIFFERENCE WITH NO SUPER-COOLING
           YNEG(I,J,bi,bj)=theta(I,J,1,bi,bj)-theta_old
          ENDIF
C surfaceTendencyTice is the equivalent amount of surface heating
C implied by above change in surface level temperature (YNEG).
C Units are degrees/s (>0 for ocean warming).
          surfaceTendencyTice(I,J,bi,bj)=YNEG(I,J,bi,bj)/deltaTtracer
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C GROWTH SUBROUTINE CALCULATES TOTAL GROWTH TENDENCIES,
C INCLUDING SNOWFALL
      CALL GROATB(A22,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW CALCULATE CORRECTED GROWTH
        DO J=1,sNy
         DO I=1,sNx
cdm ===> why is following line here?
          FICE(I,J,bi,bj)=FICE(I,J,bi,bj)
          GHEFF(I,J)=-DELTAT*FICE(I,J,bi,bj)
          GAREA(I,J)=HSNOW(I,J,bi,bj)*QS
          IF(GHEFF(I,J).GT.ZERO.AND.GHEFF(I,J).LE.GAREA(I,J)) THEN
           HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)-GHEFF(I,J)/QS
C SNOW CONVERTED INTO WATER AND THEN INTO ICE
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &                  -(GHEFF(I,J)/QS)/SDF/0.920*AR(I,J,bi,bj)
           WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &                  -(GHEFF(I,J)/QS)/SDF/0.920*AR(I,J,bi,bj)
           FICE(I,J,bi,bj)=0.0D+00
          ELSE IF(GHEFF(I,J).GT.GAREA(I,J)) THEN
           FICE(I,J,bi,bj)=-(GHEFF(I,J)-GAREA(I,J))/DELTAT
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &               -HSNOW(I,J,bi,bj)/SDF/0.920*AR(I,J,bi,bj)
           WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &               -HSNOW(I,J,bi,bj)/SDF/0.920*AR(I,J,bi,bj)
           HSNOW(I,J,bi,bj)=0.0
          END IF

         ENDDO
        ENDDO

C NOW GET TOTAL GROWTH RATE
        DO J=1,sNy
         DO I=1,sNx
          FHEFF(I,J,bi,bj)=FICE(I,J,bi,bj)*AR(I,J,bi,bj)
     &                    +(1.0-AR(I,J,bi,bj))*FO(I,J,bi,bj)
         ENDDO
        ENDDO


C NOW UPDATE AREA
        DO J=1,sNy
         DO I=1,sNx
          GHEFF(I,J)=-DELTAT*FHEFF(I,J,bi,bj)*Q0
          GAREA(I,J)=DELTAT*FO(I,J,bi,bj)*Q0
          GHEFF(I,J)=-1.0*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
          GAREA(I,J)=MAX(ZERO,GAREA(I,J))
          HCORR(I,J,bi,bj)=MIN(ZERO,GHEFF(I,J))
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          GAREA(I,J)=2.0*(1.0-AREA(I,J,2,bi,bj))*GAREA(I,J)/HO
     &    +0.5*HCORR(I,J,bi,bj)*AREA(I,J,2,bi,bj)
     &    /(HEFF(I,J,1,bi,bj)+.00001)
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)+GAREA(I,J)
         ENDDO
        ENDDO

C NOW UPDATE HEFF
        DO J=1,sNy
         DO I=1,sNx
          GHEFF(I,J)=-DELTAT*FICE(I,J,bi,bj)*Q0*AR(I,J,bi,bj)
          GHEFF(I,J)=-1.0*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
          HEFF(I,J,1,bi,bj)=HEFF(I,J,1,bi,bj)+GHEFF(I,J)
          SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)+GHEFF(I,J)
C NOW CALCULATE QNETI UNDER ICE IF ANY
          QNETI(I,J,bi,bj)=(GHEFF(I,J)-DELTAT*FICE(I,J,bi,bj)
     &         *Q0*AR(I,J,bi,bj))/Q0/DELTAT
         ENDDO
        ENDDO

C NOW GET TOTAL QNET AND QSW
        DO J=1,sNy
         DO I=1,sNx
          QNET(I,J,bi,bj)=QNETI(I,J,bi,bj)*AR(I,J,bi,bj)
     &                    +(1.0-AR(I,J,bi,bj))*QNETO(I,J,bi,bj)
          QSW(I,J,bi,bj)=QSWI(I,J,bi,bj)*AR(I,J,bi,bj)
     &                    +(1.0-AR(I,J,bi,bj))*QSWO(I,J,bi,bj)
         ENDDO
        ENDDO

C NOW UPDATE OTHER THINGS
        DO J=1,sNy
         DO I=1,sNx
          IF(FICE(I,J,bi,bj).GT.ZERO) THEN
C FREEZING, PRECIP ADDED AS SNOW
           HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)
     &               +DELTAT*RAIN(I,J,bi,bj)*AREA(I,J,2,bi,bj)*SDF
          ELSE
C ADD PRECIP AS RAIN, WATER CONVERTED INTO ICE BY /0.920
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &               -RAIN(I,J,bi,bj)*AREA(I,J,2,bi,bj)*DELTAT/0.920
           WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &               -RAIN(I,J,bi,bj)*AREA(I,J,2,bi,bj)*DELTAT/0.920
          ENDIF
c Now add in precip over open water directly into ocean as negative salt
          SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &          -RAIN(I,J,bi,bj)*(1.0-AREA(I,J,2,bi,bj))*DELTAT/0.920
          WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &          -RAIN(I,J,bi,bj)*(1.0-AREA(I,J,2,bi,bj))*DELTAT/0.920
C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)=EVAP(I,J,bi,bj)-RUNOFF(I,J,bi,bj)
     &                    +SEAICE_SALT(I,J,bi,bj)*0.92/DELTAT
         ENDDO
        ENDDO

#ifdef SEAICE_DEBUG
c       CALL PLOT_FIELD_XYRS( GAIRX,'Current GAIRX ', myIter, myThid )
c       CALL PLOT_FIELD_XYRS( GAIRY,'Current GAIRY ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( GWATX,'Current GWATX ', myIter, myThid )
       CALL PLOT_FIELD_XYRS( GWATY,'Current GWATY ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( FO,'Current FO ', myIter, myThid )
       CALL PLOT_FIELD_XYRL( FHEFF,'Current FHEFF ', myIter, myThid )
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
     &            HEFF(i,j,1,bi,bj),theta(I,J,1,bi,bj),yneg(I,J,bi,bj)
             print '(A,3f10.2)','QSW, QNET before/after correction',
     &            QSW(I,J,bi,bj),QNETI(I,J,bi,bj)*AR(I,J,bi,bj)
     &           +(1.0-AR(I,J,bi,bj))*QNETO(I,J,bi,bj), QNET(I,J,bi,bj)
          endif
         ENDDO
        ENDDO
#endif SEAICE_DEBUG

C NOW ZERO OUTSIDE POINTS
        DO J=1,sNy
         DO I=1,sNx
C NOW SET AREA(I,J,1,bi,bj)=0 WHERE NO ICE IS
          AREA(I,J,1,bi,bj)=MIN(AREA(I,J,1,bi,bj)
     &                         ,HEFF(I,J,1,bi,bj)/.0001)
C NOW TRUNCATE AREA
          AREA(I,J,1,bi,bj)=MIN(ONE,AREA(I,J,1,bi,bj))
          AREA(I,J,1,bi,bj)=MAX(ZERO,AREA(I,J,1,bi,bj))
          HSNOW(I,J,bi,bj)=MAX(ZERO,HSNOW(I,J,bi,bj))
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,1,bi,bj)=HEFF(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,1,bi,bj)=MIN(MAX_HEFF,HEFF(I,J,1,bi,bj))
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif ALLOW_SEAICE

      RETURN
      END
