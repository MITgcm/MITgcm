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

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  TBC, salinity_ice, SDF, Q0, QS
      _RL  theta_old

      _RL GAREA( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL GHEFF( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy           )
      _RL AR   ( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy, nSx, nSy )

      salinity_ice=4.0 _d 0      ! ICE SALINITY
      TBC=271.2 _d 0-273.16 _d 0 ! FREEZING TEMP. OF SEA WATER
      SDF=1000.0 _d 0/330.0 _d 0 ! RATIO OF WATER DESITY AND SNOW DENSITY
      Q0=1.0D-06/302.0 _d +00    ! INVERSE HEAT OF FUSION OF ICE
      QS=1.1 _d +08              ! HEAT OF FUSION OF SNOW

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c
cph(
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
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)= comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE atemp(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
cph)
        DO J=1,sNy
         DO I=1,sNx
C Adjust surface air temperature according to ice conditions
          IF(AREA(I,J,1,bi,bj).GE.0.75 _d 0
     &      .AND.ATEMP(I,J,bi,bj).GT.273.052 _d 0) THEN
           ATEMP(I,J,bi,bj)=273.052 _d 0
          ENDIF
          SEAICE_SALT(I,J,bi,bj)=ZERO
          WATR(I,J,bi,bj)=ZERO
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          AR(I,J,bi,bj)=MIN(AREA(I,J,2,bi,bj),
     &         HEFF(I,J,2,bi,bj)*1.0 _d +04)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C NOW BALANCE THE HEAT IN OCEAN FIRT LEVEL
C Here the units of YNEG are m of sea-ice.
C The factor dRf(1)/72.0764, used to convert temperature
C change in deg K to m of sea-ice, is approximately:
C dRf(1) * (sea water heat capacity = 3996 J/kg/K)
C        * (density of sea-water = 1026 kg/m^3)
C        / (latent heat of fusion of sea-ice = 334000 J/kg)
C        / (density of sea-ice = 910 kg/m^3)
          YNEG(I,J,bi,bj)=(theta(I,J,1,bi,bj)-TBC)*dRf(1)/72.0764 _d 0
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
           HEFF(I,J,1,bi,bj)=MAX(ZERO,HEFF(I,J,3,bi,bj))
C NOW CORRECT YNEG
           YNEG(I,J,bi,bj)=HEFF(I,J,1,bi,bj)-HEFF(I,J,3,bi,bj)
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &                          +(HEFF(I,J,1,bi,bj)-GHEFF(I,J))
           theta_old=theta(I,J,1,bi,bj)
           theta(I,J,1,bi,bj)=
     &                    (YNEG(I,J,bi,bj)*72.0764 _d 0/dRf(1)+TBC)
C HERE YNEG IS THE OCEAN TEMP. DIFFERENCE WITH NO SUPER-COOLING
           YNEG(I,J,bi,bj)=theta(I,J,1,bi,bj)-theta_old
          ENDIF
C surfaceTendencyTice is the equivalent amount of surface heating
C implied by above change in surface level temperature (YNEG).
C Units are degrees/s (>0 for ocean warming).
          surfaceTendencyTice(I,J,bi,bj)=YNEG(I,J,bi,bj)/deltaTtracer
         ENDDO
        ENDDO
c
       ENDDO
      ENDDO

cph(
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area   = comlev1, key = ikey_dynamics
CADJ STORE atemp  = comlev1, key = ikey_dynamics
CADJ STORE heff   = comlev1, key = ikey_dynamics
CADJ STORE hsnow  = comlev1, key = ikey_dynamics
CADJ STORE lwdown = comlev1, key = ikey_dynamics
CADJ STORE tice   = comlev1, key = ikey_dynamics
CADJ STORE uwind  = comlev1, key = ikey_dynamics
CADJ STORE vwind  = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */
cph)
C GROWTH SUBROUTINE CALCULATES TOTAL GROWTH TENDENCIES,
C INCLUDING SNOWFALL
      CALL GROATB(A22,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
cph(
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
c
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE heff(:,:,:,bi,bj) = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE hsnow(:,:,bi,bj)  = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE fo(:,:,bi,bj)     = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
CADJ STORE fice(:,:,bi,bj)   = comlev1_bibj, 
CADJ &                         key = iicekey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
cph)
C NOW CALCULATE CORRECTED GROWTH
        DO J=1,sNy
         DO I=1,sNx
          GHEFF(I,J)=-DELTAT*FICE(I,J,bi,bj)
          GAREA(I,J)=HSNOW(I,J,bi,bj)*QS
          IF(GHEFF(I,J).GT.ZERO.AND.GHEFF(I,J).LE.GAREA(I,J)) THEN
           HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)-GHEFF(I,J)/QS
C SNOW CONVERTED INTO WATER AND THEN INTO ICE
C The factor 0.920 is used to convert m of sea-ice to m of freshwater
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &                  -(GHEFF(I,J)/QS)/SDF/0.920 _d 0*AR(I,J,bi,bj)
           WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &                  -(GHEFF(I,J)/QS)/SDF/0.920 _d 0*AR(I,J,bi,bj)
           FICE(I,J,bi,bj)=ZERO
          ELSE IF(GHEFF(I,J).GT.GAREA(I,J)) THEN
           FICE(I,J,bi,bj)=-(GHEFF(I,J)-GAREA(I,J))/DELTAT
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &               -HSNOW(I,J,bi,bj)/SDF/0.920 _d 0*AR(I,J,bi,bj)
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
     &                    +(ONE-AR(I,J,bi,bj))*FO(I,J,bi,bj)
         ENDDO
        ENDDO


C NOW UPDATE AREA
        DO J=1,sNy
         DO I=1,sNx
          GHEFF(I,J)=-DELTAT*FHEFF(I,J,bi,bj)*Q0
          GAREA(I,J)=DELTAT*FO(I,J,bi,bj)*Q0
          GHEFF(I,J)=-ONE*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
          GAREA(I,J)=MAX(ZERO,GAREA(I,J))
          HCORR(I,J,bi,bj)=MIN(ZERO,GHEFF(I,J))
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          GAREA(I,J)=TWO*(ONE-AREA(I,J,2,bi,bj))*GAREA(I,J)/HO
     &    +HALF*HCORR(I,J,bi,bj)*AREA(I,J,2,bi,bj)
     &    /(HEFF(I,J,1,bi,bj)+.00001 _d 0)
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)+GAREA(I,J)
         ENDDO
        ENDDO

C NOW UPDATE HEFF
        DO J=1,sNy
         DO I=1,sNx
          GHEFF(I,J)=-DELTAT*FICE(I,J,bi,bj)*Q0*AR(I,J,bi,bj)
          GHEFF(I,J)=-ONE*MIN(HEFF(I,J,1,bi,bj),GHEFF(I,J))
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
     &                    +(ONE-AR(I,J,bi,bj))*QNETO(I,J,bi,bj)
          QSW(I,J,bi,bj)=QSWI(I,J,bi,bj)*AR(I,J,bi,bj)
     &                    +(ONE-AR(I,J,bi,bj))*QSWO(I,J,bi,bj)
#ifndef SHORTWAVE_HEATING
          QNET(I,J,bi,bj)=QNET(I,J,bi,bj)+QSW(I,J,bi,bj)
#endif
         ENDDO
        ENDDO

C NOW UPDATE OTHER THINGS
        DO J=1,sNy
         DO I=1,sNx
          IF(FICE(I,J,bi,bj).GT.ZERO) THEN
C FREEZING, PRECIP ADDED AS SNOW
           HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)
     &          +DELTAT*PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*SDF
          ELSE
C ADD PRECIP AS RAIN, WATER CONVERTED INTO ICE BY /0.920 _d 0
           SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &         -PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*DELTAT/0.920 _d 0
           WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &         -PRECIP(I,J,bi,bj)*AREA(I,J,2,bi,bj)*DELTAT/0.920 _d 0
          ENDIF
c Now add in precip over open water directly into ocean as negative salt
          SEAICE_SALT(I,J,bi,bj)=SEAICE_SALT(I,J,bi,bj)
     &         -PRECIP(I,J,bi,bj)*(ONE-AREA(I,J,2,bi,bj))
     &         *DELTAT/0.920 _d 0
          WATR(I,J,bi,bj)=WATR(I,J,bi,bj)
     &         -PRECIP(I,J,bi,bj)*(ONE-AREA(I,J,2,bi,bj))
     &         *DELTAT/0.920 _d 0
C NOW GET FRESH WATER FLUX
          EmPmR(I,J,bi,bj)=EVAP(I,J,bi,bj)-RUNOFF(I,J,bi,bj)
     &                    +SEAICE_SALT(I,J,bi,bj)*0.92 _d 0/DELTAT
         ENDDO
        ENDDO

#ifdef SEAICE_DEBUG
c      CALL PLOT_FIELD_XYRS( UWIND,'Current UWIND ', myIter, myThid )
c      CALL PLOT_FIELD_XYRS( VWIND,'Current VWIND ', myIter, myThid )
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
     &           +(ONE-AR(I,J,bi,bj))*QNETO(I,J,bi,bj), QNET(I,J,bi,bj)
          endif
         ENDDO
        ENDDO
#endif /* SEAICE_DEBUG */

crg Added by Ralf Giering: do we need DO_WE_NEED_THIS ?
#define DO_WE_NEED_THIS
C NOW ZERO OUTSIDE POINTS
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
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          AREA(I,J,1,bi,bj)=MAX(ZERO,AREA(I,J,1,bi,bj))
          HSNOW(I,J,bi,bj)=MAX(ZERO,HSNOW(I,J,bi,bj))
#endif
          AREA(I,J,1,bi,bj)=AREA(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
          HEFF(I,J,1,bi,bj)=HEFF(I,J,1,bi,bj)*HEFFM(I,J,bi,bj)
#ifdef DO_WE_NEED_THIS
c          HEFF(I,J,1,bi,bj)=MIN(MAX_HEFF,HEFF(I,J,1,bi,bj))
#endif
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)*HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

#endif /* ALLOW_SEAICE */

      RETURN
      END
