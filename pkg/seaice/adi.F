C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE adi( myThid )
C     /==========================================================\
C     | SUBROUTINE  adi                                          |
C     | o Solve ice momentum equation with an ADI                |
C     |   (see Zhang and Rothrock, JGR, 105, 3325-3338, 2000)    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_GRID.h"

 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj, j1, j2, im, jm
      _RL  RADIUS, DELXY, DELXR, DELYR, DELX2, DELY2, RADIUS2
      _RL  ETAMEAN, ZETAMEAN
      _RL  AA1, AA2, AA3, AA4, AA5, AA6, AA9

      _RL AU   (1:sNx,1:sNy), BU   (1:sNx,1:sNy), CU   (1:sNx,1:sNy)
      _RL AV   (1:sNx,1:sNy), BV   (1:sNx,1:sNy), CV   (1:sNx,1:sNy)
      _RL FXY  (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL FXY1 (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

      _RL URT(1:sNx), VRT(1:sNy), CUU(1:sNx), CVV(1:sNy)

C SET SOME VALUES
      RADIUS=6370. _d 3

C SOLVE FOR UICE
C FIRST HALF FIRST

c$taf loop = parallel
      DO bj=myByLo(myThid),myByHi(myThid)
c$taf loop = parallel
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1,sNy
         DO i=1,sNx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)*UVM(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)*UVM(I,J,bi,bj)
         ENDDO
        ENDDO

        DO J=1,sNy
         DO I=1,sNx
          DELXY=HALF/(DXUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELXR=HALF/(DXUICE(I,J,bi,bj)*RADIUS)
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))

          FXY(I,J)=DRAGA(I,J,bi,bj)*VICEC(I,J,bi,bj)+FORCEX(I,J,bi,bj)
     3            +HALF*(ZETA(I+1,J+1,bi,bj)*(VICEC(I+1,J+1,bi,bj)
     3            +VICEC(I,J+1,bi,bj)
     3            -VICEC(I+1,J,bi,bj)-VICEC(I,J,bi,bj))
     3            +ZETA(I+1,J,bi,bj)*(VICEC(I+1,J,bi,bj)
     3            +VICEC(I,J,bi,bj)-VICEC(I+1,J-1,bi,bj)
     3            -VICEC(I,J-1,bi,bj))+ZETA(I,J+1,bi,bj)
     3            *(VICEC(I,J,bi,bj)+VICEC(I-1,J,bi,bj)
     3            -VICEC(I,J+1,bi,bj)-VICEC(I-1,J+1,bi,bj))
     3            +ZETA(I,J,bi,bj)*(VICEC(I,J-1,bi,bj)
     3            +VICEC(I-1,J-1,bi,bj)-VICEC(I,J,bi,bj)
     3            -VICEC(I-1,J,bi,bj)))*DELXY/CSUICE(I,J,bi,bj)
     3
     4            -HALF*(ETA(I+1,J+1,bi,bj)*(VICEC(I+1,J+1,bi,bj)
     4            +VICEC(I,J+1,bi,bj)
     4            -VICEC(I+1,J,bi,bj)-VICEC(I,J,bi,bj))
     4            +ETA(I+1,J,bi,bj)*(VICEC(I+1,J,bi,bj)
     4            +VICEC(I,J,bi,bj)-VICEC(I+1,J-1,bi,bj)
     4            -VICEC(I,J-1,bi,bj))+ETA(I,J+1,bi,bj)
     4            *(VICEC(I,J,bi,bj)+VICEC(I-1,J,bi,bj)
     4            -VICEC(I,J+1,bi,bj)-VICEC(I-1,J+1,bi,bj))
     4            +ETA(I,J,bi,bj)*(VICEC(I,J-1,bi,bj)
     4            +VICEC(I-1,J-1,bi,bj)-VICEC(I,J,bi,bj)
     4            -VICEC(I-1,J,bi,bj)))*DELXY/CSUICE(I,J,bi,bj)
     4
     5            +HALF*(VICEC(I+1,J,bi,bj)-VICEC(I-1,J,bi,bj))
     5            *(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     5            -ETA(I,J,bi,bj)-ETA(I+1,J,bi,bj))*DELXY
     5            /CSUICE(I,J,bi,bj)+HALF*ETAMEAN
     5            *((VICEC(I+1,J+1,bi,bj)
     5            -VICEC(I-1,J+1,bi,bj))/CSUICE(I,J+1,bi,bj)
     5            -(VICEC(I+1,J-1,bi,bj)-VICEC(I-1,J-1,bi,bj))
     5            /CSUICE(I,J-1,bi,bj))*DELXY
     5
     6            -((ZETA(I+1,J+1,bi,bj)+ZETA(I+1,J,bi,bj)
     6            -ZETA(I,J,bi,bj)-ZETA(I,J+1,bi,bj))
     6            +(ETA(I+1,J+1,bi,bj)+ETA(I+1,J,bi,bj)
     6            -ETA(I,J,bi,bj)-ETA(I,J+1,bi,bj)))
     6            *TNGICE(I,J,bi,bj)*VICEC(I,J,bi,bj)
     &            *DELXR/CSUICE(I,J,bi,bj)
     6            -(ETAMEAN+ZETAMEAN)*TNGICE(I,J,bi,bj)
     6            *(VICEC(I+1,J,bi,bj)-VICEC(I-1,J,bi,bj))
     6            *DELXR/CSUICE(I,J,bi,bj)
     6
     7            -ETAMEAN*TWO*TNGICE(I,J,bi,bj)*(VICEC(I+1,J,bi,bj)
     7            -VICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)
         END DO
        END DO

        DO J=1,sNy
         DO I=1,sNx
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          RADIUS2=ONE/(RADIUS*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          AA1=((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA2=((ETA(I,J,bi,bj)+ZETA(I,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA3=ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
          AA4=ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
          AA5=-(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)-ETA(I,J,bi,bj)
     &       -ETA(I+1,J,bi,bj))*TNGICE(I,J,bi,bj)
          AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)
          AU(I,J)=-AA2*DELX2*UVM(I,J,bi,bj)
          BU(I,J)=((AA1+AA2)*DELX2+AA6*RADIUS2
     &    +AMASS(I,J,bi,bj)/SEAICE_DT*TWO
     &    +DRAGS(I,J,bi,bj))*UVM(I,J,bi,bj)
     &    +(ONE-UVM(I,J,bi,bj))
          CU(I,J)=-AA1*DELX2*UVM(I,J,bi,bj)
         END DO
        END DO

cdm ==> this is equivalent to zeroing AU and CU at arbitrary locations
cdm ==> in the middle of the ocean.  Is this really what you want to do?
        DO J=1,sNy
         AU(1,J)=ZERO
         CU(sNx,J)=ZERO
         CU(1,J)=CU(1,J)/BU(1,J)
        END DO

c$taf loop = parallel
        DO 1200 J=1,sNy
         DO I=1,sNx

          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))

          AA1=((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA2=((ETA(I,J,bi,bj)+ZETA(I,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA3=ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
          AA4=ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
          AA5=-(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)-ETA(I,J,bi,bj)
     &       -ETA(I+1,J,bi,bj))*TNGICE(I,J,bi,bj)
          AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

          IF(I.EQ.1) THEN
           AA9=AA2*DELX2*UICEC(I-1,J,bi,bj)*UVM(I,J,bi,bj)
          ELSE IF(I.EQ.sNx) THEN
           AA9=AA1*DELX2*UICEC(I+1,J,bi,bj)*UVM(I,J,bi,bj)
          ELSE
           AA9=ZERO
          END IF

          URT(I)=AA9+FXY(I,J)-AA5*DELYR*UICE(I,J,2,bi,bj)
     1    -(AA3+AA4)*DELY2*UICE(I,J,2,bi,bj)
     1    +(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj))
     1    *UICE(I,J+1,2,bi,bj)*DELY2
     2    +(ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))*UICE(I,J-1,2,bi,bj)*DELY2
     3    +ETAMEAN*DELYR*(UICE(I,J+1,2,bi,bj)*TNGICE(I,J+1,bi,bj)
     3    -UICE(I,J-1,2,bi,bj)*TNGICE(I,J-1,bi,bj))
     4    -ETAMEAN*DELYR*TWO*TNGICE(I,J,bi,bj)*(UICE(I,J+1,2,bi,bj)
     4    -UICE(I,J-1,2,bi,bj))
          URT(I)=(URT(I)+AMASS(I,J,bi,bj)/SEAICE_DT
     &         *UICE(I,J,2,bi,bj)*TWO)*UVM(I,J,bi,bj)
         END DO

cdm ==> CUU will contain bands of zeros at arbitrary locations
         DO I=1,sNx
          CUU(I)=CU(I,J)
         END DO
         URT(1)=URT(1)/BU(1,J)
         DO I=2,sNx
          IM=I-1
cdm ==> following operation may require exchange or global sum
          CUU(I)=CUU(I)/(BU(I,J)-AU(I,J)*CUU(IM))
cdm ==> following operation may require exchange or global sum
          URT(I)=(URT(I)-AU(I,J)*URT(IM))/(BU(I,J)-AU(I,J)*CUU(IM))
         END DO
         DO I=1,sNx-1
          J1=sNx-I
          J2=J1+1
cdm ==> following operation may require exchange or global sum
          URT(J1)=URT(J1)-CUU(J1)*URT(J2)
         END DO
         DO I=1,sNx
          UICE(I,J,1,bi,bj)=URT(I)
         END DO
1200    CONTINUE

        DO J=1,sNy
         DO I=1,sNx
          UICE(I,J,3,bi,bj)=UICE(I,J,1,bi,bj)
         END DO
        END DO

       ENDDO
      ENDDO

cdm ==> is this correct?  does it mean that tile boundaries
cdm ==> will be slightly different?
       CALL EXCH_RL( UICE, OLx, OLx, OLy, OLy, 3, OLx, OLy,
     I      FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

C NOW SECOND HALF

c$taf loop = parallel
      DO bj=myByLo(myThid),myByHi(myThid)
c$taf loop = parallel
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO I=1,sNx
         DO J=1,sNy
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          RADIUS2=ONE/(RADIUS*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))

          AA1=ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
          AA2=ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
          AA5=-(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &       -ETA(I,J,bi,bj)-ETA(I+1,J,bi,bj))*TNGICE(I,J,bi,bj)
          AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)
      
          AV(I,J)=(-AA2*DELY2+ETAMEAN*DELYR*(TNGICE(I,J-1,bi,bj)
     &           -TWO*TNGICE(I,J,bi,bj)))*UVM(I,J,bi,bj)
          BV(I,J)=((AA1+AA2)*DELY2+AA5*DELYR+AA6*RADIUS2
     &         +AMASS(I,J,bi,bj)/SEAICE_DT*TWO
     &         +DRAGS(I,J,bi,bj))*UVM(I,J,bi,bj)
     &         +(ONE-UVM(I,J,bi,bj))
          CV(I,J)=(-AA1*DELY2-ETAMEAN*DELYR*(TNGICE(I,J+1,bi,bj)
     &           -TWO*TNGICE(I,J,bi,bj)))*UVM(I,J,bi,bj)
         END DO
        END DO

        DO I=1,sNx
         AV(I,1)=ZERO
         CV(I,sNy)=ZERO
         CV(I,1)=CV(I,1)/BV(I,1)
        END DO

        DO I=1,sNx
         DO J=1,sNy
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          RADIUS2=ONE/(RADIUS*RADIUS)

          AA1=((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA2=((ETA(I,J,bi,bj)+ZETA(I,J,bi,bj))/CSUICE(I,J,bi,bj)
     &       +(ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj))
     &       /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)

          AA3=ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
          AA4=ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)

          IF(J.EQ.1) THEN
           AA9=( AA4*DELY2*UICEC(I,J-1,bi,bj)
     &     -ETAMEAN*DELYR*(TNGICE(I,J-1,bi,bj)-TWO*TNGICE(I,J,bi,bj))
     &     *UICEC(I,J-1,bi,bj) )*UVM(I,J,bi,bj)
          ELSE IF(J.EQ.sNy) THEN
           AA9=( AA3*DELY2*UICEC(I,J+1,bi,bj)
     &     +ETAMEAN*DELYR*(TNGICE(I,J+1,bi,bj)-TWO*TNGICE(I,J,bi,bj))
     &     *UICEC(I,J+1,bi,bj) )*UVM(I,J,bi,bj)
          ELSE
           AA9=ZERO
          END IF

          FXY1(I,J)=AA9+AMASS(I,J,bi,bj)/SEAICE_DT*UICE(I,J,1,bi,bj)*TWO
     5             -(AA1+AA2)*DELX2*UICE(I,J,1,bi,bj)
     6             +((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj)
     6             +ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     6             *UICE(I+1,J,1,bi,bj)
     6             +(ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     6             +ZETA(I,J+1,bi,bj))*UICE(I-1,J,1,bi,bj))
     6             *DELX2/CSUICE(I,J,bi,bj)/CSUICE(I,J,bi,bj)
      
         END DO
        END DO

        DO 1300 I=1,sNx
         DO J=1,sNy
          VRT(J)=FXY(I,J)+FXY1(I,J)
          VRT(J)=VRT(J)*UVM(I,J,bi,bj)
         END DO

         DO J=1,sNy
          CVV(J)=CV(I,J)
         END DO
         VRT(1)=VRT(1)/BV(I,1)
         DO J=2,sNy
          JM=J-1
          CVV(J)=CVV(J)/(BV(I,J)-AV(I,J)*CVV(JM))
cdm ==> following operation may require exchange or global sum
          VRT(J)=(VRT(J)-AV(I,J)*VRT(JM))/(BV(I,J)-AV(I,J)*CVV(JM))
         END DO
         DO J=1,sNy-1
          J1=sNy-J
          J2=J1+1
cdm ==> following operation may require exchange or global sum
          VRT(J1)=VRT(J1)-CVV(J1)*VRT(J2)
         END DO
         DO J=1,sNy
          UICE(I,J,1,bi,bj)=VRT(J)
         END DO
1300    CONTINUE

       ENDDO
      ENDDO

C SOLVE FOR VICE
C FIRST HALF FIRST

c$taf loop = parallel
      DO bj=myByLo(myThid),myByHi(myThid)
c$taf loop = parallel
       DO bi=myBxLo(myThid),myBxHi(myThid)

         DO I=1,sNx
          DO J=1,sNy
           DELXY=HALF/(DXUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELXR=HALF/(DXUICE(I,J,bi,bj)*RADIUS)
           DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
           ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
           ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &             +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))
  
           FXY(I,J)=-DRAGA(I,J,bi,bj)*UICEC(I,J,bi,bj)+FORCEY(I,J,bi,bj)
     3        +(HALF*(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     3        *(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     3        -ZETA(I,J,bi,bj)-ZETA(I+1,J,bi,bj))*DELXY
     3        /CSUICE(I,J,bi,bj)+HALF*ZETAMEAN*((UICEC(I+1,J+1,bi,bj)
     3        -UICEC(I-1,J+1,bi,bj))/CSUICE(I,J+1,bi,bj)
     3        -(UICEC(I+1,J-1,bi,bj)-UICEC(I-1,J-1,bi,bj))
     3        /CSUICE(I,J-1,bi,bj))*DELXY)
     3
     4        -(HALF*(UICEC(I+1,J,bi,bj)-UICEC(I-1,J,bi,bj))
     4        *(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     4        -ETA(I,J,bi,bj)-ETA(I+1,J,bi,bj))*DELXY
     4        /CSUICE(I,J,bi,bj)+HALF*ETAMEAN*((UICEC(I+1,J+1,bi,bj)
     4        -UICEC(I-1,J+1,bi,bj))/CSUICE(I,J+1,bi,bj)
     4        -(UICEC(I+1,J-1,bi,bj)-UICEC(I-1,J-1,bi,bj))
     4        /CSUICE(I,J-1,bi,bj))*DELXY)
     4
     5        +HALF*(ETA(I+1,J+1,bi,bj)*(UICEC(I+1,J+1,bi,bj)
     5        +UICEC(I,J+1,bi,bj)
     5        -UICEC(I+1,J,bi,bj)-UICEC(I,J,bi,bj))+ETA(I+1,J,bi,bj)
     5        *(UICEC(I+1,J,bi,bj)
     5        +UICEC(I,J,bi,bj)-UICEC(I+1,J-1,bi,bj)-UICEC(I,J-1,bi,bj))
     5        +ETA(I,J+1,bi,bj)
     5        *(UICEC(I,J,bi,bj)+UICEC(I-1,J,bi,bj)-UICEC(I,J+1,bi,bj)
     5        -UICEC(I-1,J+1,bi,bj))
     5        +ETA(I,J,bi,bj)*(UICEC(I,J-1,bi,bj)+UICEC(I-1,J-1,bi,bj)
     5        -UICEC(I,J,bi,bj)
     5        -UICEC(I-1,J,bi,bj)))*DELXY/CSUICE(I,J,bi,bj)
     5
     6        +(ETA(I+1,J+1,bi,bj)+ETA(I+1,J,bi,bj)
     6        -ETA(I,J,bi,bj)-ETA(I,J+1,bi,bj))
     6        *TNGICE(I,J,bi,bj)*UICEC(I,J,bi,bj)
     6        *DELXR/CSUICE(I,J,bi,bj)
     6        +ETAMEAN*TNGICE(I,J,bi,bj)*(UICEC(I+1,J,bi,bj)
     6        -UICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)
     6
     7        +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*(UICEC(I+1,J,bi,bj)
     7        -UICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)

          END DO
         END DO

         DO I=1,sNx
          DO J=1,sNy
           DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
           DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
           RADIUS2=ONE/(RADIUS*RADIUS)
           ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
           ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))
           AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)
     &        +ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
           AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
     &        +ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj)
           AA3=(ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA4=(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA5=((ZETA(I,J+1,bi,bj)-ETA(I,J+1,bi,bj))
     &        +(ZETA(I+1,J+1,bi,bj)-ETA(I+1,J+1,bi,bj))
     &        -(ZETA(I,J,bi,bj)-ETA(I,J,bi,bj))-(ZETA(I+1,J,bi,bj)
     &        -ETA(I+1,J,bi,bj)))*TNGICE(I,J,bi,bj)
           AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

           AV(I,J)=(-AA2*DELY2-(ZETAMEAN-ETAMEAN)*TNGICE(I,J-1,bi,bj)
     &          *DELYR-ETAMEAN*TWO*TNGICE(I,J,bi,bj)
     &          *DELYR)*UVM(I,J,bi,bj)
           BV(I,J)=((AA1+AA2)*DELY2+AA5*DELYR+AA6*RADIUS2
     &          +AMASS(I,J,bi,bj)/SEAICE_DT*TWO
     &          +DRAGS(I,J,bi,bj))*UVM(I,J,bi,bj)
     &          +(ONE-UVM(I,J,bi,bj))
           CV(I,J)=(-AA1*DELY2+(ZETAMEAN-ETAMEAN)*TNGICE(I,J+1,bi,bj)
     &          *DELYR+ETAMEAN*TWO*TNGICE(I,J,bi,bj)
     &          *DELYR)*UVM(I,J,bi,bj)
          END DO
         END DO
         DO I=1,sNx
          AV(I,1)=ZERO
          CV(I,sNy)=ZERO
          CV(I,1)=CV(I,1)/BV(I,1)
         END DO

c$taf loop = parallel
         DO 1301 I=1,sNx
          DO J=1,sNy
           DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
           DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
           RADIUS2=ONE/(RADIUS*RADIUS)
           ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
           ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))

           AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)
     &        +ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
           AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
     &        +ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj)
           AA3=(ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA4=(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA5=((ZETA(I,J+1,bi,bj)-ETA(I,J+1,bi,bj))
     &        +(ZETA(I+1,J+1,bi,bj)-ETA(I+1,J+1,bi,bj))
     &        -(ZETA(I,J,bi,bj)-ETA(I,J,bi,bj))
     &        -(ZETA(I+1,J,bi,bj)-ETA(I+1,J,bi,bj)))*TNGICE(I,J,bi,bj)
           AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

           IF(J.EQ.1) THEN
            AA9=(AA2*DELY2+(ZETAMEAN-ETAMEAN)*TNGICE(I,J-1,bi,bj)*DELYR
     &      +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     &      *VICEC(I,J-1,bi,bj)*UVM(I,J,bi,bj)
           ELSE IF(J.EQ.sNy) THEN
            AA9=(AA1*DELY2-(ZETAMEAN-ETAMEAN)*TNGICE(I,J+1,bi,bj)*DELYR
     &      -ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     &      *VICEC(I,J+1,bi,bj)*UVM(I,J,bi,bj)
           ELSE
            AA9=ZERO
           END IF

           VRT(J)=AA9+FXY(I,J)-(AA3+AA4)*DELX2*VICE(I,J,2,bi,bj)
     6        +((ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     6        /CSUICE(I,J,bi,bj))*VICE(I+1,J,2,bi,bj)*DELX2
     7        +(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     7        /CSUICE(I,J,bi,bj))*VICE(I-1,J,2,bi,bj)*DELX2)
     7        /CSUICE(I,J,bi,bj)
           VRT(J)=(VRT(J)+AMASS(I,J,bi,bj)/SEAICE_DT
     &          *VICE(I,J,2,bi,bj)*TWO)
     &          *UVM(I,J,bi,bj)
          END DO

          DO J=1,sNy
           CVV(J)=CV(I,J)
          END DO
          VRT(1)=VRT(1)/BV(I,1)
          DO J=2,sNy
           JM=J-1
           CVV(J)=CVV(J)/(BV(I,J)-AV(I,J)*CVV(JM))
cdm ==> following operation may require exchange or global sum
           VRT(J)=(VRT(J)-AV(I,J)*VRT(JM))/(BV(I,J)-AV(I,J)*CVV(JM))
          END DO
          DO J=1,sNy-1
           J1=sNy-J
           J2=J1+1
cdm ==> following operation may require exchange or global sum
           VRT(J1)=VRT(J1)-CVV(J1)*VRT(J2)
          END DO
          DO J=1,sNy
           VICE(I,J,1,bi,bj)=VRT(J)
          END DO
1301     CONTINUE

         DO J=1,sNy
          DO I=1,sNx
           VICE(I,J,3,bi,bj)=VICE(I,J,1,bi,bj)
          END DO
         END DO

       ENDDO
      ENDDO

cdm ==> is this correct?  does it mean that tile boundaries
cdm ==> will be slightly different?
       CALL EXCH_RL( VICE, OLx, OLx, OLy, OLy, 3, OLx, OLy,
     I      FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )

C NOW SECOND HALF

c$taf loop = parallel
      DO bj=myByLo(myThid),myByHi(myThid)
c$taf loop = parallel
       DO bi=myBxLo(myThid),myBxHi(myThid)

         DO J=1,sNy
          DO I=1,sNx
           DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
           DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
           RADIUS2=ONE/(RADIUS*RADIUS)
           ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
           ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &             +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))

           AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)
     &        +ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
           AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
     &        +ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj)
           AA3=(ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA4=(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

           AU(I,J)=-AA4*DELX2*UVM(I,J,bi,bj)
           BU(I,J)=((AA3+AA4)*DELX2+AA6*RADIUS2
     &          +AMASS(I,J,bi,bj)/SEAICE_DT*TWO
     &          +DRAGS(I,J,bi,bj))*UVM(I,J,bi,bj)
     &          +(ONE-UVM(I,J,bi,bj))
           CU(I,J)=-AA3*DELX2*UVM(I,J,bi,bj)
          END DO
         END DO
         DO J=1,sNy
          AU(1,J)=ZERO
          CU(sNx,J)=ZERO
          CU(1,J)=CU(1,J)/BU(1,J)
         END DO

         DO J=1,sNy
          DO I=1,sNx

           DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
           DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
           DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
           ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
           ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))

           AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)
     &        +ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
           AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)
     &        +ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj)
           AA3=(ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA4=(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     &        /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
           AA5=((ZETA(I,J+1,bi,bj)-ETA(I,J+1,bi,bj))
     &        +(ZETA(I+1,J+1,bi,bj)-ETA(I+1,J+1,bi,bj))
     &        -(ZETA(I,J,bi,bj)-ETA(I,J,bi,bj))-(ZETA(I+1,J,bi,bj)
     &        -ETA(I+1,J,bi,bj)))*TNGICE(I,J,bi,bj)
           AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

           IF(I.EQ.1) THEN
            AA9=AA4*DELX2*VICEC(I-1,J,bi,bj)*UVM(I,J,bi,bj)
           ELSE IF(I.EQ.sNx) THEN
            AA9=AA3*DELX2*VICEC(I+1,J,bi,bj)*UVM(I,J,bi,bj)
           ELSE
            AA9=ZERO
           END IF

           FXY1(I,J)=AA9+AMASS(I,J,bi,bj)/SEAICE_DT
     1        *VICE(I,J,1,bi,bj)*TWO
     1        -AA5*DELYR*VICE(I,J,1,bi,bj)
     1        -(AA1+AA2)*DELY2*VICE(I,J,1,bi,bj)
     1        +AA1*DELY2*VICE(I,J+1,1,bi,bj)-((ZETAMEAN-ETAMEAN)
     1        *TNGICE(I,J+1,bi,bj)*DELYR
     1        +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     1        *VICE(I,J+1,1,bi,bj)
     2        +AA2*DELY2*VICE(I,J-1,1,bi,bj)+((ZETAMEAN-ETAMEAN)
     2        *TNGICE(I,J-1,bi,bj)*DELYR
     2        +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     2        *VICE(I,J-1,1,bi,bj)
          END DO
         END DO

         DO 1201 J=1,sNy
          DO I=1,sNx
           URT(I)=FXY(I,J)+FXY1(I,J)
           URT(I)=URT(I)*UVM(I,J,bi,bj)
          END DO

          DO I=1,sNx
           CUU(I)=CU(I,J)
          END DO
          URT(1)=URT(1)/BU(1,J)
          DO I=2,sNx
           IM=I-1
           CUU(I)=CUU(I)/(BU(I,J)-AU(I,J)*CUU(IM))
cdm ==> following operation may require exchange or global sum
           URT(I)=(URT(I)-AU(I,J)*URT(IM))/(BU(I,J)-AU(I,J)*CUU(IM))
          END DO
          DO I=1,sNx-1
           J1=sNx-I
           J2=J1+1
cdm ==> following operation may require exchange or global sum
           URT(J1)=URT(J1)-CUU(J1)*URT(J2)
          END DO
          DO I=1,sNx
           VICE(I,J,1,bi,bj)=URT(I)
          END DO
1201    CONTINUE

       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          UICEC(I,J,bi,bj)=UICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
          VICEC(I,J,bi,bj)=VICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(UICEC, myThid)
      _EXCH_XY_R8(VICEC, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UICE(I,J,1,bi,bj)=UICEC(I,J,bi,bj)
          VICE(I,J,1,bi,bj)=VICEC(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif ALLOW_SEAICE

      RETURN
      END
