C $Header:

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE lsr( myThid )
C     /==========================================================\
C     | SUBROUTINE  lsr                                          |
C     | o Solve ice momentum equation with an LSR method         |
C     |   (see Zhang and Hibler, JGR, 102, 8691-8702, 1997)      |
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
#ifdef SEAICE_ALLOW_DYNAMICS

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj, j1, j2, im, jm, icou
      _RL  RADIUS, DELXY, DELXR, DELYR, DELX2, DELY2, RADIUS2
      _RL  ETAMEAN, ZETAMEAN, WFAU, WFAV
      _RL  AA1, AA2, AA3, AA4, AA5, AA6, S1, S2

      _RL AU   (1:sNx,1:sNy), BU   (1:sNx,1:sNy), CU   (1:sNx,1:sNy)
      _RL AV   (1:sNx,1:sNy), BV   (1:sNx,1:sNy), CV   (1:sNx,1:sNy)
      _RL UERR (1:sNx,1:sNy,nSx,nSy)
      _RL FXY  (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

      _RL URT(1:sNx), VRT(1:sNy), CUU(1:sNx), CVV(1:sNy)

C SET SOME VALUES
      RADIUS=6370. _d 3
      ICOUNT=0
      ICOU=0
      WFAU=0.95 _d 0
      WFAV=0.95 _d 0

C SOLVE FOR UICE

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)
     &                      +AMASS(I,J,bi,bj)/DELTAT*UICE(I,J,2,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)
     &                      +AMASS(I,J,bi,bj)/DELTAT*VICE(I,J,2,bi,bj)
          FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj)*UVM(I,J,bi,bj)
          FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj)*UVM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          RADIUS2=ONE/(RADIUS*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          AA1=((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj))/CSTICE(I,J,bi,bj)
     &           +(ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     &           /CSTICE(I,J+1,bi,bj))/CSUICE(I,J,bi,bj)
          AA2=((ETA(I,J,bi,bj)+ZETA(I,J,bi,bj))/CSTICE(I,J,bi,bj)
     &           +(ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj))
     &           /CSTICE(I,J+1,bi,bj))/CSUICE(I,J,bi,bj)
          AA3=ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
          AA4=ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
          AA5=-(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)-ETA(I,J,bi,bj)
     &           -ETA(I+1,J,bi,bj))*TNGICE(I,J,bi,bj)
          AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)
          AU(I,J)=-AA2*DELX2*UVM(I,J,bi,bj)
          BU(I,J)=((AA1+AA2)*DELX2+(AA3+AA4)*DELY2+AA5*DELYR+AA6*RADIUS2
     &           +AMASS(I,J,bi,bj)/DELTAT+DRAGS(I,J,bi,bj))
     &           *UVM(I,J,bi,bj)+(ONE-UVM(I,J,bi,bj))
          CU(I,J)=-AA1*DELX2*UVM(I,J,bi,bj)
         END DO
        END DO

        DO J=1,sNy
         AU(1,J)=ZERO
         CU(sNx,J)=ZERO
         CU(1,J)=CU(1,J)/BU(1,J)
        END DO

        DO J=1,sNy
         DO I=1,sNx
          DELXY=HALF/(DXUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELXR=HALF/(DXUICE(I,J,bi,bj)*RADIUS)
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &            +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &            +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))
          AA1=((ETA(I+1,J,bi,bj)+ZETA(I+1,J,bi,bj))/CSUICE(I,J,bi,bj)
     &            +(ETA(I+1,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj))
     &            /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA2=((ETA(I,J,bi,bj)+ZETA(I,J,bi,bj))/CSUICE(I,J,bi,bj)
     &            +(ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj))
     &            /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)

          IF(I.EQ.1) THEN
           AA3=AA2*DELX2*UICE(I-1,J,1,bi,bj)*UVM(I,J,bi,bj)
          ELSE IF(I.EQ.sNx) THEN
           AA3=AA1*DELX2*UICE(I+1,J,1,bi,bj)*UVM(I,J,bi,bj)
          ELSE
           AA3=ZERO
          END IF

          FXY(I,J)=AA3+DRAGA(I,J,bi,bj)*VICEC(I,J,bi,bj)
     3            +FORCEX(I,J,bi,bj)
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
     5            +HALF*(ETA(I+1,J+1,bi,bj)/CSTICE(I,J+1,bi,bj)
     5            *(VICEC(I+1,J+1,bi,bj)+VICEC(I+1,J,bi,bj)
     5            -VICEC(I,J+1,bi,bj)-VICEC(I,J,bi,bj))
     5            +ETA(I,J+1,bi,bj)/CSTICE(I,J+1,bi,bj)
     5            *(VICEC(I,J+1,bi,bj)
     5            +VICEC(I,J,bi,bj)-VICEC(I-1,J+1,bi,bj)
     5            -VICEC(I-1,J,bi,bj))
     5            +ETA(I+1,J,bi,bj)/CSTICE(I,J,bi,bj)
     5            *(VICEC(I,J,bi,bj)+VICEC(I,J-1,bi,bj)
     5            -VICEC(I+1,J,bi,bj)-VICEC(I+1,J-1,bi,bj))
     5            +ETA(I,J,bi,bj)/CSTICE(I,J,bi,bj)*(VICEC(I-1,J,bi,bj)
     5            +VICEC(I-1,J-1,bi,bj)-VICEC(I,J,bi,bj)
     5            -VICEC(I,J-1,bi,bj)))*DELXY
     5
     6            -((ZETA(I+1,J+1,bi,bj)+ZETA(I+1,J,bi,bj)
     6            -ZETA(I,J,bi,bj)-ZETA(I,J+1,bi,bj))
     6            +(ETA(I+1,J+1,bi,bj)+ETA(I+1,J,bi,bj)-ETA(I,J,bi,bj)
     6            -ETA(I,J+1,bi,bj)))
     6            *TNGICE(I,J,bi,bj)*VICEC(I,J,bi,bj)*DELXR
     6            /CSUICE(I,J,bi,bj)
     6            -(ETAMEAN+ZETAMEAN)*TNGICE(I,J,bi,bj)
     6            *(VICEC(I+1,J,bi,bj)-VICEC(I-1,J,bi,bj))
     6            *DELXR/CSUICE(I,J,bi,bj)
     6
     7            -ETAMEAN*TWO*TNGICE(I,J,bi,bj)*(VICEC(I+1,J,bi,bj)
     7            -VICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)
         END DO
        END DO

       ENDDO
      ENDDO

C NOW DO ITERATION
100   CONTINUE
        
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C NOW SET U(3)=U(1)
        DO J=1,sNy
         DO I=1,sNx
          UICE(I,J,3,bi,bj)=UICE(I,J,1,bi,bj)
         END DO
        END DO

        DO 1200 J=1,sNy
         DO I=1,sNx
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &           +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          URT(I)=FXY(I,J)
     1       +(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj))
     1       *UICE(I,J+1,1,bi,bj)*DELY2
     2       +(ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
     2       *UICE(I,J-1,1,bi,bj)*DELY2
     3       +ETAMEAN*DELYR*(UICE(I,J+1,1,bi,bj)*TNGICE(I,J+1,bi,bj)
     3       -UICE(I,J-1,1,bi,bj)*TNGICE(I,J-1,bi,bj))
     4       -ETAMEAN*DELYR*TWO*TNGICE(I,J,bi,bj)*(UICE(I,J+1,1,bi,bj)
     4       -UICE(I,J-1,1,bi,bj))
          URT(I)=URT(I)*UVM(I,J,bi,bj)
         END DO

         DO I=1,sNx
          CUU(I)=CU(I,J)
         END DO
         URT(1)=URT(1)/BU(1,J)
         DO I=2,sNx
          IM=I-1
          CUU(I)=CUU(I)/(BU(I,J)-AU(I,J)*CUU(IM))
          URT(I)=(URT(I)-AU(I,J)*URT(IM))/(BU(I,J)-AU(I,J)*CUU(IM))
         END DO
         DO I=1,sNx-1
          J1=sNx-I
          J2=J1+1
          URT(J1)=URT(J1)-CUU(J1)*URT(J2)
         END DO
         DO I=1,sNx
          UICE(I,J,1,bi,bj)=UICE(I,J,3,bi,bj)
     &        +WFAU*(URT(I)-UICE(I,J,3,bi,bj))
         END DO

1200    CONTINUE

       ENDDO
      ENDDO

      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.1500) GO TO 201
 202  CONTINUE
C NOW CHECK MAX ERROR
C The following loop has to be global operation
      S1=ZERO
C FORM ERROR MATRIX
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          UERR(I,J,bi,bj)=(UICE(I,J,1,bi,bj)-UICE(I,J,3,bi,bj))
     &             *UVM(I,J,bi,bj)
          S1=MAX(ABS(UERR(I,J,bi,bj)),S1)
         END DO
        END DO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8( S1, myThid )
C NOW FIND ERROR
      IF(S1.LT.LSR_ERROR) GO TO 200
      CALL SEAICE_EXCH ( UICE, myThid )
      GO TO 100
 201  CONTINUE
      PRINT 11
 200  CONTINUE
      write(*,'(A,I6,1P2E22.14)')' lsr iters, error = ',ICOUNT,S1

C NOW FOR VICE
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          RADIUS2=ONE/(RADIUS*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &          +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &          +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))
          AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &          +ZETA(I+1,J+1,bi,bj)
          AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
     &          +ZETA(I+1,J,bi,bj)
          AA3=(ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     &          /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA4=(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     &          /CSUICE(I,J,bi,bj))/CSUICE(I,J,bi,bj)
          AA5=((ZETA(I,J+1,bi,bj)-ETA(I,J+1,bi,bj))
     &          +(ZETA(I+1,J+1,bi,bj)-ETA(I+1,J+1,bi,bj))
     &          -(ZETA(I,J,bi,bj)-ETA(I,J,bi,bj))-(ZETA(I+1,J,bi,bj)
     &          -ETA(I+1,J,bi,bj)))*TNGICE(I,J,bi,bj)
          AA6=TWO*ETAMEAN*TNGICE(I,J,bi,bj)*TNGICE(I,J,bi,bj)

          AV(I,J)=(-AA2*DELY2
     &           -(ZETAMEAN-ETAMEAN)*TNGICE(I,J-1,bi,bj)*DELYR
     &           -ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)*UVM(I,J,bi,bj)
          BV(I,J)=((AA1+AA2)*DELY2+(AA3+AA4)*DELX2+AA5*DELYR+AA6*RADIUS2
     &           +AMASS(I,J,bi,bj)/DELTAT+DRAGS(I,J,bi,bj))
     &           *UVM(I,J,bi,bj)+(ONE-UVM(I,J,bi,bj))
          CV(I,J)=(-AA1*DELY2
     &           +(ZETAMEAN-ETAMEAN)*TNGICE(I,J+1,bi,bj)*DELYR
     &           +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)*UVM(I,J,bi,bj)
         END DO
        END DO

        DO I=1,sNx
         AV(I,1)=ZERO
         CV(I,sNy)=ZERO
         CV(I,1)=CV(I,1)/BV(I,1)
        END DO

        DO J=1,sNy
         DO I=1,sNx
          DELXY=HALF/(DXUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELXR=HALF/(DXUICE(I,J,bi,bj)*RADIUS)
          DELY2=HALF/(DYUICE(I,J,bi,bj)*DYUICE(I,J,bi,bj))
          DELYR=HALF/(DYUICE(I,J,bi,bj)*RADIUS)
          ETAMEAN=QUART*(ETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        +ETA(I,J,bi,bj)+ETA(I+1,J,bi,bj))
          ZETAMEAN=QUART*(ZETA(I,J+1,bi,bj)+ZETA(I+1,J+1,bi,bj)
     &        +ZETA(I,J,bi,bj)+ZETA(I+1,J,bi,bj))
          AA1=ETA(I,J+1,bi,bj)+ZETA(I,J+1,bi,bj)+ETA(I+1,J+1,bi,bj)
     &        +ZETA(I+1,J+1,bi,bj)
          AA2=ETA(I,J,bi,bj)+ZETA(I,J,bi,bj)+ETA(I+1,J,bi,bj)
     &        +ZETA(I+1,J,bi,bj)

           IF(J.EQ.1) THEN
            AA3=(AA2*DELY2+(ZETAMEAN-ETAMEAN)*TNGICE(I,J-1,bi,bj)*DELYR
     &      +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     &      *VICE(I,J-1,1,bi,bj)*UVM(I,J,bi,bj)
           ELSE IF(J.EQ.sNy) THEN
            AA3=(AA1*DELY2-(ZETAMEAN-ETAMEAN)*TNGICE(I,J+1,bi,bj)*DELYR
     &      -ETAMEAN*TWO*TNGICE(I,J,bi,bj)*DELYR)
     &      *VICE(I,J+1,1,bi,bj)*UVM(I,J,bi,bj)
           ELSE
            AA3=ZERO
           END IF

           FXY(I,J)=AA3-DRAGA(I,J,bi,bj)*UICEC(I,J,bi,bj)
     2        +FORCEY(I,J,bi,bj)
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
     4        -ETA(I,J,bi,bj)-ETA(I+1,J,bi,bj))*DELXY/CSUICE(I,J,bi,bj)
     4        +HALF*ETAMEAN*((UICEC(I+1,J+1,bi,bj)
     4        -UICEC(I-1,J+1,bi,bj))/CSUICE(I,J+1,bi,bj)
     4        -(UICEC(I+1,J-1,bi,bj)-UICEC(I-1,J-1,bi,bj))
     4        /CSUICE(I,J-1,bi,bj))*DELXY)
     4
     5        +HALF*(ETA(I+1,J+1,bi,bj)*(UICEC(I+1,J+1,bi,bj)
     5        +UICEC(I,J+1,bi,bj)
     5        -UICEC(I+1,J,bi,bj)-UICEC(I,J,bi,bj))+ETA(I+1,J,bi,bj)
     5        *(UICEC(I+1,J,bi,bj)
     5        +UICEC(I,J,bi,bj)-UICEC(I+1,J-1,bi,bj)
     5        -UICEC(I,J-1,bi,bj))+ETA(I,J+1,bi,bj)
     5        *(UICEC(I,J,bi,bj)+UICEC(I-1,J,bi,bj)-UICEC(I,J+1,bi,bj)
     5        -UICEC(I-1,J+1,bi,bj))
     5        +ETA(I,J,bi,bj)*(UICEC(I,J-1,bi,bj)+UICEC(I-1,J-1,bi,bj)
     5        -UICEC(I,J,bi,bj)
     5        -UICEC(I-1,J,bi,bj)))*DELXY/CSUICE(I,J,bi,bj)
     5
     6        +(ETA(I+1,J+1,bi,bj)+ETA(I+1,J,bi,bj)-ETA(I,J,bi,bj)
     6        -ETA(I,J+1,bi,bj))
     6        *TNGICE(I,J,bi,bj)*UICEC(I,J,bi,bj)
     6        *DELXR/CSUICE(I,J,bi,bj)
     6        +ETAMEAN*TNGICE(I,J,bi,bj)*(UICEC(I+1,J,bi,bj)
     6        -UICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)
     6
     7        +ETAMEAN*TWO*TNGICE(I,J,bi,bj)*(UICEC(I+1,J,bi,bj)
     7        -UICEC(I-1,J,bi,bj))*DELXR/CSUICE(I,J,bi,bj)

         END DO
        END DO

       ENDDO
      ENDDO

C NOW DO ITERATION
300   CONTINUE
C NOW SET U(3)=U(1)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO J=1,sNy
         DO I=1,sNx
          VICE(I,J,3,bi,bj)=VICE(I,J,1,bi,bj) 
         END DO
        END DO

        DO 1300 I=1,sNx
         DO J=1,sNy
          DELX2=HALF/(DXUICE(I,J,bi,bj)*DXUICE(I,J,bi,bj))
          VRT(J)=FXY(I,J)
     6      +((ETA(I+1,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I+1,J+1,bi,bj)
     6      /CSUICE(I,J,bi,bj))*VICE(I+1,J,1,bi,bj)*DELX2
     7      +(ETA(I,J,bi,bj)/CSUICE(I,J,bi,bj)+ETA(I,J+1,bi,bj)
     7      /CSUICE(I,J,bi,bj))*VICE(I-1,J,1,bi,bj)*DELX2)
     7      /CSUICE(I,J,bi,bj)
          VRT(J)=VRT(J)*UVM(I,J,bi,bj)
         END DO

         DO J=1,sNy
          CVV(J)=CV(I,J)
         END DO
         VRT(1)=VRT(1)/BV(I,1)
         DO J=2,sNy
          JM=J-1
          CVV(J)=CVV(J)/(BV(I,J)-AV(I,J)*CVV(JM))
          VRT(J)=(VRT(J)-AV(I,J)*VRT(JM))/(BV(I,J)-AV(I,J)*CVV(JM))
         END DO
         DO J=1,sNy-1
          J1=sNy-J
          J2=J1+1
          VRT(J1)=VRT(J1)-CVV(J1)*VRT(J2)
         END DO
         DO J=1,sNy
          VICE(I,J,1,bi,bj)=VICE(I,J,3,bi,bj)
     &        +WFAV*(VRT(J)-VICE(I,J,3,bi,bj))
         END DO
1300    CONTINUE

       ENDDO
      ENDDO

      ICOU=ICOU+1                                                  
      IF(ICOU.GT.1500) GO TO 301
C NOW CHECK MAX ERROR
C The following loop has to be global operation
      S2=ZERO
C FORM ERROR MATRIX
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          UERR(I,J,bi,bj)=(VICE(I,J,1,bi,bj)-VICE(I,J,3,bi,bj))
     &             *UVM(I,J,bi,bj)
          S2=MAX(ABS(UERR(I,J,bi,bj)),S2)
         END DO
        END DO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8( S2, myThid )
C NOW FIND ERROR
      IF(S2.LT.LSR_ERROR) GO TO 400
      CALL SEAICE_EXCH( VICE, myThid )
      GO TO 300
 301  CONTINUE
      PRINT 121
 11   FORMAT(1X,'NO CONVERGENCE FOR U AFTER 1500 ITERATIONS')
 121  FORMAT(1X,'NO CONVERGENCE FOR V AFTER 1500 ITERATIONS')
400   CONTINUE
      write(*,'(A,I6,1P2E22.14)')' lsr iters, error = ',ICOU,S2

C NOW END
C NOW MAKE COROLIS TERM IMPLICIT
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          UICE(I,J,1,bi,bj)=UICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
          VICE(I,J,1,bi,bj)=VICE(I,J,1,bi,bj)*UVM(I,J,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO
      CALL SEAICE_EXCH( UICE, myThid )
      CALL SEAICE_EXCH( VICE, myThid )

#endif /* SEAICE_ALLOW_DYNAMICS */
#endif /* ALLOW_SEAICE */

      RETURN
      END
