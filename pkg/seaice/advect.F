C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE advect( UICE,VICE,HEFF,HEFFM,myThid )
C     /==========================================================\
C     | SUBROUTINE advect                                        |
C     | o Calculate ice advection                                |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      INTEGER K2, K3, LL, KD
      _RL  DELTT

      _RL UI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL VI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL DIFFA(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C NOW DECIDE IF BACKWARD EULER OR LEAPFROG
      LL=LAD
      IF(LL.EQ.1) GO TO 100
C BACKWARD EULER
      DELTT=DELTAT
      K3=2
      K2=2
      GO TO 101
C LEAPFROG
 100  DELTT=DELTAT*2.0
      K3=3
      K2=2
 101  CONTINUE

C NOW REARRANGE H'S

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          UI(I,J)=UICE(I,J,1,bi,bj)
          VI(I,J)=VICE(I,J,1,bi,bj)
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,3,bi,bj)=HEFF(I,J,2,bi,bj)
          HEFF(I,J,2,bi,bj)=HEFF(I,J,1,bi,bj)
         ENDDO
        ENDDO

       ENDDO
      ENDDO

 202  CONTINUE

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=0,sNy-1
         DO I=0,sNx-1
          HEFF(I+1,J+1,1,bi,bj)=HEFF(I+1,J+1,K3,bi,bj)
     &     -DELTT*((HEFF(I+1,J+1,2,bi,bj)+HEFF
     &     (I+2,J+1,2,bi,bj))*(UI(I+1,J+1)+UI(I+1,J))-
     &     (HEFF(I+1,J+1,2,bi,bj)+HEFF
     &     (I,J+1,2,bi,bj))*(UI(I,J+1)+UI(I,J)))
     &     *(0.25/(DXTICE(I+1,J,bi,bj)*CSTICE(I,J+1,bi,bj)))
     &     -DELTT*((HEFF(I+1,J+1,2,bi,bj)
     &     +HEFF(I+1,J+2,2,bi,bj))*(VI(I,J+1)
     &     +VI(I+1,J+1)*CSUICE(I,J+1,bi,bj)
     &     -(HEFF(I+1,J+1,2,bi,bj)+HEFF(I+1,J,2,bi,bj))
     &     *(VI(I,J)+VI(I+1,J))*CSUICE(I,J,bi,bj))
     &     *(0.25/(DYTICE(I,J+1,bi,bj)*CSTICE(I,J+1,bi,bj))))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER
      CALL EXCH_RL( HEFF, OLx, OLx, OLy, OLy, 3, OLx, OLy,
     I     FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
      _BARRIER

C NOW DECIDE IF DONE
      IF(LL.EQ.2) GO TO 99
      IF (LL.EQ.3) GO TO 89
      GO TO 102
 89   CONTINUE

C  NOW FIX UP H(I,J,2)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,2,bi,bj)=HEFF(I,J,3,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      GO TO 102
 99   CONTINUE

C NOW DO BACKWARD EULER CORRECTION
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,3,bi,bj)=HEFF(I,J,2,bi,bj)
          HEFF(I,J,2,bi,bj)=0.5*(HEFF(I,J,1,bi,bj)
     &                          +HEFF(I,J,2,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      LL=3
      K3=3
      GO TO 202
 102  CONTINUE
C NOW DO DIFFUSION ON H(I,J,K3)
      DO 240 KD=1,2
      GO TO (241,242),KD
 241  CONTINUE

C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J)=DIFF1*MIN(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj)
     &                        ,DYTICE(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)
      GO TO 243
 242  CONTINUE

C NOW CALCULATE DIFFUSION COEF ROUGHLY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DIFFA(I,J)=-(MIN(DXTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj)
     &                    ,DYTICE(I,J,bi,bj)))**2/DELTT
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL DIFFUS(HEFF,DIFFA,HEFFM,DELTT, myThid)
 243  CONTINUE

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,1,bi,bj)=(HEFF(I,J,1,bi,bj)+HEFF(I,J,3,bi,bj))
     &                      *HEFFM(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

 240  CONTINUE

#endif ALLOW_SEAICE

      RETURN
      END
