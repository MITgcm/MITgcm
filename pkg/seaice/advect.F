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

      INTEGER i, j, bi, bj
      INTEGER K3
      _RL  DELTT

      _RL UI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL VI   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL DIFFA(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

C NOW DECIDE IF BACKWARD EULER OR LEAPFROG
      IF(LAD.EQ.1) THEN
C LEAPFROG
         DELTT=DELTAT*TWO
         K3=3
      ELSE
C BACKWARD EULER
         DELTT=DELTAT
         K3=2
      ENDIF

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

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=0,sNy-1
         DO I=0,sNx-1
          HEFF(I+1,J+1,1,bi,bj)=HEFF(I+1,J+1,K3,bi,bj)
     &     -DELTT*((HEFF(I+1,J+1,2,bi,bj)+HEFF
     &     (I+2,J+1,2,bi,bj))*(UI(I+2,J+2)+UI(I+2,J+1))-
     &     (HEFF(I+1,J+1,2,bi,bj)+HEFF
     &     (I,J+1,2,bi,bj))*(UI(I+1,J+2)+UI(I+1,J+1)))
     &     *(QUART/(DXTICE(I+1,J,bi,bj)*CSTICE(I,J+1,bi,bj)))
     &     -DELTT*((HEFF(I+1,J+1,2,bi,bj)
     &     +HEFF(I+1,J+2,2,bi,bj))*(VI(I+1,J+2)
     &     +VI(I+2,J+2)*CSUICE(I+1,J+2,bi,bj)
     &     -(HEFF(I+1,J+1,2,bi,bj)+HEFF(I+1,J,2,bi,bj))
     &     *(VI(I+1,J+1)+VI(I+2,J+1))*CSUICE(I+1,J+1,bi,bj))
     &     *(QUART/(DYTICE(I,J+1,bi,bj)*CSTICE(I,J+1,bi,bj))))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER
      CALL SEAICE_EXCH ( HEFF, myThid )
      _BARRIER

      IF (LAD .EQ. 2) THEN

C NOW DO BACKWARD EULER CORRECTION
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             HEFF(I,J,3,bi,bj)=HEFF(I,J,2,bi,bj)
             HEFF(I,J,2,bi,bj)=HALF*(HEFF(I,J,1,bi,bj)
     &                             +HEFF(I,J,2,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO J=0,sNy-1
            DO I=0,sNx-1
             HEFF(I+1,J+1,1,bi,bj)=HEFF(I+1,J+1,3,bi,bj)
     &        -DELTT*((HEFF(I+1,J+1,2,bi,bj)+HEFF
     &        (I+2,J+1,2,bi,bj))*(UI(I+2,J+2)+UI(I+2,J+1))-
     &        (HEFF(I+1,J+1,2,bi,bj)+HEFF
     &        (I,J+1,2,bi,bj))*(UI(I+1,J+2)+UI(I+1,J+1)))
     &        *(QUART/(DXTICE(I+1,J,bi,bj)*CSTICE(I,J+1,bi,bj)))
     &        -DELTT*((HEFF(I+1,J+1,2,bi,bj)
     &        +HEFF(I+1,J+2,2,bi,bj))*(VI(I+1,J+2)
     &        +VI(I+2,J+2)*CSUICE(I+1,J+2,bi,bj)
     &        -(HEFF(I+1,J+1,2,bi,bj)+HEFF(I+1,J,2,bi,bj))
     &        *(VI(I+1,J+1)+VI(I+2,J+1))*CSUICE(I+1,J+1,bi,bj))
     &        *(QUART/(DYTICE(I,J+1,bi,bj)*CSTICE(I,J+1,bi,bj))))
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         _BARRIER
         CALL SEAICE_EXCH( HEFF, myThid )
         _BARRIER

C     NOW FIX UP H(I,J,2)
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             HEFF(I,J,2,bi,bj)=HEFF(I,J,3,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

      ENDIF

C NOW DO DIFFUSION ON H(I,J,3)
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

#endif /* ALLOW_SEAICE */

      RETURN
      END
