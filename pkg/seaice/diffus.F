C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE diffus( HEFF, DIFFA, HEFFM, DELTT, myThid )
C     /==========================================================\
C     | SUBROUTINE diffus                                        |
C     | o Add diffusion terms to ice mass conservation equations |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE_GRID.h"
 
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DIFFA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL DELTT
      INTEGER myThid
CEndOfInterface
 
#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL DELTXX1, DELTYY1, DELTXX, DELTYY
      _RL HEFF1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF1(I,J,bi,bj)=0.0 _d 0
         ENDDO
        ENDDO

C NOW DO DIFFUSION WITH NUIT CONVERSION
        DO j=1,sNy
         DO i=1,sNx
          DELTXX1=DELTT*DIFFA(I,J,bi,bj)
          DELTYY1=DELTT*DIFFA(I,J,bi,bj)
          DELTXX=DELTXX1/(DXTICE(I,J,bi,bj)*DXTICE(I,J,bi,bj)
     &          *CSTICE(I,J,bi,bj)*CSTICE(I,J,bi,bj))
          DELTYY=DELTYY1/(DYTICE(I,J,bi,bj)*DYTICE(I,J,bi,bj)
     &          *CSTICE(I,J,bi,bj))
          HEFF1(I,J,bi,bj)=DELTXX*((HEFF(I+1,J,3,bi,bj)
     &     -HEFF(I,J,3,bi,bj))*HEFFM(I+1,J,bi,bj)
     &     -(HEFF(I,J,3,bi,bj)-HEFF(I-1,J,3,bi,bj))*HEFFM(I-1,J,bi,bj))
     &     +DELTYY*((HEFF(I,J+1,3,bi,bj)-HEFF(I,J,3,bi,bj))
     &     *CSUICE(I+1,J+1,bi,bj)*HEFFM(I,J+1,bi,bj)
     &     -(HEFF(I,J,3,bi,bj)-HEFF(I,J-1,3,bi,bj))
     &     *CSUICE(I+1,J,bi,bj)*HEFFM(I,J-1,bi,bj))
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      _EXCH_XY_R8(HEFF1, myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,3,bi,bj)=HEFF1(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_SEAICE */

      RETURN
      END
