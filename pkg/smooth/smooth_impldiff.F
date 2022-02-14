#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     deltaTX, KappaRX, recip_hFac,
     U                     gXNm1,
     I                     myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_impldiff
C     | o Copy of model/src/impldiff
C     |    (simplified and with specified time step)
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL deltaTX
      _RL KappaRX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gXnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i,j,k
      _RL gYnm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL a(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL bet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gam(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
CEOP

C--   Initialise
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         gYNm1(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C--   Old aLower
      DO j=jMin,jMax
       DO i=iMin,iMax
         a(i,j,1) = 0. _d 0
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          a(i,j,k) = -deltaTX*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j, k )*recip_drC( k )
     &               *deepFac2F(k)*rhoFacF(k)
          IF (recip_hFac(i,j,k-1,bi,bj).EQ.0.) a(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO

C--   Old aUpper
      DO k=1,Nr-1
       DO j=jMin,jMax
        DO i=iMin,iMax
          c(i,j,k) = -deltaTX*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &               *recip_deepFac2C(k)*recip_rhoFacC(k)
     &               *KappaRX(i,j,k+1)*recip_drC(k+1)
     &               *deepFac2F(k+1)*rhoFacF(k+1)
          IF (recip_hFac(i,j,k+1,bi,bj).EQ.0.) c(i,j,k)=0.
        ENDDO
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin,iMax
         c(i,j,Nr) = 0. _d 0
       ENDDO
      ENDDO

C--   Old aCenter
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          b(i,j,k) = 1. _d 0 - c(i,j,k) - a(i,j,k)
        ENDDO
       ENDDO
      ENDDO

C--   Old and new gam, bet are the same
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
          bet(i,j,k) = 1. _d 0
          gam(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C--   Only need do anything if Nr>1
      IF (Nr.GT.1) THEN

       k = 1
C--    Beginning of forward sweep (top level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF (b(i,j,1).NE.0.) bet(i,j,1) = 1. _d 0 / b(i,j,1)
        ENDDO
       ENDDO

      ENDIF

C--   Middle of forward sweep
      IF (Nr.GE.2) THEN

CADJ loop = sequential
       DO k=2,Nr

        DO j=jMin,jMax
         DO i=iMin,iMax
          gam(i,j,k) = c(i,j,k-1)*bet(i,j,k-1)
          IF ( ( b(i,j,k) - a(i,j,k)*gam(i,j,k) ) .NE. 0.)
     &        bet(i,j,k) = 1. _d 0 / ( b(i,j,k) - a(i,j,k)*gam(i,j,k) )
         ENDDO
        ENDDO

       ENDDO

      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        gYNm1(i,j,1,bi,bj) = gXNm1(i,j,1,bi,bj)*bet(i,j,1)
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         gYnm1(i,j,k,bi,bj) = bet(i,j,k)*
     &        (gXnm1(i,j,k,bi,bj) - a(i,j,k)*gYnm1(i,j,k-1,bi,bj))
        ENDDO
       ENDDO
      ENDDO

C--    Backward sweep
CADJ loop = sequential
       DO k=Nr-1,1,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          gYnm1(i,j,k,bi,bj)=gYnm1(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*gYnm1(i,j,k+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO

       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          gXnm1(i,j,k,bi,bj)=gYnm1(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO

      RETURN
      END
