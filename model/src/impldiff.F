C $Header: /u/gcmpack/MITgcm/model/src/impldiff.F,v 1.8 1999/05/18 18:01:13 adcroft Exp $

#include "CPP_OPTIONS.h"

C     /==========================================================\
C     | S/R IMPLDIFF                                             |
C     | o Solve implicit diffusion equation for vertical         |
C     |   diffusivity.                                           |
C     \==========================================================/
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     deltaTX,KappaRX,recip_hFac,
     U                     gXNm1,
     I                     myThid )
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL deltaTX
      _RL KappaRX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RS recip_hFac(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL gXnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,k
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL ckm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)

      IF (Nr.GT.1) THEN		! Only need do anything if Nr>1
C--    Beginning of forward sweep (top level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         c(i,j)=-deltaTX*recip_hFac(i,j,1,bi,bj)*recip_drF(1)
     &         *KappaRX(i,j,2)*recip_drC(2)
         b(i,j)=1.-c(i,j)
         bet(i,j)=0.
         IF (b(i,j).NE.0.) bet(i,j)=1. / b(i,j)
         gXnm1(i,j,1,bi,bj)=gXnm1(i,j,1,bi,bj)*bet(i,j)
        ENDDO
       ENDDO
      ENDIF
C--   Middle of forward sweep
      IF (Nr.GT.2) THEN
       DO k=2,Nr-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          ckm1(i,j)=c(i,j)
          a(i,j)=-deltaTX*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &         *KappaRX(i,j, k )*recip_drC( k )
          c(i,j)=-deltaTX*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &         *KappaRX(i,j,k+1)*recip_drC(k+1)
          b(i,j)=1.-c(i,j)-a(i,j)
         ENDDO
        ENDDO
        DO j=jMin,jMax
         DO i=iMin,iMax
          gam(i,j,k)=ckm1(i,j)*bet(i,j)
          bet(i,j)=b(i,j)-a(i,j)*gam(i,j,k)
          IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
          gXnm1(i,j,k,bi,bj)=(gXnm1(i,j,k,bi,bj)
     &       -a(i,j)*gXnm1(i,j,k-1,bi,bj))*bet(i,j)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF (Nr.GT.1) THEN
C--    End of forward sweep (bottom level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         ckm1(i,j)=c(i,j)
         a(i,j)=-deltaTX*recip_hFac(i,j,Nr,bi,bj)*recip_drF(Nr)
     &        *KappaRX(i,j, Nr )*recip_drC( Nr )
         b(i,j)=1.-a(i,j)
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         gam(i,j,Nr)=ckm1(i,j)*bet(i,j)
         bet(i,j)=b(i,j)-a(i,j)*gam(i,j,Nr)
         IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
         gXnm1(i,j,Nr,bi,bj)=(gXnm1(i,j,Nr,bi,bj)
     &      -a(i,j)*gXnm1(i,j,Nr-1,bi,bj))*bet(i,j)
        ENDDO
       ENDDO
C--    Backward sweep
       DO k=Nr-1,1,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          gXnm1(i,j,k,bi,bj)=gXnm1(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*gXnm1(i,j,k+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
