C $Header: /u/gcmpack/MITgcm/model/src/impldiff.F,v 1.1 1998/06/01 22:27:14 adcroft Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R IMPLDIFF                                             |
C     | o Step model fields forward in time                      |
C     \==========================================================/
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     KappaZT,
     I                     myThid )
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL KappaZT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      INTEGER myThid
C     == Local variables ==
      INTEGER i,j,k
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL ckm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)

      IF (Nz.GT.1) THEN		! Only need do anything if Nz>1
C Beginning of forward sweep (top level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         c(i,j)=-deltaTtracer*_rhFacC(i,j,1,bi,bj)*rdzF(1)
     &         *KappaZT(i,j,2)*rdzC(2)
         b(i,j)=1.-c(i,j)
         bet(i,j)=0.
         IF (b(i,j).NE.0.) bet(i,j)=1. / b(i,j)
         theta(i,j,1,bi,bj)=theta(i,j,1,bi,bj)*bet(i,j)
        ENDDO
       ENDDO
      ENDIF
C Middle of forward sweep
      IF (Nz.GT.2) THEN
       DO k=2,Nz-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          ckm1(i,j)=c(i,j)
          a(i,j)=-deltaTtracer*_rhFacC(i,j,k,bi,bj)*rdzF(k)
     &         *KappaZT(i,j, k )*rdzC( k )
          c(i,j)=-deltaTtracer*_rhFacC(i,j,k,bi,bj)*rdzF(k)
     &         *KappaZT(i,j,k+1)*rdzC(k+1)
          b(i,j)=1.-c(i,j)-a(i,j)
         ENDDO
        ENDDO
        DO j=jMin,jMax
         DO i=iMin,iMax
          gam(i,j,k)=ckm1(i,j)*bet(i,j)
          bet(i,j)=b(i,j)-a(i,j)*gam(i,j,k)
          IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
          theta(i,j,k,bi,bj)=(theta(i,j,k,bi,bj)
     &       -a(i,j)*theta(i,j,k-1,bi,bj))*bet(i,j)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF (Nz.GT.1) THEN
C End of forward sweep (bottom level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         ckm1(i,j)=c(i,j)
         a(i,j)=-deltaTtracer*_rhFacC(i,j,Nz,bi,bj)*rdzF(Nz)
     &        *KappaZT(i,j, Nz )*rdzC( Nz )
         b(i,j)=1.-a(i,j)
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         gam(i,j,Nz)=ckm1(i,j)*bet(i,j)
         bet(i,j)=b(i,j)-a(i,j)*gam(i,j,Nz)
         IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
         theta(i,j,Nz,bi,bj)=(theta(i,j,Nz,bi,bj)
     &      -a(i,j)*theta(i,j,Nz-1,bi,bj))*bet(i,j)
        ENDDO
       ENDDO
C Backward sweep
       DO k=Nz-1,1,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          theta(i,j,k,bi,bj)=theta(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*theta(i,j,k+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
