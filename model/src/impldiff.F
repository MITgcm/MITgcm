C $Header: /u/gcmpack/MITgcm/model/src/impldiff.F,v 1.5 1998/08/20 20:21:23 cnh Exp $

#include "CPP_EEOPTIONS.h"

C     /==========================================================\
C     | S/R IMPLDIFF                                             |
C     | o Solve implicit diffusion equation for vertical         |
C     |   diffusivity.                                           |
C     \==========================================================/
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     KappaRT,KappaRS,
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
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,k
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL ckm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nz)

C ***************************************************************
C *****************                              ****************
C *****************           N O T E            ****************
C *****************                              ****************
C ***************************************************************
C
C The implicit diffusion of SALT currently uses the diffusivity
C of the THETA.
C                  ie. KappaRS is ignored.
C
C ***************************************************************
C ***************************************************************

      IF (Nz.GT.1) THEN		! Only need do anything if Nz>1
C--    Beginning of forward sweep (top level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         c(i,j)=-deltaTtracer*_rhFacC(i,j,1,bi,bj)*recip_drF(1)
     &         *KappaRT(i,j,2)*recip_drC(2)
         b(i,j)=1.-c(i,j)
         bet(i,j)=0.
         IF (b(i,j).NE.0.) bet(i,j)=1. / b(i,j)
         gTnm1(i,j,1,bi,bj)=gTnm1(i,j,1,bi,bj)*bet(i,j)
         gSnm1(i,j,1,bi,bj)=gSnm1(i,j,1,bi,bj)*bet(i,j)
        ENDDO
       ENDDO
      ENDIF
C--   Middle of forward sweep
      IF (Nz.GT.2) THEN
       DO k=2,Nz-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          ckm1(i,j)=c(i,j)
          a(i,j)=-deltaTtracer*_rhFacC(i,j,k,bi,bj)*recip_drF(k)
     &         *KappaRT(i,j, k )*recip_drC( k )
          c(i,j)=-deltaTtracer*_rhFacC(i,j,k,bi,bj)*recip_drF(k)
     &         *KappaRT(i,j,k+1)*recip_drC(k+1)
          b(i,j)=1.-c(i,j)-a(i,j)
         ENDDO
        ENDDO
        DO j=jMin,jMax
         DO i=iMin,iMax
          gam(i,j,k)=ckm1(i,j)*bet(i,j)
          bet(i,j)=b(i,j)-a(i,j)*gam(i,j,k)
          IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
          gTnm1(i,j,k,bi,bj)=(gTnm1(i,j,k,bi,bj)
     &       -a(i,j)*gTnm1(i,j,k-1,bi,bj))*bet(i,j)
          gSnm1(i,j,k,bi,bj)=(gSnm1(i,j,k,bi,bj)
     &       -a(i,j)*gSnm1(i,j,k-1,bi,bj))*bet(i,j)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF (Nz.GT.1) THEN
C--    End of forward sweep (bottom level)
       DO j=jMin,jMax
        DO i=iMin,iMax
         ckm1(i,j)=c(i,j)
         a(i,j)=-deltaTtracer*_rhFacC(i,j,Nz,bi,bj)*recip_drF(Nz)
     &        *KappaRT(i,j, Nz )*recip_drC( Nz )
         b(i,j)=1.-a(i,j)
        ENDDO
       ENDDO
       DO j=jMin,jMax
        DO i=iMin,iMax
         gam(i,j,Nz)=ckm1(i,j)*bet(i,j)
         bet(i,j)=b(i,j)-a(i,j)*gam(i,j,Nz)
         IF (bet(i,j).NE.0.) bet(i,j)=1. / bet(i,j)
         gTnm1(i,j,Nz,bi,bj)=(gTnm1(i,j,Nz,bi,bj)
     &      -a(i,j)*gTnm1(i,j,Nz-1,bi,bj))*bet(i,j)
         gSnm1(i,j,Nz,bi,bj)=(gSnm1(i,j,Nz,bi,bj)
     &      -a(i,j)*gSnm1(i,j,Nz-1,bi,bj))*bet(i,j)
        ENDDO
       ENDDO
C--    Backward sweep
       DO k=Nz-1,1,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          gTnm1(i,j,k,bi,bj)=gTnm1(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*gTnm1(i,j,k+1,bi,bj)
          gSnm1(i,j,k,bi,bj)=gSnm1(i,j,k,bi,bj)
     &              -gam(i,j,k+1)*gSnm1(i,j,k+1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
