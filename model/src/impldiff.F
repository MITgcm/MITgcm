C $Header: /u/gcmpack/MITgcm/model/src/impldiff.F,v 1.17 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: IMPLDIFF
C     !INTERFACE:
      SUBROUTINE IMPLDIFF( bi, bj, iMin, iMax, jMin, jMax,
     I                     deltaTX,KappaRX,recip_hFac,
     U                     gXNm1,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R IMPLDIFF                                              
C     | o Solve implicit diffusion equation for vertical          
C     |   diffusivity.                                            
C     *==========================================================*
C     | o Recoded from 2d intermediate fields to 3d to reduce     
C     |   TAMC storage                                            
C     | o Fixed missing masks for fields a(), c()                 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL deltaTX
      _RL KappaRX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RS recip_hFac(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL gXnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL gYnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
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
     &               *KappaRX(i,j, k )*recip_drC( k )
        ENDDO
       ENDDO
      ENDDO

C--   Old aUpper
      DO k=1,Nr-1
       DO j=jMin,jMax
        DO i=iMin,iMax
          c(i,j,k) = -deltaTX*recip_hFac(i,j,k,bi,bj)*recip_drF(k)
     &               *KappaRX(i,j,k+1)*recip_drC(k+1)
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
          bet(i,j,k) = 0. _d 0
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
      IF (Nr.GT.2) THEN

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
