C $Header: /u/gcmpack/MITgcm/pkg/ggl90/Attic/ggl90_solve.F,v 1.1 2004/09/16 11:27:18 mlosch Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_SOLVE
C     !INTERFACE:
      SUBROUTINE GGL90_SOLVE( bi, bj, iMin, iMax, jMin, jMax,
     I                     a, b, c,
     U                     gXNm1,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R GGL90_SOLVE                                              
C     | o Solve implicit diffusion equation for vertical          
C     |   diffusivity for turbulent kinetic energy.
C     | o Tridiagonal matrix solver.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
CML#ifdef ALLOW_AUTODIFF_TAMC
CML#include "tamc_keys.h"
CML#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
      _RL a(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gXnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)

#ifdef ALLOW_GGL90
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL gYnm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gam(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
CEOP

cph(
cph Not good for TAF: may create irreducible control flow graph
cph      IF (Nr.LE.1) RETURN
cph)

C--   Initialise
      DO k=1,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         gYNm1(i,j,k) = 0. _d 0
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
        gYNm1(i,j,1) = gXNm1(i,j,1)*bet(i,j,1)
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         gYnm1(i,j,k) = bet(i,j,k)*
     &        (gXNm1(i,j,k) - a(i,j,k)*gYnm1(i,j,k-1))
        ENDDO
       ENDDO
      ENDDO


C--    Backward sweep
CADJ loop = sequential
       DO k=Nr-1,1,-1
        DO j=jMin,jMax
         DO i=iMin,iMax
          gYnm1(i,j,k)=gYnm1(i,j,k) - gam(i,j,k+1)*gYnm1(i,j,k+1)
         ENDDO
        ENDDO
       ENDDO

       DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
          gXNm1(i,j,k)=gYnm1(i,j,k)
         ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_GGL90 */
      RETURN
      END

