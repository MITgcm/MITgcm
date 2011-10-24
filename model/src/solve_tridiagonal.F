C $Header: /u/gcmpack/MITgcm/model/src/solve_tridiagonal.F,v 1.5 2011/10/24 16:11:26 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CMLC o Switch to code that has the k-loop inside the 
CMLC   ij-loops, which matters in adjoint mode.
CML#ifdef ALLOW_AUTODIFF 
CML#define ALLOW_SOLVERS_KLOOPINSIDE
CML#endif

CBOP
C     !ROUTINE: SOLVE_TRIDIAGONAL
C     !INTERFACE:
      SUBROUTINE SOLVE_TRIDIAGONAL( 
     I                     iMin,iMax, jMin,jMax,
     I                     a3d, b3d, c3d,
     U                     y3d,
     O                     errCode,
     I                     bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SOLVE_TRIDIAGONAL                                              
C     | o Solve a tri-diagonal system A*X=Y (dimension Nr)
C     *==========================================================*
C     | o Used to solve implicitly vertical advection & diffusion
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     a3d :: matrix lower diagnonal
C     b3d :: matrix main  diagnonal
C     c3d :: matrix upper diagnonal
C     y3d :: Input = Y vector ; Output = X = solution of A*X=Y
C     errCode :: > 0 if singular matrix
      INTEGER iMin,iMax,jMin,jMax
      _RL a3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL y3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER errCode
      INTEGER bi, bj, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL tmpvar
#ifndef ALLOW_SOLVERS_KLOOPINSIDE
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
C     This field is required for efficient adjoint vector code
C     The extra storage that is involved is avoided in the case
C     of forward simulations at the cost of even uglier code.
      _RL y3dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
# endif 
#else
      _RL c3d_prime(Nr), y3d_prime(Nr),y3d_update(Nr)
      _RL c3d_m1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL y3d_m1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#endif
CEOP

#ifndef ALLOW_SOLVERS_KLOOPINSIDE      

      errCode = 0

# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         y3dloc(i,j,k)       = y3d(i,j,k,bi,bj)
         y3d   (i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF_TAMC and TARGET_NEC_SX */
C--   Beginning of forward sweep (top level)
# ifdef TARGET_NEC_SX
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
# else
      DO j=jMin,jMax
       DO i=iMin,iMax
# endif 
         IF ( b3d(i,j,1).NE.0. _d 0 ) THEN 
           bet(i,j,1) = 1. _d 0 / b3d(i,j,1)
         ELSE
           bet(i,j,1) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
         ENDIF
       ENDDO
      ENDDO

C--   Middle of forward sweep
CADJ loop = sequential
      DO k=2,Nr
# ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
# else
       DO j=jMin,jMax
        DO i=iMin,iMax
# endif
         tmpvar = b3d(i,j,k) - a3d(i,j,k)*c3d(i,j,k-1)*bet(i,j,k-1)
         IF ( tmpvar .NE. 0. _d 0 ) THEN
           bet(i,j,k) = 1. _d 0 / tmpvar
         ELSE
           bet(i,j,k) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
         ENDIF
        ENDDO
       ENDDO
      ENDDO

# ifdef TARGET_NEC_SX
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
# else
      DO j=jMin,jMax
       DO i=iMin,iMax
# endif
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
         y3d(i,j,1,bi,bj) = y3dloc(i,j,1)*bet(i,j,1)
# else
         y3d(i,j,1,bi,bj) = y3d(i,j,1,bi,bj)*bet(i,j,1)
# endif 
       ENDDO
      ENDDO
      DO k=2,Nr
# ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
# else
       DO j=jMin,jMax
        DO i=iMin,iMax
# endif
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
         y3d(i,j,k,bi,bj) = ( y3dloc(i,j,k) 
# else
         y3d(i,j,k,bi,bj) = ( y3d(i,j,k,bi,bj) 
# endif
     &                      - a3d(i,j,k)*y3d(i,j,k-1,bi,bj)
     &                      )*bet(i,j,k) 
        ENDDO
       ENDDO
      ENDDO

C--    Backward sweep
CADJ loop = sequential
      DO k=Nr-1,1,-1
# ifdef TARGET_NEC_SX
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
# else
       DO j=jMin,jMax
        DO i=iMin,iMax
# endif
          y3d(i,j,k,bi,bj) = y3d(i,j,k,bi,bj)
     &         - c3d(i,j,k)*bet(i,j,k)*y3d(i,j,k+1,bi,bj)
        ENDDO
       ENDDO
      ENDDO

#else  /* ALLOW_SOLVERS_KLOOPINSIDE */

      errCode = 0

C--   Temporary array
      DO j=jMin,jMax
      DO i=iMin,iMax
      DO k=1,Nr
         c3d_m1(i,j,k) = c3d(i,j,k)
         y3d_m1(i,j,k) = y3d(i,j,k,bi,bj)
      ENDDO
      ENDDO
      ENDDO

C--   Main loop
      DO j=jMin,jMax
      DO i=iMin,iMax

      DO k=1,Nr
        c3d_prime(k) = 0. _d 0
        y3d_prime(k) = 0. _d 0
        y3d_update(k) = 0. _d 0
      ENDDO

C--   Forward sweep
      DO k=1,Nr
         IF ( k.EQ.1 ) THEN
           IF ( b3d(i,j,1).NE.0. _d 0 ) THEN
             c3d_prime(1) = c3d_m1(i,j,1) / b3d(i,j,1)
             y3d_prime(1) = y3d_m1(i,j,1) / b3d(i,j,1)
           ELSE
             c3d_prime(1) = 0. _d 0
             y3d_prime(1) = 0. _d 0
             errCode = 1
           ENDIF
         ELSE
           tmpvar = b3d(i,j,k) - a3d(i,j,k)*c3d_prime(k-1)
           IF ( tmpvar .NE. 0. _d 0 ) THEN
             c3d_prime(k) = c3d_m1(i,j,k) / tmpvar
             y3d_prime(k) = (y3d_m1(i,j,k) - y3d_prime(k-1)*a3d(i,j,k))
     &                      / tmpvar
           ELSE
             c3d_prime(k) = 0. _d 0
             y3d_prime(k) = 0. _d 0
             errCode = 1
           ENDIF
         ENDIF
      ENDDO

C--   Backward sweep
      DO k=Nr,1,-1
         IF ( k.EQ.Nr ) THEN
          y3d_update(k)=y3d_prime(k)
         ELSE
          y3d_update(k)=y3d_prime(k)-c3d_prime(k)*y3d_update(k+1)
         ENDIF
      ENDDO

C--   Update array
      DO k=1,Nr
         y3d(i,j,k,bi,bj)=y3d_update(k)
      ENDDO

      ENDDO
      ENDDO

#endif  /* ALLOW_SOLVERS_KLOOPINSIDE */

      RETURN
      END
