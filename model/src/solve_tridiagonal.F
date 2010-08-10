C $Header: /u/gcmpack/MITgcm/model/src/solve_tridiagonal.F,v 1.3 2010/08/10 17:58:30 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C o Switch to code that has the k-loop inside the 
C   ij-loops, which matters in adjoint mode.
#ifdef ALLOW_AUTODIFF 
#define ALLOW_SOLVERS_KLOOPINSIDE
#endif

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
#else
      _RL c3d_prime(Nr), y3d_prime(Nr),y3d_update(Nr)
      _RL c3d_m1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL y3d_m1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#endif
CEOP

#ifndef ALLOW_SOLVERS_KLOOPINSIDE      

      errCode = 0

C--   Beginning of forward sweep (top level)
      DO j=jMin,jMax
       DO i=iMin,iMax
         IF ( b3d(i,j,1).NE.0. _d 0 ) THEN 
           bet(i,j,1) = 1. _d 0 / b3d(i,j,1)
         ELSE
           bet(i,j,1) = 0. _d 0
           errCode = 1
         ENDIF
       ENDDO
      ENDDO

C--   Middle of forward sweep
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         tmpvar = b3d(i,j,k) - a3d(i,j,k)*c3d(i,j,k-1)*bet(i,j,k-1)
         IF ( tmpvar .NE. 0. _d 0 ) THEN
           bet(i,j,k) = 1. _d 0 / tmpvar
         ELSE
           bet(i,j,k) = 0. _d 0
           errCode = 1
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
         y3d(i,j,1,bi,bj) = y3d(i,j,1,bi,bj)*bet(i,j,1)
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         y3d(i,j,k,bi,bj) = ( y3d(i,j,k,bi,bj) 
     &                      - a3d(i,j,k)*y3d(i,j,k-1,bi,bj)
     &                      )*bet(i,j,k) 
        ENDDO
       ENDDO
      ENDDO

C--    Backward sweep
CADJ loop = sequential
      DO k=Nr-1,1,-1
       DO j=jMin,jMax
        DO i=iMin,iMax
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
