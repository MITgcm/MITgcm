C $Header: /u/gcmpack/MITgcm/model/src/solve_tridiagonal.F,v 1.11 2014/08/14 16:49:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
      _RL a3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL y3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER errCode
      INTEGER bi, bj, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL tmpVar
#ifndef SOLVE_DIAGONAL_LOWMEMORY
      _RL recVar
      _RL c3d_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL y3d_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
# ifdef SOLVE_DIAGONAL_KINNER
      _RL c3d_prime(Nr)
      _RL y3d_prime(Nr)
      _RL y3d_update(Nr)
# else
      _RL c3d_prime(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL y3d_prime(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL y3d_update(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
# endif
#endif /* SOLVE_DIAGONAL_LOWMEMORY */
CEOP

      errCode = 0

#ifdef SOLVE_DIAGONAL_LOWMEMORY

C--   Beginning of forward sweep (top level)
      DO j=jMin,jMax
       DO i=iMin,iMax
         IF ( b3d(i,j,1).NE.0. _d 0 ) THEN
           b3d(i,j,1) = 1. _d 0 / b3d(i,j,1)
         ELSE
           b3d(i,j,1) = 0. _d 0
           errCode = 1
         ENDIF
       ENDDO
      ENDDO

C--   Middle of forward sweep
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         tmpVar = b3d(i,j,k) - a3d(i,j,k)*c3d(i,j,k-1)*b3d(i,j,k-1)
         IF ( tmpVar .NE. 0. _d 0 ) THEN
           b3d(i,j,k) = 1. _d 0 / tmpVar
         ELSE
           b3d(i,j,k) = 0. _d 0
           errCode = 1
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
         y3d(i,j,1) = y3d(i,j,1)*b3d(i,j,1)
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
         y3d(i,j,k) = ( y3d(i,j,k)
     &                - a3d(i,j,k)*y3d(i,j,k-1)
     &                )*b3d(i,j,k)
        ENDDO
       ENDDO
      ENDDO

C--    Backward sweep
      DO k=Nr-1,1,-1
       DO j=jMin,jMax
        DO i=iMin,iMax
          y3d(i,j,k) = y3d(i,j,k)
     &               - c3d(i,j,k)*b3d(i,j,k)*y3d(i,j,k+1)
        ENDDO
       ENDDO
      ENDDO

#else /* ndef SOLVE_DIAGONAL_LOWMEMORY */

C--   Temporary array
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
c      DO j=jMin,jMax
c       DO i=iMin,iMax
         c3d_m1(i,j,k) = c3d(i,j,k)
         y3d_m1(i,j,k) = y3d(i,j,k)
        ENDDO
       ENDDO
      ENDDO

#ifdef SOLVE_DIAGONAL_KINNER

C--   Main loop
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx

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
           tmpVar = b3d(i,j,k) - a3d(i,j,k)*c3d_prime(k-1)
           IF ( tmpVar .NE. 0. _d 0 ) THEN
             recVar = 1. _d 0 / tmpVar
             c3d_prime(k) = c3d_m1(i,j,k)*recVar
             y3d_prime(k) = (y3d_m1(i,j,k) - y3d_prime(k-1)*a3d(i,j,k))
     &                      *recVar
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
         y3d(i,j,k) = y3d_update(k)
        ENDDO

       ENDDO
      ENDDO

#else  /* ndef SOLVE_DIAGONAL_KINNER */

C--   Init.
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
c      DO j=jMin,jMax
c       DO i=iMin,iMax
         c3d_prime(i,j,k) = 0. _d 0
         y3d_prime(i,j,k) = 0. _d 0
         y3d_update(i,j,k) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

CADJ loop = sequential
C--   Forward sweep
      DO k=1,Nr

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
c      DO j=jMin,jMax
c       DO i=iMin,iMax
         IF ( k.EQ.1 ) THEN
           IF ( b3d(i,j,1).NE.0. _d 0 ) THEN
             recVar = 1. _d 0 / b3d(i,j,1)
             c3d_prime(i,j,1) = c3d_m1(i,j,1)*recVar
             y3d_prime(i,j,1) = y3d_m1(i,j,1)*recVar
           ELSE
             c3d_prime(i,j,1) = 0. _d 0
             y3d_prime(i,j,1) = 0. _d 0
             errCode = 1
           ENDIF
         ELSE
           tmpVar = b3d(i,j,k) - a3d(i,j,k)*c3d_prime(i,j,k-1)
           IF ( tmpVar .NE. 0. _d 0 ) THEN
             recVar = 1. _d 0 / tmpVar
             c3d_prime(i,j,k) = c3d_m1(i,j,k)*recVar
             y3d_prime(i,j,k) = ( y3d_m1(i,j,k)
     &                          - a3d(i,j,k)*y3d_prime(i,j,k-1)
     &                          )*recVar
           ELSE
             c3d_prime(i,j,k) = 0. _d 0
             y3d_prime(i,j,k) = 0. _d 0
             errCode = 1
           ENDIF
         ENDIF
        ENDDO
       ENDDO
C-- k-loop
      ENDDO

CADJ loop = sequential
C--   Backward sweep
      DO k=Nr,1,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
c      DO j=jMin,jMax
c       DO i=iMin,iMax
         IF ( k.EQ.Nr ) THEN
          y3d_update(i,j,k) = y3d_prime(i,j,k)
         ELSE
          y3d_update(i,j,k) = y3d_prime(i,j,k)
     &                      - c3d_prime(i,j,k)*y3d_update(i,j,k+1)
         ENDIF
        ENDDO
       ENDDO
C-- k-loop
      ENDDO

C--   Update array
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
c      DO j=jMin,jMax
c       DO i=iMin,iMax
         y3d(i,j,k) = y3d_update(i,j,k)
        ENDDO
       ENDDO
C-- k-loop
      ENDDO

#endif /* SOLVE_DIAGONAL_KINNER */

#endif /* SOLVE_DIAGONAL_LOWMEMORY */

      RETURN
      END
