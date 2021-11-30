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
#ifdef SOLVE_DIAGONAL_LOWMEMORY
C     | o Allows 2 types of call:
C     | 1) with INPUT errCode < 0 (first call):
C     |   Solve system A*X=Y by LU decomposition and return
C     |     inverse matrix as output (in a3d,b3d,c3d)
C     | 2) with INPUT errCode = 0 (subsequent calls):
C     |   Solve system A*Xp=Yp by using inverse matrix coeff
C     |     from first call output.
#endif /* SOLVE_DIAGONAL_LOWMEMORY */
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     -- INPUT: --
C     iMin,iMax,jMin,jMax  :: computational domain
C     a3d     :: matrix lower diagnonal
C     b3d     :: matrix main  diagnonal
C     c3d     :: matrix upper diagnonal
C     y3d     :: Y vector (R.H.S.);
C     bi,bj   :: tile indices
C     myThid  :: thread number
C     -- OUTPUT: --
C     y3d     :: X = solution of A*X=Y
C     errCode :: > 0 if singular matrix
#ifdef SOLVE_DIAGONAL_LOWMEMORY
C     a3d,b3d,c3d :: inverse matrix coeff to enable to find Xp solution
C                    of A*Xp=Yp with subsequent call to this routine
#endif /* SOLVE_DIAGONAL_LOWMEMORY */
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
      _RL y3d_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
# ifdef SOLVE_DIAGONAL_KINNER
      _RL c3d_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d_prime(Nr)
      _RL y3d_prime(Nr)
      _RL y3d_update(Nr)
# else
      _RL c3d_prime(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL y3d_prime(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
# endif
#endif /* SOLVE_DIAGONAL_LOWMEMORY */
CEOP

#ifdef SOLVE_DIAGONAL_LOWMEMORY

      IF ( errCode .LT. 0 ) THEN
       errCode = 0

C--   forward sweep (starting from top) part-1: matrix L.U decomposition
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

C-    Middle of forward sweep
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

C--   end if errCode < 0
      ENDIF

C--   forward sweep (starting from top) part-2: process RHS
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

C--   backward sweep (starting from bottom)
      DO k=Nr-1,1,-1
       DO j=jMin,jMax
        DO i=iMin,iMax
          y3d(i,j,k) = y3d(i,j,k)
     &               - c3d(i,j,k)*b3d(i,j,k)*y3d(i,j,k+1)
        ENDDO
       ENDDO
      ENDDO

#else /* ndef SOLVE_DIAGONAL_LOWMEMORY */

      errCode = 0

#ifdef SOLVE_DIAGONAL_KINNER
C--   Temporary array
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         c3d_m1(i,j,k) = c3d(i,j,k)
         y3d_m1(i,j,k) = y3d(i,j,k)
        ENDDO
       ENDDO
      ENDDO

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

C--   Init. + copy to temp. array
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         c3d_prime(i,j,k) = 0. _d 0
         y3d_prime(i,j,k) = 0. _d 0
         y3d_m1(i,j,k) = y3d(i,j,k)
        ENDDO
       ENDDO
      ENDDO

CADJ loop = sequential
C--   Forward sweep
      DO k=1,Nr

       IF ( k .EQ. 1 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( b3d(i,j,1).NE.0. _d 0 ) THEN
           recVar = 1. _d 0 / b3d(i,j,1)
           c3d_prime(i,j,1) = c3d(i,j,1)*recVar
           y3d_prime(i,j,1) = y3d_m1(i,j,1)*recVar
          ELSE
           c3d_prime(i,j,1) = 0. _d 0
           y3d_prime(i,j,1) = 0. _d 0
           errCode = 1
          ENDIF
         ENDDO
        ENDDO
       ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          tmpVar = b3d(i,j,k) - a3d(i,j,k)*c3d_prime(i,j,k-1)
          IF ( tmpVar .NE. 0. _d 0 ) THEN
           recVar = 1. _d 0 / tmpVar
           c3d_prime(i,j,k) = c3d(i,j,k)*recVar
           y3d_prime(i,j,k) = ( y3d_m1(i,j,k)
     &                        - a3d(i,j,k)*y3d_prime(i,j,k-1)
     &                        )*recVar
          ELSE
           c3d_prime(i,j,k) = 0. _d 0
           y3d_prime(i,j,k) = 0. _d 0
           errCode = 1
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C-    end k-loop
      ENDDO

CADJ loop = sequential
C--   Backward sweep
      DO k=Nr,1,-1
       IF ( k.EQ.Nr ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          y3d(i,j,k) = y3d_prime(i,j,k)
         ENDDO
        ENDDO
       ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          y3d(i,j,k) = y3d_prime(i,j,k)
     &               - c3d_prime(i,j,k)*y3d(i,j,k+1)
         ENDDO
        ENDDO
       ENDIF
C-    end k-loop
      ENDDO

#endif /* SOLVE_DIAGONAL_KINNER */

#endif /* SOLVE_DIAGONAL_LOWMEMORY */

      RETURN
      END
