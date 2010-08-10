C $Header: /u/gcmpack/MITgcm/model/src/solve_pentadiagonal.F,v 1.4 2010/08/10 17:58:30 gforget Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C o Switch to code that has the k-loop inside the 
C   ij-loops, which matters in adjoint mode.
#ifdef ALLOW_AUTODIFF 
#define ALLOW_SOLVERS_KLOOPINSIDE
#endif

CBOP
C     !ROUTINE: SOLVE_PENTADIAGONAL
C     !INTERFACE:
      SUBROUTINE SOLVE_PENTADIAGONAL( 
     I                     iMin,iMax, jMin,jMax,
     U                     a5d, b5d, c5d, d5d, e5d,
     U                     y5d,
     O                     errCode,
     I                     bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SOLVE_PENTADIAGONAL                                              
C     | o Solve a penta-diagonal system A*X=Y (dimension Nr)
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
C     INPUT:
C     iMin,iMax,jMin,jMax  :: computational domain
C     a5d    :: 2nd  lower diagonal of the pentadiagonal matrix
C     b5d    :: 1rst lower diagonal of the pentadiagonal matrix
C     c5d    :: main diagonal       of the pentadiagonal matrix
C     d5d    :: 1rst upper diagonal of the pentadiagonal matrix
C     e5d    :: 2nd  upper diagonal of the pentadiagonal matrix
C     y5d    :: Y vector (R.H.S.); 
C     bi,bj  :: tile indices
C     myThid :: thread number
C     OUTPUT:
C     y5d    :: X = solution of A*X=Y
C     a5d,b5d,c5d,d5d,e5d :: modified to enable to find Xp solution of
C                        A*Xp=Yp without solving the full system again
C     errCode :: > 0 if singular matrix
      INTEGER iMin,iMax,jMin,jMax
      _RL a5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL d5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL e5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL y5d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER errCode
      INTEGER bi, bj, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
#ifdef ALLOW_SOLVERS_KLOOPINSIDE
      _RL y5d_m1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL a5d_prime(Nr), b5d_prime(Nr)
      _RL c5d_prime(Nr), d5d_prime(Nr), e5d_prime(Nr)
      _RL y5d_prime(Nr), y5d_update(Nr), tmpval
#endif
CEOP

#ifndef ALLOW_SOLVERS_KLOOPINSIDE

      errCode = 0

      DO k=1,Nr
C--   forward sweep (starting from top)
       IF (k.EQ.1) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
          ELSE
           c5d(i,j,k) = 0. _d 0
           errCode = 1
          ENDIF
         ENDDO
        ENDDO

       ELSEIF (k.EQ.2) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
C--       [k] <- [k] - b_k/c_k-1 * [k-1]
          b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
          c5d(i,j,k) = c5d(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
          IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
          ELSE
           c5d(i,j,k) = 0. _d 0
           errCode = 1
          ENDIF
         ENDDO
        ENDDO

       ELSE
C--   Middle of forward sweep
        DO j=jMin,jMax
         DO i=iMin,iMax
C--       [k] <- [k] - a_k/c_k-2 * [k-2]
          a5d(i,j,k) = a5d(i,j,k)*c5d(i,j,k-2) 
          b5d(i,j,k) = b5d(i,j,k) - a5d(i,j,k)*d5d(i,j,k-2)
          c5d(i,j,k) = c5d(i,j,k) - a5d(i,j,k)*e5d(i,j,k-2)
C--       [k] <- [k] - b_k/c_k-1 * [k-1]
          b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
          c5d(i,j,k) = c5d(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
     &                     - a5d(i,j,k)*y5d(i,j,k-2,bi,bj)
          IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
          ELSE
           c5d(i,j,k) = 0. _d 0
           errCode = 1
          ENDIF
         ENDDO
        ENDDO
C-      end if k= .. ; end of k loop
       ENDIF
      ENDDO

C--   Backward sweep (starting from bottom)
      DO k=Nr,1,-1
       IF (k.EQ.Nr) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          y5d(i,j,k,bi,bj) =   y5d(i,j,k,bi,bj)*c5d(i,j,k)
         ENDDO
        ENDDO
       ELSEIF (k.EQ.Nr-1) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          y5d(i,j,k,bi,bj) = ( y5d(i,j,k,bi,bj)
     &                       - d5d(i,j,k)*y5d(i,j,k+1,bi,bj)
     &                       )*c5d(i,j,k)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          y5d(i,j,k,bi,bj) = ( y5d(i,j,k,bi,bj)
     &                       - d5d(i,j,k)*y5d(i,j,k+1,bi,bj)
     &                       - e5d(i,j,k)*y5d(i,j,k+2,bi,bj)
     &                       )*c5d(i,j,k)
         ENDDO
        ENDDO
C-      end if k= .. ; end of k loop
       ENDIF
      ENDDO

#else  /* ALLOW_SOLVERS_KLOOPINSIDE */

      errCode = 0

C--   Temporary array
      DO j=jMin,jMax
      DO i=iMin,iMax
      DO k=1,Nr
         y5d_m1(i,j,k) = y5d(i,j,k,bi,bj)
      ENDDO
      ENDDO
      ENDDO

C--   Main loop
      DO j=jMin,jMax
      DO i=iMin,iMax

      DO k=1,Nr
        a5d_prime(k) = 0. _d 0
        b5d_prime(k) = 0. _d 0
        c5d_prime(k) = 0. _d 0
        d5d_prime(k) = 0. _d 0
        e5d_prime(k) = 0. _d 0
        y5d_prime(k) = 0. _d 0
        y5d_update(k) = 0. _d 0
      ENDDO

      DO k=1,Nr
C--   forward sweep (starting from top)

          IF (k.EQ.1) THEN
c just copy terms
           a5d_prime(k) = 0. _d 0
           b5d_prime(k) = 0. _d 0
           c5d_prime(k) = c5d(i,j,k)
           d5d_prime(k) = d5d(i,j,k)
           e5d_prime(k) = e5d(i,j,k)
           y5d_prime(k) = y5d_m1(i,j,k)
          ELSEIF (k.EQ.2) THEN
c subtract one term
           a5d_prime(k) = 0. _d 0
           b5d_prime(k) = 0. _d 0
           c5d_prime(k) = c5d(i,j,k)
     &      -b5d(i,j,k)*d5d_prime(k-1)
           d5d_prime(k) = d5d(i,j,k)
     &      -b5d(i,j,k)*e5d_prime(k-1)
           e5d_prime(k) = e5d(i,j,k)
           y5d_prime(k) = y5d_m1(i,j,k)
     &      -b5d(i,j,k)*y5d_prime(k-1)
          ELSE
c subtract two terms
           a5d_prime(k) = 0. _d 0
           b5d_prime(k) = 0. _d 0
           c5d_prime(k) = c5d(i,j,k)
     &      -a5d(i,j,k)*e5d_prime(k-2)
     &      -(b5d(i,j,k)-a5d(i,j,k)*d5d_prime(k-2))*d5d_prime(k-1)
           d5d_prime(k) = d5d(i,j,k)
     &      -(b5d(i,j,k)-a5d(i,j,k)*d5d_prime(k-2))*e5d_prime(k-1)
           e5d_prime(k) = e5d(i,j,k)
           y5d_prime(k) = y5d_m1(i,j,k)
     &      -a5d(i,j,k)*y5d_prime(k-2)
     &      -(b5d(i,j,k)-a5d(i,j,k)*d5d_prime(k-2))*y5d_prime(k-1)
          ENDIF

c normalization
          tmpval=c5d_prime(k)
          IF ( tmpval.NE.0. _d 0 ) THEN
           a5d_prime(k) = a5d_prime(k) / tmpval
           b5d_prime(k) = b5d_prime(k) / tmpval
           c5d_prime(k) = 1. _d 0
           d5d_prime(k) = d5d_prime(k) / tmpval
           e5d_prime(k) = e5d_prime(k) / tmpval
           y5d_prime(k) = y5d_prime(k) / tmpval
          ELSE
           a5d_prime(k) = 0. _d 0
           b5d_prime(k) = 0. _d 0
           c5d_prime(k) = 0. _d 0
           d5d_prime(k) = 0. _d 0
           e5d_prime(k) = 0. _d 0
           y5d_prime(k) = 0. _d 0
           errCode = 1
          ENDIF

      ENDDO

C--   Backward sweep (starting from bottom)
      DO k=Nr,1,-1
       IF (k.EQ.Nr) THEN
          y5d_update(k) =   y5d_prime(k)
       ELSEIF (k.EQ.Nr-1) THEN
          y5d_update(k) =   y5d_prime(k)
     &     - y5d_update(k+1)*d5d_prime(k)
       ELSE
          y5d_update(k) =   y5d_prime(k)
     &     - y5d_update(k+1)*d5d_prime(k)
     &     - y5d_update(k+2)*e5d_prime(k)
       ENDIF
      ENDDO

C--   Update array
      DO k=1,Nr
         y5d(i,j,k,bi,bj)=y5d_update(k)
      ENDDO

      ENDDO
      ENDDO

#endif  /* ALLOW_SOLVERS_KLOOPINSIDE */

      RETURN
      END
