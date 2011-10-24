C $Header: /u/gcmpack/MITgcm/model/src/solve_pentadiagonal.F,v 1.6 2011/10/24 16:11:19 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CMLC o Switch to code that has the k-loop inside the 
CMLC   ij-loops, which matters in adjoint mode.
CML#ifdef ALLOW_AUTODIFF 
CML#define ALLOW_SOLVERS_KLOOPINSIDE
CML#endif

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
#else
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
C     These fields are required for efficient adjoint vector code.
C     The extra storage that is involved is avoided in the case
C     of forward simulations at the cost of even uglier code.
      _RL y5dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL a5dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b5dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c5dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL d5dloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
# endif
      _RL c5dtmp
#endif
CEOP

#ifndef ALLOW_SOLVERS_KLOOPINSIDE

      errCode = 0

# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
      DO k=1,Nr
C--   forward sweep (starting from top)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         a5dloc(i,j,k)       = a5d(i,j,k)
         a5d   (i,j,k)       = 0. _d 0
         b5dloc(i,j,k)       = b5d(i,j,k)
         b5d   (i,j,k)       = 0 . _d 0
         c5dloc(i,j,k)       = c5d(i,j,k)
         c5d   (i,j,k)       = 0. _d 0
         d5dloc(i,j,k)       = d5d(i,j,k)
         d5d   (i,j,k)       = 0. _d 0
         y5dloc(i,j,k)       = y5d(i,j,k,bi,bj)
         y5d   (i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_AUTODIFF_TAMC and TARGET_NEC_SX */

C--   forward sweep (starting from top)
CADJ loop = sequential
      DO k=1,Nr
       IF (k.EQ.1) THEN
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
          IF ( c5dloc(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5dloc(i,j,k)
          ELSE
           c5d(i,j,k) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
          ENDIF
          d5d(i,j,k)       = d5dloc(i,j,k)
          y5d(i,j,k,bi,bj) = y5dloc(i,j,k)
# else
          IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
          ELSE
           c5d(i,j,k) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
          ENDIF
# endif
         ENDDO
        ENDDO

       ELSEIF (k.EQ.2) THEN
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
C--       [k] <- [k] - b_k/c_k-1 * [k-1]
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
          b5d(i,j,k) = b5dloc(i,j,k)*c5d(i,j,k-1)
          c5dtmp     = c5dloc(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5dloc(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5dloc(i,j,k)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
# else
          b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
          c5dtmp     = c5d(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
# endif
          IF ( c5dtmp.NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5dtmp
          ELSE
           c5d(i,j,k) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
          ENDIF
         ENDDO
        ENDDO

       ELSE
C--   Middle of forward sweep
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
# if (defined ALLOW_AUTODIFF_TAMC && defined TARGET_NEC_SX)
C--       [k] <- [k] - a_k/c_k-2 * [k-2]
          a5d(i,j,k) = a5dloc(i,j,k)*c5d(i,j,k-2) 
          b5d(i,j,k) = b5dloc(i,j,k) - a5d(i,j,k)*d5d(i,j,k-2)
          c5dtmp     = c5dloc(i,j,k) - a5d(i,j,k)*e5d(i,j,k-2)
C--       [k] <- [k] - b_k/c_k-1 * [k-1]
          b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
          c5dtmp     = c5dtmp        - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5dloc(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5dloc(i,j,k)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
     &                     - a5d(i,j,k)*y5d(i,j,k-2,bi,bj)
# else
C--       [k] <- [k] - a_k/c_k-2 * [k-2]
          a5d(i,j,k) = a5d(i,j,k)*c5d(i,j,k-2) 
          b5d(i,j,k) = b5d(i,j,k) - a5d(i,j,k)*d5d(i,j,k-2)
          c5dtmp     = c5d(i,j,k) - a5d(i,j,k)*e5d(i,j,k-2)
C--       [k] <- [k] - b_k/c_k-1 * [k-1]
          b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
          c5dtmp     = c5dtmp     - b5d(i,j,k)*d5d(i,j,k-1)
          d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
          y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                     - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
     &                     - a5d(i,j,k)*y5d(i,j,k-2,bi,bj)
# endif
          IF ( c5dtmp.NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5dtmp
          ELSE
           c5d(i,j,k) = 0. _d 0
# ifndef TARGET_NEC_SX
C     keep your fingers crossed and enjoy the better performance
           errCode = 1
# endif
          ENDIF
         ENDDO
        ENDDO
C-      end if k= .. ; end of k loop
       ENDIF
      ENDDO

C--   Backward sweep (starting from bottom)
CADJ loop = sequential
      DO k=Nr,1,-1
       IF (k.EQ.Nr) THEN
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
          y5d(i,j,k,bi,bj) =   y5d(i,j,k,bi,bj)*c5d(i,j,k)
         ENDDO
        ENDDO
       ELSEIF (k.EQ.Nr-1) THEN
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
          y5d(i,j,k,bi,bj) = ( y5d(i,j,k,bi,bj)
     &                       - d5d(i,j,k)*y5d(i,j,k+1,bi,bj)
     &                       )*c5d(i,j,k)
         ENDDO
        ENDDO
       ELSE
# ifdef TARGET_NEC_SX
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
# else
        DO j=jMin,jMax
         DO i=iMin,iMax
# endif
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
