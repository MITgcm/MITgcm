C $Header: /u/gcmpack/MITgcm/model/src/solve_pentadiagonal.F,v 1.1 2004/01/07 21:19:37 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
C     a5d :: 2nd  lower diagonal of the pentadiagonal matrix
C     b5d :: 1rst lower diagonal of the pentadiagonal matrix
C     c5d :: main diagonal       of the pentadiagonal matrix
C     d5d :: 1rst upper diagonal of the pentadiagonal matrix
C     e5d :: 2nd  upper diagonal of the pentadiagonal matrix
C     y5d :: Y vector (R.H.S.); 
C     OUTPUT:
C     y5d :: X = solution of A*X=Y
C     a5d,b5d,c5d,d5d,e5d :: modified to enable to find X' solution of
C                        A*X'=Y' without solving the full system again
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
CEOP

      errCode = 0

C--   forward sweep (starting from top)
      k = 1
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

      k = 2
      DO j=jMin,jMax
       DO i=iMin,iMax
C--      [k] <- [k] - b_k/c_k-1 * [k-1]
         b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
         c5d(i,j,k) = c5d(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
         d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
         y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                    - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
         IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
         ELSE
           c5d(i,j,k) = 0. _d 0
           errCode = 1
         ENDIF
       ENDDO
      ENDDO

C--   Middle of forward sweep
      DO k=3,Nr
       DO j=jMin,jMax
        DO i=iMin,iMax
C--      [k] <- [k] - a_k/c_k-2 * [k-2]
         a5d(i,j,k) = a5d(i,j,k)*c5d(i,j,k-2) 
         b5d(i,j,k) = b5d(i,j,k) - a5d(i,j,k)*d5d(i,j,k-2)
         c5d(i,j,k) = c5d(i,j,k) - a5d(i,j,k)*e5d(i,j,k-2)
C--      [k] <- [k] - b_k/c_k-1 * [k-1]
         b5d(i,j,k) = b5d(i,j,k)*c5d(i,j,k-1)
         c5d(i,j,k) = c5d(i,j,k) - b5d(i,j,k)*d5d(i,j,k-1)
         d5d(i,j,k) = d5d(i,j,k) - b5d(i,j,k)*e5d(i,j,k-1)
         y5d(i,j,k,bi,bj) = y5d(i,j,k,bi,bj)
     &                    - b5d(i,j,k)*y5d(i,j,k-1,bi,bj)
     &                    - a5d(i,j,k)*y5d(i,j,k-2,bi,bj)
         IF ( c5d(i,j,k).NE.0. _d 0 ) THEN 
           c5d(i,j,k) = 1. _d 0 / c5d(i,j,k)
         ELSE
           c5d(i,j,k) = 0. _d 0
           errCode = 1
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C--   Backward sweep (starting from bottom)
      k = Nr
      DO j=jMin,jMax
       DO i=iMin,iMax
         y5d(i,j,k,bi,bj) =   y5d(i,j,k,bi,bj)*c5d(i,j,k)
       ENDDO
      ENDDO
      k = Nr-1
      DO j=jMin,jMax
       DO i=iMin,iMax
         y5d(i,j,k,bi,bj) = ( y5d(i,j,k,bi,bj)
     &                      - d5d(i,j,k)*y5d(i,j,k+1,bi,bj)
     &                      )*c5d(i,j,k)
       ENDDO
      ENDDO
      DO k=Nr-2,1,-1
       DO j=jMin,jMax
        DO i=iMin,iMax
         y5d(i,j,k,bi,bj) = ( y5d(i,j,k,bi,bj)
     &                      - d5d(i,j,k)*y5d(i,j,k+1,bi,bj)
     &                      - e5d(i,j,k)*y5d(i,j,k+2,bi,bj)
     &                      )*c5d(i,j,k)
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
