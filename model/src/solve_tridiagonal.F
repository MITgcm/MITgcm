C $Header: /u/gcmpack/MITgcm/model/src/solve_tridiagonal.F,v 1.2 2004/12/05 17:29:36 jmc Exp $
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
      _RL a3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL b3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL c3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL y3d(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER errCode
      INTEGER bi, bj, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL bet(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL tmpvar
CEOP

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

      RETURN
      END
