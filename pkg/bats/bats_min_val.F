#include "BATS_OPTIONS.h"

CBOP
      subroutine BATS_MIN_VAL(
     I           PTR, min_val, adj, bi, bj )

C     =================================================================
C     | subroutine bats_min_val
C     | o replaces tracer values that are too small or negative
C     |   (in a separate routine so can be hidden from adjoint)
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"

C     === Routine arguments ===
      INTEGER bi, bj
C     === Input ===
      _RL     PTR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     min_val
      _RL     adj(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
CEOP

C     == Local variables ==
      INTEGER i,j,k

       DO k=1,Nr
         DO i=1,sNx
          DO j=1,sNy
           adj(i,j,k) = PTR(i,j,k)
           PTR(i,j,k) = max( min_val*maskC(i,j,k,bi,bj), PTR(i,j,k) )
           adj(i,j,k) = PTR(i,j,k)-adj(i,j,k)
          ENDDO
         ENDDO
       ENDDO

      RETURN
      END
