C $Header: /u/gcmpack/MITgcm/model/src/Attic/convect.F,v 1.2 1998/04/24 02:05:40 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CONVECT( bi, bj, iMin, iMax, jMin, jMax, myThid )
C     /==========================================================\
C     | SUBROUTINE CONVECT                                       |
C     | o Does vertical mizing of unstable water column          |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,k
      _RL tMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSum(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DO k=2,nZ
       DO j=jMin,jmax
        DO i=iMin,imax
         tMix(i,j) = theta(i,j,k-1,bi,bj)*hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &              +theta(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)*dzF(k)
         dSum(i,j) = hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &              +hFacC(i,j,k,bi,bj)*dzF(k)
        ENDDO
       ENDDO
       DO j=jMin,jmax
        DO i=iMin,imax
         IF ( hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &        theta(i,j,k-1,bi,bj) .LT. theta(i,j,k,bi,bj) ) THEN
          theta(i,j,k-1,bi,bj) = tMix(i,j)/dSum(i,j)
          theta(i,j,k  ,bi,bj) = tMix(i,j)/dSum(i,j)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
