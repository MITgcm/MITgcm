C $Header: /u/gcmpack/MITgcm/model/src/Attic/convect.F,v 1.4 1998/04/29 21:28:55 adcroft Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CONVECT( bi, bj, iMin, iMax, jMin, jMax, K,
     &       rhoKm1, rhoKp1,  myThid )
C     /==========================================================\
C     | SUBROUTINE CONVECT                                       |
C     | o Does vertical mixing of unstable water column          |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL tMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSum(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C Calculate heat content of two level column
      DO j=jMin,jmax
       DO i=iMin,imax
        tMix(i,j) = theta(i,j,k-1,bi,bj)*hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &             +theta(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)*dzF(k)
        sMix(i,j) = salt(i,j,k-1,bi,bj)*hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &             +salt(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)*dzF(k)
        dSum(i,j) = hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &             +hFacC(i,j,k,bi,bj)*dzF(k)
       ENDDO
      ENDDO
C Where statically unstable, mix the heat and salt
      DO j=jMin,jmax
       DO i=iMin,imax
        IF ( hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &       rhokm1(i,j) .GT. rhokp1(i,j) ) THEN
         theta(i,j,k-1,bi,bj) = tMix(i,j)/dSum(i,j)
         theta(i,j,k  ,bi,bj) = tMix(i,j)/dSum(i,j)
c        salt(i,j,k-1,bi,bj) = sMix(i,j)/dSum(i,j)
c        salt(i,j,k  ,bi,bj) = sMix(i,j)/dSum(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
