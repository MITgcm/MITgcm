C $Header: /u/gcmpack/MITgcm/model/src/Attic/convect.F,v 1.5 1998/05/25 20:05:55 cnh Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE CONVECT( bi, bj, iMin, iMax, jMin, jMax, K,
     &       rhoKm1, rhoKp1,myTime,myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CONVECT                                       |
C     | o Does vertical mixing of unstable water column          |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K - Loop counters
C     rhoKm1 - rho in layer above
C     rhoKp1 - rho in layer below
C     myTime - Current time in simulation
C     myIter - Current iteration in simulation
C     myThid - Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
      _RL tMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sMix(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSum(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C--   Check to see if should convect now
      IF ( DIFFERENT_MULTIPLE(cAdjFreq,myTime,myTime-deltaTClock) ) THEN

C--    Calculate heat content of two level column
       DO j=jMin,jmax
        DO i=iMin,imax
         tMix(i,j) = theta(i,j,k-1,bi,bj)*hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &              +theta(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)*dzF(k)
         sMix(i,j) = salt(i,j,k-1,bi,bj)*hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &              +salt(i,j,k,bi,bj)*hFacC(i,j,k,bi,bj)*dzF(k)
         dSum(i,j) = hFacC(i,j,k-1,bi,bj)*dzF(k-1)
     &              +hFacC(i,j,k,bi,bj)*dzF(k)
        ENDDO
       ENDDO

C--    Where statically unstable, mix the heat and salt
       DO j=jMin,jmax
        DO i=iMin,imax
         IF ( hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &        rhokm1(i,j) .GT. rhokp1(i,j) ) THEN
          theta(i,j,k-1,bi,bj) = tMix(i,j)/dSum(i,j)
          theta(i,j,k  ,bi,bj) = tMix(i,j)/dSum(i,j)
c         salt(i,j,k-1,bi,bj) = sMix(i,j)/dSum(i,j)
c         salt(i,j,k  ,bi,bj) = sMix(i,j)/dSum(i,j)
         ENDIF
        ENDDO
       ENDDO
       
      ENDIF

      RETURN
      END
