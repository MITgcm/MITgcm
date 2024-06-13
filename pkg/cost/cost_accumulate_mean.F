#include "COST_OPTIONS.h"

      SUBROUTINE cost_accumulate_mean( myThid )
C     *==========================================================*
C     | SUBROUTINE cost_accumulate_mean                          |
C     | o accumulate mean state for cost evalualtion             |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "cost.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_COST
C     == Local variables
      INTEGER i, j, k
      INTEGER bi, bj
      _RL     deltaTfrac

      deltaTfrac = deltaTClock/lastinterval

C--   Calculate cost function on tile of this instance
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
            DO j=1,sNy
              DO i=1,sNx
                cMeanTheta(i,j,k,bi,bj) = cMeanTheta(i,j,k,bi,bj)
     &                + theta(i,j,k,bi,bj) * deltaTfrac
                cMeanUVel(i,j,k,bi,bj) = cMeanUVel(i,j,k,bi,bj)
     &               + uVel(i,j,k,bi,bj) * deltaTfrac
                cMeanVVel(i,j,k,bi,bj) = cMeanVVel(i,j,k,bi,bj)
     &               + vVel(i,j,k,bi,bj) * deltaTfrac

                cMeanThetaUVel(i,j,k,bi,bj) =
     &               cMeanThetaUVel(i,j,k,bi,bj)
     &               + halfRL*(theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))
     &                 *uvel(i,j,k,bi,bj)
     &                 *maskW(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
     &                 *deltaTfrac
                cMeanThetaVVel(i,j,k,bi,bj) =
     &               cMeanThetaVVel(i,j,k,bi,bj)
     &               + halfRL*(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))
     &                 *vvel(i,j,k,bi,bj)
     &                 *maskS(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
     &                 *deltaTfrac
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

#endif

      RETURN
      END
