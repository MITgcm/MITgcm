C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_accumulate_mean.F,v 1.7 2012/03/06 16:45:20 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_COST_ACCUMULATE_MEAN(
     &           myTime, myIter, myThid )
C     *==========================================================*
C     | subroutine seaice_cost_accumulate_mean                   |
C     | o accumulate time-mean state for cost evalualtion        |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_COST
C     == Local variables
      INTEGER bi, bj
      INTEGER i, j
      INTEGER itlo,ithi
      INTEGER jtlo,jthi

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

#ifdef ALLOW_SEAICE_COST_EXPORT

      IF ( myTime .GT. (EndTime - lastinterval) ) THEN

C--   Calculate cost function on tile of this instance
       DO bj = jtlo,jthi
        DO bi = itlo,ithi
         DO j=1,sNy
          DO i=1,sNx
            uHeffExportCell(i,j,bi,bj) = uHeffExportCell(i,j,bi,bj)
     &      + ( 0.92 _d 0*(HEFF(i,j,bi,bj)+HEFF(i-1,j,bi,bj))*0.5 _d 0
     &        + 0.33 _d 0*(HSNOW(i,j,bi,bj)+HSNOW(i-1,j,bi,bj))*0.5 _d 0
     &        )*UICE(i,j,bi,bj)
     &         *_dyG(i,j,bi,bj)/lastinterval*deltaTClock
            vHeffExportCell(i,j,bi,bj) = vHeffExportCell(i,j,bi,bj)
     &      + ( 0.92 _d 0*(HEFF(i,j,bi,bj)+HEFF(i,j-1,bi,bj))*0.5 _d 0
     &        + 0.33 _d 0*(HSNOW(i,j,bi,bj)+HSNOW(i,j-1,bi,bj))*0.5 _d 0
     &        )*VICE(i,j,bi,bj)
     &         *_dxG(i,j,bi,bj)/lastinterval*deltaTClock
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_SEAICE_COST_EXPORT */

#endif /* ALLOW_COST */

      RETURN
      END
