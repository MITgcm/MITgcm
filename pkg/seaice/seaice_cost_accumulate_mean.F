#include "SEAICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_COST_ACCUMULATE_MEAN

C     !INTERFACE:
      SUBROUTINE SEAICE_COST_ACCUMULATE_MEAN(
     &           myTime, myIter, myThid )

C     !DESCRIPTION:
C     Accumulate time-mean state for cost evalualtion

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#ifdef ALLOW_COST
# include "SEAICE_COST.h"
# include "cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_COST
#ifdef ALLOW_SEAICE_COST_EXPORT

C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj

      IF ( myTime .GT. (EndTime - lastinterval) ) THEN

C--   Calculate cost function on tile of this instance
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
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
