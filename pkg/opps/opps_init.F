#include "OPPS_OPTIONS.h"

      SUBROUTINE OPPS_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE OPPS_INIT                                     |
C     | o Routine to initialize OPPS parameters and variables.   |
C     |==========================================================|
C     | Initialize OPPS parameters and variables.                |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OPPS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of MY_INIT
      INTEGER myThid

C     !LOCAL VARIABLES :
C     === Local variables ===
C     i,j,k,bi,bj :: Loop counters
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic

      INTEGER i, j, k, bi, bj
#ifdef ALLOW_DIAGNOSTICS
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#endif
CEOP

C-----------------------------------------------------------------------
C Initialize OPPS variables
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           OPPSconvectCount(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'OPPScadj'
      diagTitle = 'OPPS Convective Adjustment Index [0-1] '
      diagUnits = 'fraction        '
      diagCode  = 'SMR     LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */
      RETURN
      END
