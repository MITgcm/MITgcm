
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_WVEL( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_WVEL                                      |
C     | o Set model initial zonal velocity field.                |
C     |==========================================================|
C     | There are several options for setting the initial        |
C     | zonal velocity field.                                    |
C     |  1. Inline code                                          |
C     |  2. Three-dimensional data from a file. For example from |
C     |     a checkpoint file from a previous integration.       |
C     | In addition to setting the velocity field we also        |
C     | set the initial velocity tendency term here.             |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_WVEL 
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K

C--   Initialise velocity field to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           wVel (I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XYZ_R8(wVel  , myThid )

      RETURN
      END
