
#include "CPP_OPTIONS.h"

      SUBROUTINE ECCO_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ECCO_CHECK                                    |
C     | o Check runtime activated packages have been built in.   |
C     |==========================================================|
C     | All packages can be selected/deselected at build time    |
C     | ( when code is compiled ) and activated/deactivated at   |
C     | runtime. This routine does a quick check to trap packages|
C     | that were activated at runtime but that were not compiled|
C     | in at build time.                                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

cph   Moved this content to packages_check.F

      RETURN
      END
