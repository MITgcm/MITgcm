#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE PACKAGES_READPARMS(myThid)
C     /==========================================================\
C     | SUBROUTINE PACKAGES_READPARMS                            |
C     | o Read runtime configuration parameters from             |
C     |   file data.${pkg} for every package ${pkg}              |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameters
      IF (useGMRedi) CALL GMREDI_READPARMS( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP parameters
      IF (useKPP) CALL KPP_READPARMS( myThid )
#endif

#ifdef INCLUDE_ECCO_PACKAGE
c--   Initialize ECCO package parameters
      IF (useECCO) CALL ECCO_READPARMS ( myThid )
#endif

      END
