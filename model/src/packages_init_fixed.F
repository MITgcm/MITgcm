C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.3 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_INIT_FIXED( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_INIT_FIXED                           |
C     | o Does initialisation of package-related fixed fields    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C--   Initialize fixed arrays for OBCS
#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_INIT_FIXED( myThid )
      ENDIF
#endif
#ifdef ALLOW_FLT
C--   Initialise Float positions
      IF (useFLT) then
       CALL FLT_INIT(nIter0,startTime,myThid )
      _BARRIER
      ENDIF
#endif

      RETURN
      END
