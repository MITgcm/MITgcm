C $Header: /u/gcmpack/MITgcm/model/src/packages_check.F,v 1.5 2000/11/13 16:32:58 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_CHECK                                |
C     | o Check dependances between selected packages            |
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

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CHECK( myThid )
#else
      IF (useKPP) CALL PACKAGES_CHECK_ERROR('KPP', myThid)
#endif

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CHECK( myThid )
#else
      IF (useGMRedi) CALL PACKAGES_CHECK_ERROR('GMREDI', myThid)
#endif

      return
      end
