C $Header: /u/gcmpack/MITgcm/model/src/Attic/packages_init.F,v 1.2 2000/09/11 20:42:57 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_INIT                                 |
C     | o Does initialisation of package-related fields          |
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

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameterization
      IF (useGMRedi) CALL GMREDI_INIT( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP vertical mixing scheme.
      IF (useKPP) THEN
       CALL KPP_INIT( myThid )
       CALL KPP_OPEN_DIAGS( myThid )
      ENDIF
#endif

#ifdef INCLUDE_ECCO_PACKAGE

      IF (useECCO) THEN
       CALL ECCO_INIT( myThid )
      ENDIF

#endif

#ifndef INCLUDE_ECCO_PACKAGE
      call ini_forcing( mythid )
#endif

      return
      end
