C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/packages_init_variables.F,v 1.2 2001/02/02 21:36:31 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE PACKAGES_INIT_VARIABLES( myThid )
C     /==========================================================\
C     | SUBROUTINE PACKAGES_INIT_VARIABLES                       |
C     | o Does initialisation of package-related variable data   |
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

#ifdef ALLOW_OBCS
C--   Open boundaries data
      IF (useOBCS) THEN
        CALL OBCS_INIT_VARIABLES( myThid )
      ENDIF
#endif

#ifdef INCLUDE_ECCO_PACKAGE
      IF (useECCO) THEN
       CALL ECCO_INIT( myThid )
      ENDIF
#else
      CALL INI_FORCING( myThid )
#endif

#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_INIT( mythid )
#endif

      RETURN
      END
