C $Header: /u/gcmpack/MITgcm/model/src/packages_init_variables.F,v 1.10 2002/01/17 16:55:25 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_INIT_VARIABLES
C     !INTERFACE:
      SUBROUTINE PACKAGES_INIT_VARIABLES( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_INIT_VARIABLES                        
C     | o Does initialisation of package-related variable data    
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_INIT_VARIABLES
C       |
C       |-- GMREDI_INIT
C       |
C       |-- KPP_INIT
C       |-- KPP_OPEN_DIAGS
C       |
C       |-- OBCS_INIT_VARIABLES
C       |
C       |-- AIM_INIT
C       |
C       |-- CTRL_MAP_INI
C       |
C       |-- COST_INIT
C       |
C       |-- ECCO_INIT
C       |
C       |-- INI_FORCING

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

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

#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_INIT( mythid )
#endif

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_TANGENTLINEAR_RUN))
c--   Map the control variables onto the model state. 
      call ctrl_map_ini( mythid )
      _BARRIER
c--   Initialise the cost function.
      call cost_init( mythid )
      _BARRIER
#endif

#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
      CALL EXF_INIT( mythid )
#else
      CALL INI_FORCING( mythid )
#endif

#ifdef INCLUDE_ECCO_PACKAGE
      IF (useECCO) THEN
       CALL ECCO_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
