C $Header: /u/gcmpack/MITgcm/model/src/packages_init_variables.F,v 1.28 2003/11/23 01:28:05 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
C       |-- SEAICE_INIT
C       | 
C       |-- OBCS_INIT_VARIABLES
C       |
C       |-- PTRACERS_INIT
C       |
C       |-- LAND_INI_VARS
C       |
C       |-- CTRL_MAP_INI
C       |
C       |-- EXF_INIT
C       |
C       |-- INI_FORCING
C       |
C       |-- ECCO_INIT
C       |
C       |-- COST_INIT
C       |
C       |-- BULKF_INIT
C       |
C       |-- THSICE_INI_VARS

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

#ifdef ALLOW_SEAICE
C--   Initialize SEAICE model.
      IF (useSEAICE) CALL SEAICE_INIT( myThid )
#endif

cph-- moved further down
cph#ifdef ALLOW_OBCS
cphC--   Open boundaries data
cph      IF (useOBCS) THEN
cph        CALL OBCS_INIT_VARIABLES( myThid )
cph      ENDIF
cph#endif

#ifdef ALLOW_PTRACERS
# ifndef ALLOW_AUTODIFF_TAMC
      IF ( usePTRACERS ) THEN
# endif
         CALL PTRACERS_INIT( mythid )
#ifdef ALLOW_GCHEM
         CALL GCHEM_INIT_VARI( mythid )
#endif
# ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
# endif
#endif

#ifdef ALLOW_LAND
      IF ( useLAND ) CALL LAND_INI_VARS( mythid )
#endif

#ifdef ALLOW_AUTODIFF
c--   Initialise auxiliary xx_ fields
      CALL CTRL_INIT_VARIABLES ( mythid )
c--   Map the control variables onto the model state.
      CALL CTRL_MAP_INI( mythid )
      _BARRIER
#endif

#ifdef ALLOW_EXF
      CALL EXF_INIT( mythid )
#else
      CALL INI_FORCING( mythid )
#endif

#ifdef ALLOW_COST
c--   Initialise the cost function.
ceh3 needs an IF ( useCOST ) THEN
      CALL COST_INITVARIA( mythid )
      _BARRIER
#endif

#ifdef ALLOW_OBCS
C--   Open boundaries data
      IF (useOBCS) CALL OBCS_INIT_VARIABLES( myThid )
#endif

#ifdef ALLOW_BULK_FORCE
      IF (useBulkForce) CALL BULKF_INIT( mythid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_INI_VARS( mythid)
#endif


      RETURN
      END
