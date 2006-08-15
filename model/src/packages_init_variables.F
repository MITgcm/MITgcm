C $Header: /u/gcmpack/MITgcm/model/src/packages_init_variables.F,v 1.53 2006/08/15 04:24:03 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
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
C       |-- CD_CODE_INI_VARS
C       |
C       |-- GMREDI_INIT
C       |
C       |-- KPP_INIT
C       |-- KPP_OPEN_DIAGS
C       |
C       |-- PP81_INIT
C       |
C       |-- MY82_INIT
C       |
C       |-- GGL90_INIT
C       |
C       |-- SEAICE_INIT
C       |
C       |-- OBCS_INIT_VARIABLES
C       |
C       |-- PTRACERS_INIT_VARIA
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
C       |-- BULKF_INIT_VARIA
C       |
C       |-- THSICE_INI_VARS
C       |
C       |-- CPL_INI_VARS
C       |
C       |-- REGRID_INIT_FIXED

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "DYNVARS.h"
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode)
     &     CALL DEBUG_ENTER('PACKAGES_INIT_VARIABLES',myThid)
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_INIT_VARIA( myThid )
      ENDIF
#endif

#ifdef ALLOW_CD_CODE
C--   Initialize CD_CODE variables:
C- note(jmc): before packaging CD_CODE, was done within ini_fields (=called before),
C             therefore call CD-ini-vars before others pkg.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CD_CODE_INI',myThid)
#endif
      IF (useCDscheme) CALL CD_CODE_INI_VARS( myThid )
#endif

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameterization
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CD_CODE_INI',myThid)
#endif
      IF (useGMRedi) CALL GMREDI_INIT( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP vertical mixing scheme.
      IF (useKPP) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode)
     &       CALL DEBUG_CALL('KPP_INIT + KPP_OPEN_DIAGS',myThid)
#endif
       CALL KPP_INIT( myThid )
       CALL KPP_OPEN_DIAGS( myThid )
      ENDIF
#endif

#ifdef ALLOW_PP81
C--   Initialize PP81 vertical mixing scheme.
      IF (usePP81) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PP81_INIT',myThid)
#endif
       CALL PP81_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_MY82
C--   Initialize MY82 vertical mixing scheme.
      IF (useMY82) THEN
       CALL MY82_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_GGL90
C--   Initialize GGL90 vertical mixing scheme.
      IF (useGGL90) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('GGL90_INIT',myThid)
#endif
       CALL GGL90_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_SEAICE
C--   Initialize SEAICE model.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('SEAICE_INIT',myThid)
#endif
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
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PTRACERS_INIT_VARIA',myThid)
#endif
         CALL PTRACERS_INIT_VARIA( myThid )
#ifdef ALLOW_GCHEM
# ifndef ALLOW_AUTODIFF_TAMC
         IF (useGCHEM) THEN
# endif
          CALL GCHEM_INIT_VARI( mythid )
# ifndef ALLOW_AUTODIFF_TAMC
         ENDIF
# endif
#endif
# ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
# endif
#endif

#ifdef ALLOW_LAND
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('LAND_INI_VARS',myThid)
#endif
      IF ( useLAND ) CALL LAND_INI_VARS( mythid )
#endif

#ifdef ALLOW_AUTODIFF
CADJ STORE theta = tapelev_init, key = 1
c--   Initialise auxiliary xx_ fields
      CALL CTRL_INIT_VARIABLES ( mythid )
c--   Map the control variables onto the model state.
# ifdef ALLOW_ECCO
      CALL CTRL_MAP_INI_ECCO( mythid )
# else
      CALL CTRL_MAP_INI( mythid )
# endif
      _BARRIER
#endif

#ifdef ALLOW_EXF
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('EXF_INIT',myThid)
#endif
      CALL EXF_INIT( mythid )
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_FORCING',myThid)
#endif
      CALL INI_FORCING( mythid )

#ifdef ALLOW_EBM
# ifdef ALLOW_AUTODIFF
CADJ STORE theta = tapelev_init, key = 1
# endif
      IF (useEBM) CALL EBM_INI_VARS( mythid )
#endif

#ifdef ALLOW_COST
c--   Initialise the cost function.
ceh3 needs an IF ( useCOST ) THEN
      CALL COST_INIT_VARIA( mythid )
      _BARRIER
#endif

#ifdef ALLOW_PROFILES
c--   Initialise the cost function.
      CALL PROFILES_INIT_VARIA( mythid )
      _BARRIER
#endif

#ifdef ALLOW_OBCS
C--   Open boundaries data
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('OBCS_INIT_VARIABLES',myThid)
#endif
      IF (useOBCS) CALL OBCS_INIT_VARIABLES( myThid )
#endif

#ifdef ALLOW_BULK_FORCE
      IF (useBulkForce) CALL BULKF_INIT_VARIA( mythid)
#endif

#ifdef ALLOW_THSICE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('THSICE_INI_VARS',myThid)
#endif
      IF (useThSIce) CALL THSICE_INI_VARS( mythid)
#endif

#ifdef COMPONENT_MODULE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CPL_INI_VARS',myThid)
#endif
      IF (useCoupler) CALL CPL_INI_VARS( mythid )
#endif

#ifdef ALLOW_FIZHI
C Initialize FIZHI state variables
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('FIZHI_INIT_VARS',myThid)
#endif
      IF (useFIZHI) CALL FIZHI_INIT_VARS( mythid )
#endif

#ifdef ALLOW_MATRIX
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('MATRIX_INIT',myThid)
#endif
      IF ( useMATRIX ) CALL MATRIX_INIT( myThid )
#endif

#ifdef ALLOW_RBCS
      IF ( useRBCS ) CALL RBCS_INIT_VARIA( myThid )
#endif

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) CALL MYPACKAGE_INIT_VARIA( myThid )
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode)
     &     CALL DEBUG_LEAVE('PACKAGES_INIT_VARIABLES',myThid)
#endif

#ifdef ALLOW_REGRID
      IF ( useREGRID ) CALL REGRID_INIT_VARIA( myThid )
#endif

      RETURN
      END
