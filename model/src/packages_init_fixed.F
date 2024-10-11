#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PACKAGES_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE PACKAGES_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_INIT_FIXED
C     | o Does initialisation of package-related fixed fields
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_INIT_FIXED
C       |
C       |-- RUNCLOCK_INIT
C       |
C       |-- CAL_INIT_FIXED
C       |
C       |-- DIAGNOSTICS_INIT_EARLY
C       |-- DIAGNOSTICS_MAIN_INIT
C       |
C       |-- NEST2W_INIT_FIXED
C       |
C       |-- GAD_INIT_FIXED
C       |
C       |-- MOM_INIT_FIXED
C       |
C       |-- CD_CODE_INIT_FIXED
C       |
C       |-- OBCS_INIT_FIXED
C       |
C       |-- SHAP_FILT_INIT_FIZED
C       |
C       |-- ZONAL_FILT_INIT
C       |
C       |-- EXF_INIT_FIXED
C       |
C       |-- CHEAPAML_INIT_FIXED
C       |
C       |-- OPPS_INIT
C       |
C       |-- PP81_INIT_FIXED
C       |
C       |-- KL10_INIT_FIXED
C       |
C       |-- MY82_INIT_FIXED
C       |
C       |-- GGL90_INIT_FIXED
C       |
C       |-- KPP_INIT_FIXED
C       |
C       |-- GMREDI_INIT_FIXED
C       |
C       |-- BBL_INIT_FIXED
C       |
C       |-- DWNSLP_INIT_FIXED
C       |
C       |-- SEAICE_COST_INIT_FIXED
C       |
C       |-- SMOOTH_INIT_FIXED
C       |
C       |-- ECCO_INIT_FIXED
C       |   |- ECCO_COST_INIT_FIXED
C       |
C       |-- PROFILES_INIT_FIXED
C       |
C       |-- OBSFIT_INIT_FIXED
C       |
C       |-- FLT_INIT_FIXED
C       |
C       |-- LONGSTEP_INIT_FIXED
C       |
C       |-- PTRACERS_INIT_FIXED
C       |
C       |-- GCHEM_INIT_FIXED
C       |
C       |-- RBCS_INIT_FIXED
C       |
C       |-- STREAMICE_INIT_FIXED
C       |
C       |-- SHELFICE_INIT_FIXED
C       |
C       |-- STIC_INIT_FIXED
C       |
C       |-- ICEFRONT_INIT_FIXED
C       |
C       |-- FRAZIL_INIT_FIXED
C       |
C       |-- SEAICE_INIT_FIXED
C       |
C       |-- SALT_PLUME_INIT_FIXED
C       |
C       |-- THSICE_INIT_FIXED
C       |
C       |-- LAND_INIT_FIXED
C       |
C       |-- ATM2D_INIT_FIXED
C       |
C       |-- AIM_INITIALISE
C       |
C       |-- ATM_PHYS_INIT_FIXED
C       |
C       |-- FIZHI_INIT_FIXED
C       |
C       |-- GRIDALT_INITIALISE
C       |
C       |-- TIMEAVE_INIT_FIXED
C       |
C       |-- REGRID_INIT_FIXED
C       |
C       |-- LAYERS_INIT_FIXED
C       |
C       |-- CPL_INIT_FIXED
C       |
C       |-- OASIS_INIT_FIXED
C       |
C       |-- EMBED_FILES_INIT
C       |
C       |-- MYPACKAGE_INIT_FIXED
C       |
C       |-- CTRL_INIT_FIXED
C       |-- CTRL_ADMTLM
C       |
C       |-- DIAGNOSTICS_INIT_FIXED

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id Number
      INTEGER myThid
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('PACKAGES_INIT_FIXED',myThid)
#endif

#ifdef ALLOW_RUNCLOCK
      IF ( useRunClock ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('RUNCLOCK_INIT',myThid)
# endif
        CALL RUNCLOCK_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_CAL
C--   Initialise the calendar package.
      IF ( useCAL ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CAL_INIT_FIXED',myThid)
# endif
        CALL CAL_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_CAL */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C-    needs to call DIAGNOSTICS_INIT_EARLY before all package-diag-init calls
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DIAGNOSTICS_INIT_EARLY',myThid)
# endif
        CALL DIAGNOSTICS_INIT_EARLY( myThid )
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DIAGNOSTICS_MAIN_INIT',myThid)
# endif
        CALL DIAGNOSTICS_MAIN_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_NEST2W_COMMON
C--   Initialise the 2-Way Nesting packages
      IF ( useNest2W_parent .OR. useNest2W_child ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('NEST2W_INIT_FIXED',myThid)
# endif
        CALL NEST2W_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_NEST2W_COMMON */

#ifdef ALLOW_GENERIC_ADVDIFF
C--   Initialize fixed params for GAD
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GAD_INIT_FIXED',myThid)
# endif
      CALL GAD_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_MOM_COMMON
C--   Initialize fixed params for Momentum pkgs (common, fluxform, vecinv)
      IF ( momStepping ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('MOM_INIT_FIXED',myThid)
# endif
        CALL MOM_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CD_CODE_INIT_FIXED',myThid)
# endif
        CALL CD_CODE_INIT_FIXED(myThid)
      ENDIF
#endif

C--   Initialize fixed arrays for OBCS
#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OBCS_INIT_FIXED',myThid)
# endif
        CALL OBCS_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_SHAP_FILT
C--   Shapiro filter initialisation
      IF (useSHAP_FILT) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SHAP_FILT_INIT_FIXED',myThid)
# endif
        CALL SHAP_FILT_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_ZONAL_FILT
C--   Latitude circle filter initialisation
      IF (useZONAL_FILT) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('ZONAL_FILT_INIT',myThid)
# endif
        CALL ZONAL_FILT_INIT(myThid)
      ENDIF
#endif

#ifdef ALLOW_EXF
C--   Initialize fixed arrays for EXF
      IF ( useEXF ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXF_INIT_FIXED',myThid)
# endif
        CALL EXF_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_CHEAPAML
      IF ( useCheapAML ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CHEAPAML_INIT_FIXED',myThid)
# endif
        CALL CHEAPAML_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_OPPS
      IF (useOPPS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OPPS_INIT',myThid)
# endif
        CALL OPPS_INIT( myThid )
      ENDIF
#endif /* ALLOW_OPPS */

#ifdef ALLOW_PP81
C--   Initialize fixed arrays for PP81
      IF ( usePP81 ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('PP81_INIT_FIXED',myThid)
# endif
        CALL PP81_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_KL10
C--   Initialize fixed arrays for KL10
      IF ( useKL10 ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('KL10_INIT_FIXED',myThid)
# endif
        CALL KL10_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_KL10 */

#ifdef ALLOW_MY82
C--   Initialize fixed arrays for MY82
      IF ( useMY82 ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('MY82_INIT_FIXED',myThid)
# endif
        CALL MY82_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_GGL90
C--   Initialize fixed arrays for GGL90
      IF ( useGGL90 ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GGL90_INIT_FIXED',myThid)
# endif
        CALL GGL90_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_KPP
C--   Initialize fixed arrays for KPP
      IF ( useKPP ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('KPP_INIT_FIXED',myThid)
# endif
        CALL KPP_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_GMREDI
C--   Initialize fixed arrays for GM-Redi
      IF ( useGMRedi ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GMREDI_INIT_FIXED',myThid)
# endif
        CALL GMREDI_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_BBL
C--   Initialize fixed arrays for BBL pkg
      IF ( useBBL ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('BBL_INIT_FIXED',myThid)
# endif
        CALL BBL_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_DOWN_SLOPE
C--   Initialize fixed arrays for Down-Slope pkg
      IF ( useDOWN_SLOPE ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DWNSLP_INIT_FIXED',myThid)
# endif
        CALL DWNSLP_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_SEAICE
C--   Initialise ecco-specific cost function.
C--   This needs to preceed the call CTRL_INIT_FIXED
C--   in order to provide the weight files
      IF (useSEAICE) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SEAICE_COST_INIT_FIXED',myThid)
# endif
        CALL SEAICE_COST_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_ECCO
      IF (useECCO) THEN
C--   Calls ECCO_COST_INIT_FIXED which initialises ecco-specific cost
C--   function. This needs to preceed CTRL_INIT_FIXED call in order to
C--   provide the weight files.
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('ECCO_INIT_FIXED',myThid)
# endif
        CALL ECCO_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_PROFILES
      IF (usePROFILES) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('PROFILES_INIT_FIXED',myThid)
# endif
        CALL PROFILES_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_OBSFIT
      IF (useOBSFIT) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OBSFIT_INIT_FIXED',myThid)
# endif
        CALL OBSFIT_INIT_FIXED( myThid )
      ENDIF
#endif

C--   Old place where CTRL_INIT_FIXED & CTRL_ADMTLM used to be called

#ifdef ALLOW_FLT
C--   Initialise fixed array for Float pkg
      IF ( useFLT ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('FLT_INIT_FIXED',myThid)
# endif
        CALL FLT_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_LONGSTEP
      IF (usePTRACERS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LONGSTEP_INIT_FIXED',myThid)
# endif
        CALL LONGSTEP_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('PTRACERS_INIT_FIXED',myThid)
# endif
        CALL PTRACERS_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GCHEM_INIT_FIXED',myThid)
# endif
        CALL GCHEM_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_RBCS
      IF (useRBCS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('RBCS_INIT_FIXED',myThid)
# endif
        CALL RBCS_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_STREAMICE
      IF (useStreamIce) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('STREAMICE_INIT_FIXED',myThid)
# endif
#ifndef ALLOW_OPENAD
       CALL STREAMICE_INIT_FIXED( myThid )
#else
       CALL OPENAD_STREAMICE_INIT_FIXED( myThid )
#endif
      ENDIF
#endif /* ALLOW_STREAMICE */

#ifdef ALLOW_SHELFICE
      IF (useShelfIce) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SHELFICE_INIT_FIXED',myThid)
# endif
        CALL SHELFICE_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_STEEP_ICECAVITY
      IF (useSTIC) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('STIC_INIT_FIXED',myThid)
# endif
        CALL STIC_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_STEEP_ICECAVITY */

#ifdef ALLOW_SMOOTH
      IF (useSMOOTH) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SMOOTH_INIT_FIXED',myThid)
# endif
        CALL SMOOTH_INIT_FIXED(myThid)
      ENDIF
#endif /* ALLOW_SMOOTH */

#ifdef ALLOW_ICEFRONT
      IF (useICEFRONT) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('ICEFRONT_INIT_FIXED',myThid)
# endif
        CALL ICEFRONT_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_FRAZIL
      IF (useFRAZIL) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('FRAZIL_INIT_FIXED',myThid)
# endif
        CALL FRAZIL_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_SEAICE
      IF (useSEAICE) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SEAICE_INIT_FIXED',myThid)
# endif
        CALL SEAICE_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_SALT_PLUME
      IF (useSALT_PLUME) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SALT_PLUME_INIT_FIXED',myThid)
# endif
        CALL SALT_PLUME_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('THSICE_INIT_FIXED',myThid)
# endif
        CALL THSICE_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_LAND
C--   Initialise Land package
      IF ( useLand ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LAND_INIT_FIXED',myThid)
# endif
        CALL LAND_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_ATM2D
      IF ( useAtm2d ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('ATM2D_INIT_FIXED',myThid)
# endif
        CALL ATM2D_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_AIM
C--   Initialise & Read AIM physical parameters
      IF (useAIM) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('AIM_INITIALISE',myThid)
# endif
        CALL AIM_INITIALISE( myThid )
      ENDIF
#endif

#ifdef ALLOW_ATM_PHYS
C--   Initialise Atm_Phys pkg
      IF ( useAtm_Phys ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('ATM_PHYS_INIT_FIXED',myThid)
# endif
        CALL ATM_PHYS_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_ATM_PHYS */

#ifdef ALLOW_FIZHI
C--   Initialise & Read FIZHI physical parameters
      IF (useFIZHI) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('FIZHI_INIT_FIXED',myThid)
# endif
        CALL FIZHI_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_GRIDALT
C--   Initialise GRIDALT parameters - the alternative grid
      IF (useGRIDALT) then
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GRIDALT_INITIALISE',myThid)
# endif
        CALL GRIDALT_INITIALISE( myThid )
      ENDIF
#endif

#ifdef ALLOW_TIMEAVE
C     IF (useTIMEAVE) THEN
      IF ( taveFreq.GT.0. ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('TIMEAVE_INIT_FIXED',myThid)
# endif
        CALL TIMEAVE_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_REGRID
      IF ( useREGRID ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('REGRID_INIT_FIXED',myThid)
# endif
        CALL REGRID_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_LAYERS
      IF ( useLayers ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('LAYERS_INIT_FIXED',myThid)
# endif
        CALL LAYERS_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_LAYERS */

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CPL_INIT_FIXED',myThid)
# endif
        CALL CPL_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_OASIS
C--   needs to be called after the OASIS_INIT
      IF (useOASIS) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OASIS_INIT_FIXED',myThid)
# endif
        CALL OASIS_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_EMBED_FILES
      IF ( useEMBED_FILES ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EMBED_FILES_INIT',myThid)
# endif
        CALL EMBED_FILES_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('MYPACKAGE_INIT_FIXED',myThid)
# endif
        CALL MYPACKAGE_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_CTRL
      IF (useCTRL) THEN
C--   Initialise the control variables. Moved to the end of this S/R
C     to allow other pkgs to set mask and weight fields.
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CTRL_INIT_FIXED',myThid)
# endif
        CALL CTRL_INIT_FIXED( myThid )
      ENDIF

# ifdef ALLOW_ADMTLM
      IF (useCTRL) THEN
C--   Initialise ADMTLM
#  ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CTRL_ADMTLM',myThid)
#  endif
        CALL CTRL_ADMTLM( myThid )
      ENDIF
# endif /* ALLOW_ADMTLM */
#endif /* ALLOW_CTRL */

#ifdef ALLOW_DIAGNOSTICS
C-    needs to call DIAGNOSTICS_INIT_FIXED after all package-diag-init calls
      IF ( useDiagnostics ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DIAGNOSTICS_INIT_FIXED',myThid)
# endif
        CALL DIAGNOSTICS_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('PACKAGES_INIT_FIXED',myThid)
#endif

      RETURN
      END
