C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.74 2011/08/07 07:08:15 dimitri Exp $
C $Name:  $

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
C       |-- DIAGNOSTICS_INIT_EARLY
C       |-- DIAGNOSTICS_MAIN_INIT
C       |
C       |-- GAD_INIT
C       |
C       |-- MOM_INIT_FIXED
C       |
C       |-- CD_CODE_INIT_FIXED
C       |
C       |-- EXF_INIT_FIXED
C       |
C       |-- CHEAPAML_INIT_FIXED
C       |
C       |-- GMREDI_INIT_FIXED
C       |
C       |-- DWNSLP_INIT_FIXED
C       |
C       |-- BBL_INIT_FIXED
C       |
C       |-- KPP_INIT_FIXED
C       |
C       |-- MY82_INIT_FIXED
C       |
C       |-- GGL90_INIT_FIXED
C       |
C       |-- OPPS_INIT
C       |
C       |-- OBCS_INIT_FIXED
C       |
C       |-- TIMEAVE_INIT_FIXED
C       |
C       |-- SHAP_FILT_INIT_FIZED
C       |
C       |-- ZONAL_FILT_INIT
C       |
C       |-- AIM_INITIALISE
C       |
C       |-- GRIDALT_INITIALISE
C       |
C       |-- FIZHI_INIT_FIXED
C       |
C       |-- LAND_INITIALISE
C       |
C       |-- SEAICE_COST_INIT_FIXED
C       |
C       |-- ECCO_COST_INIT_FIXED
C       |
C       |-- PROFILES_INIT_FIXED
C       |
C       |-- CTRL_INIT
C       |
C       |-- FLT_INIT_FIXED
C       |
C       |-- LONGSTEP_INIT_FIXED
C       |
C       |-- PTRACERS_INIT_FIXED
C       |
C       |-- GCHEM_INIT_FIXED
C       |
C       |-- THSICE_INIT_FIXED
C       |
C       |-- SHELFICE_INIT_FIXED
C       |
C       |-- ICEFRONT_INIT_FIXED
C       |
C       |-- CPL_INIT_FIXED
C       |
C       |-- ATM2D_INIT_FIXED
C       |
C       |-- REGRID_INIT_FIXED
C       |
C       |-- LAYERS_INIT_FIXED
C       |
C       |-- SALT_PLUME_INIT_FIXED
C       |
C       |-- MYPACKAGE_INIT_FIXED
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

#ifdef ALLOW_RUNCLOCK
      IF ( useRunClock ) THEN
        CALL RUNCLOCK_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C-    needs to call DIAGNOSTICS_INIT_EARLY before all package-diag-init calls
        CALL DIAGNOSTICS_INIT_EARLY( myThid )
        CALL DIAGNOSTICS_MAIN_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
C--   Initialize fixed params for GAD
      CALL GAD_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_MOM_COMMON
C--   Initialize fixed params for Momentum pkgs (common, fluxform, vecinv)
      IF ( momStepping ) THEN
       CALL MOM_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
       CALL CD_CODE_INIT_FIXED(myThid)
      ENDIF
#endif

#ifdef ALLOW_EXF
C--   Initialize fixed arrays for EXF
      IF ( useEXF ) THEN
       CALL EXF_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_CHEAPAML
      IF ( useCheapAML ) CALL CHEAPAML_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_GMREDI
C--   Initialize fixed arrays for GM-Redi
      IF ( useGMRedi ) THEN
       CALL GMREDI_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_DOWN_SLOPE
C--   Initialize fixed arrays for Down-Slope pkg
      IF ( useDOWN_SLOPE ) THEN
       CALL DWNSLP_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_BBL
C--   Initialize fixed arrays for BBL pkg
      IF ( useBBL ) THEN
       CALL BBL_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_KPP
C--   Initialize fixed arrays for KPP
      IF ( useKPP ) THEN
       CALL KPP_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_MY82
C--   Initialize fixed arrays for MY82
      IF ( useMY82 ) THEN
       CALL MY82_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_GGL90
C--   Initialize fixed arrays for GGL90
      IF ( useGGL90 ) THEN
       CALL GGL90_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_OPPS
      IF (useOPPS) CALL OPPS_INIT( myThid )
#endif /* ALLOW_OPPS */

C--   Initialize fixed arrays for OBCS
#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_TIMEAVE
C     IF (useTIMEAVE) THEN
      IF ( taveFreq.GT.0. ) THEN
       CALL TIMEAVE_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_SHAP_FILT
C--   Shapiro filter initialisation
      IF (useSHAP_FILT) THEN
       CALL SHAP_FILT_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_ZONAL_FILT
C--   Latitude circle filter initialisation
      IF (useZONAL_FILT) THEN
       CALL ZONAL_FILT_INIT(myThid)
      ENDIF
#endif

#ifdef ALLOW_AIM
C--   Initialise & Read AIM physical parameters
      IF (useAIM) CALL AIM_INITIALISE( myThid )
#endif

C AMM
#ifdef ALLOW_GRIDALT
C--   Initialise GRIDALT parameters - the alternative grid
      IF (useGRIDALT) then
       CALL TIMER_START('GRIDALT_INITIALISE  [PACKAGES_INIT_F]',myThid)
       CALL GRIDALT_INITIALISE( myThid )
       CALL TIMER_STOP ('GRIDALT_INITIALISE  [PACKAGES_INIT_F]',myThid)
      ENDIF

#endif

#ifdef ALLOW_FIZHI
C--   Initialise & Read FIZHI physical parameters
      IF (useFIZHI) CALL FIZHI_INIT_FIXED( myThid )
#endif
C AMM

#ifdef ALLOW_LAND
C--   Initialise & Read Land package parameters
      IF (useLand) CALL LAND_INITIALISE( myThid )
#endif

#if (defined (ALLOW_SEAICE) && defined (ALLOW_COST))
C--   Initialise ecco-specific cost function.
C--   This needs to preceed the call ctrl_init
C--   in order to provide the weight files
      IF (useSEAICE) CALL SEAICE_COST_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_SMOOTH
      CALL SMOOTH_INIT_FIXED(myThid)
#endif /* ALLOW_SMOOTH */

#if (defined (ALLOW_ECCO) && defined (ALLOW_COST))
C--   Initialise ecco-specific cost function.
C--   This needs to preceed the call ctrl_init
C--   in order to provide the weight files
      CALL ECCO_COST_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_PROFILES
      CALL PROFILES_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_AUTODIFF
C--   Initialise the control variables
      CALL CTRL_INIT( myThid )
#endif

#ifdef ALLOW_ADMTLM
C--   Initialise ADMTLM
      CALL CTRL_ADMTLM( myThid )
#endif

#ifdef ALLOW_FLT
C--   Initialise fixed array for Float pkg
      IF ( useFLT ) CALL FLT_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_LONGSTEP
      IF (usePTRACERS) CALL LONGSTEP_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PTRACERS_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) CALL GCHEM_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_RBCS
      IF (useRBCS) CALL RBCS_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_SEAICE
      IF (useSEAICE) CALL SEAICE_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_SHELFICE
      IF (useShelfIce) CALL SHELFICE_INIT_FIXED( myThid )
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_ICEFRONT
      IF (useICEFRONT) CALL ICEFRONT_INIT_FIXED( myThid )
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_EMBED_FILES
      IF ( useEMBED_FILES ) CALL EMBED_FILES_INIT( myThid )
#endif

#ifdef ALLOW_REGRID
      IF ( useREGRID ) CALL REGRID_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_LAYERS
      IF ( useLayers ) CALL LAYERS_INIT_FIXED( myThid )
#endif /* ALLOW_LAYERS */

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) CALL CPL_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_ATM2D
      IF ( useAtm2d ) CALL ATM2D_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_SALT_PLUME
      IF (useSALT_PLUME) CALL SALT_PLUME_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_OASIS
C--   needs to be called after the OASIS_INIT
      IF (useOASIS) CALL OASIS_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) CALL MYPACKAGE_INIT_FIXED(myThid)
#endif

#ifdef ALLOW_DIAGNOSTICS
C-    needs to call DIAGNOSTICS_INIT_FIXED after all package-diag-init calls
      IF ( useDiagnostics ) CALL DIAGNOSTICS_INIT_FIXED( myThid )
#endif

      RETURN
      END
