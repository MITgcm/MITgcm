C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.44 2005/09/04 19:24:27 jmc Exp $
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
C       |-- GMREDI_INIT_FIXED
C       |
C       |-- KPP_INIT_FIXED
C       |
C       |-- OPPS_INIT
C       |
C       |-- OBCS_INIT_FIXED
C       |
C       |-- FLT_INIT
C       |
C       |-- TIMEAVE_INIT_FIXED
C       |
C       |-- SHAP_FILT_INIT_FIZED
C       |
C       |-- ZONAL_FILT_INIT
C       |
C       |-- ATM_DIAGNOSTICS_INIT
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
C       |-- CTRL_INIT
C       |
C       |-- PTRACERS_INITIALISE
C       |
C       |-- GCHEM_INIT_FIXED
C       |
C       |-- DIC_INIT_FIXED
C       |
C       |-- THSICE_INIT_FIXED
C       |
C       |-- CPL_INIT_FIXED
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
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

#ifdef ALLOW_RUNCLOCK
      IF ( useRunClock ) THEN
        CALL RUNCLOCK_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_INIT_EARLY( myThid )
        CALL DIAGNOSTICS_MAIN_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
C--   Initialize fixed params for GAD
      CALL GAD_INIT( myThid )
#endif

#ifdef ALLOW_MOM_COMMON
C--   Initialize fixed params for Momentum pkgs (common, fluxform, vecinv)
      IF ( momStepping ) THEN
       CALL MOM_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
       CALL CD_CODE_INIT_FIXED(mythid)
      ENDIF
#endif

#ifdef ALLOW_EXF
C--   Initialize fixed arrays for EXF
c     IF ( useEXF ) THEN
       CALL EXF_INIT_FIXED( myThid )
c     ENDIF
#endif

#ifdef ALLOW_GMREDI
C--   Initialize fixed arrays for GM-Redi
      IF ( useGMRedi ) THEN
       CALL GMREDI_INIT_FIXED( myThid )
      ENDIF
#endif

#ifdef ALLOW_KPP
C--   Initialize fixed arrays for KPP
      IF ( useKPP ) THEN
       CALL KPP_INIT_FIXED( myThid )
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

#ifdef ALLOW_FLT
C--   Initialise Float positions
      IF (useFLT) THEN
       CALL FLT_INIT(nIter0,startTime,myThid )
      _BARRIER
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
      _BARRIER
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
       CALL TIMER_START('GRIDALT_INITIALISE  [PACKAGES_INIT_F]',mythid)
       CALL GRIDALT_INITIALISE( myThid )
       CALL TIMER_STOP ('GRIDALT_INITIALISE  [PACKAGES_INIT_F]',mythid)
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
      CALL SEAICE_COST_INIT_FIXED( myThid )
#endif

#if (defined (ALLOW_ECCO) && defined (ALLOW_COST))
C--   Initialise ecco-specific cost function.
C--   This needs to preceed the call ctrl_init
C--   in order to provide the weight files
      CALL ECCO_COST_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_AUTODIFF
C--   Initialise the control variables
      CALL CTRL_INIT( myThid )
      _BARRIER
#endif

#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PTRACERS_INITIALISE(mythid)
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) CALL GCHEM_INIT_FIXED(mythid)
#endif

#ifdef ALLOW_DIC
C     IF (useDIC)
      CALL DIC_INIT_FIXED(mythid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_INIT_FIXED(mythid)
#endif

#ifdef COMPONENT_MODULE
      IF ( useCoupler ) CALL CPL_INIT_FIXED( mythid )
#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_INIT_FIXED( myThid )
      ENDIF
#endif

      RETURN
      END
