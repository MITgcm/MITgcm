C $Header: /u/gcmpack/MITgcm/model/src/packages_init_fixed.F,v 1.20 2004/02/26 22:31:58 edhill Exp $
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
C       |-- GAD_INIT
C       |
C       |-- OBCS_INIT_FIXED
C       |
C       |-- FLT_INIT
C       |
C       |-- ZONAL_FILT_INIT
C       |
C       |-- AIM_INITIALISE
C       |
C       |-- LAND_INITIALISE

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

C--   Initialize fixed params for GAD
      CALL GAD_INIT( myThid )

C--   Initialize MNC look-up tables
#ifdef ALLOW_MNC
CEH3 NEED RUN-TIME SWITCH:  IF (useOBCS) THEN
      CALL MNC_INIT(myThid)
      CALL MNC_CW_INIT(myThid, sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr)

      CALL INI_MNC_IO(myThid)
CEH3 NEED RUN-TIME SWITCH:  ENDIF
#endif

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
      IF (useGRIDALT) CALL GRIDALT_INITIALISE( myThid )
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

#if (defined (ALLOW_ECCO) && defined (ALLOW_COST))
C--   Initialise ecco-specific cost function.
C--   This needs to preceed the call ctrl_init
C--   in order to provide the weight files
      IF (useECCO) CALL ECCO_COST_INIT_FIXED( myThid )
#endif

#ifdef ALLOW_AUTODIFF
C--   Initialise the control variables
      CALL CTRL_INIT( myThid )
      _BARRIER
#endif

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM
      IF (usePTRACERS) then
       call GCHEM_INIT_FIXED(mythid)
      ENDIF
#endif
#endif

CAMM
#ifdef ALLOW_DIAGNOSTICS
      IF (usediagnostics) then
       call diagnostics_initialise(mythid)
      ENDIF
#endif
CAMM

      RETURN
      END
