C $Header: /u/gcmpack/MITgcm/model/src/load_fields_driver.F,v 1.11 2011/08/28 22:26:39 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: LOAD_FIELDS_DRIVER
C     !INTERFACE:
      SUBROUTINE LOAD_FIELDS_DRIVER( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | SUBROUTINE LOAD_FIELDS_DRIVER
C     | o Load external forcing fields from file
C     *==================================================================
C     *==================================================================
C     \ev

C     !CALLING SEQUENCE:
C     LOAD_FIELDS_DRIVER
C       |
C       |-- BULKF_FIELDS_LOAD
C       |
C       |-- EXF_GETFORCING
C       |
C       |-- CHEAPAML_FIELDS_LOAD
C       |
C       |-- EXTERNAL_FIELDS_LOAD
C       |
C       |-- OFFLINE_FIELDS_LOAD
C       |
C       |-- GCHEM_FIELDS_LOAD
C       |
C       |-- RBCS_FIELDS_LOAD
C       |
C       |-- AIM_FIELDS_LOAD

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "FFIELDS.h"
# include "EOS.h"
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'LOAD_FIELDS_DRIVER', myThid )
#endif

#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
C--   Bulk-Formulae pkg: load all forcing fields at current time
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('BULKF_FIELDS_LOAD',myThid)
#endif
        CALL TIMER_START('BULKF_FIELDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
        CALL BULKF_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('BULKF_FIELDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
      ENDIF
#endif /* ALLOW_BULK_FORCE */

C--   Call external forcing package
#ifdef ALLOW_EXF
      IF (useEXF) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXF_GETFORCING',myThid)
# endif
        CALL TIMER_START('EXF_GETFORCING     [LOAD_FLDS_DRIVER]',myThid)
        CALL EXF_GETFORCING( myTime, myIter, myThid )
        CALL TIMER_STOP ('EXF_GETFORCING     [LOAD_FLDS_DRIVER]',myThid)
      ENDIF
#endif /* ALLOW_EXF */

C--   Call external CheapAML forcing package
#ifdef ALLOW_CHEAPAML
      IF ( useCheapAML  ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CHEAPAML_FIELDS_LOAD',myThid)
#endif
        CALL CHEAPAML_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif /*ALLOW_CHEAPAML */

      IF ( .NOT.(useEXF.OR.useCheapAML) ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXTERNAL_FIELDS_LOAD',myThid)
#endif
        CALL TIMER_START('EXTERNAL_FLDS_LOAD [LOAD_FLDS_DRIVER]',myThid)
        CALL EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('EXTERNAL_FLDS_LOAD [LOAD_FLDS_DRIVER]',myThid)
#ifdef NONLIN_FRSURF
CADJ STORE SST    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SSS    = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef SHORTWAVE_HEATING
CADJ STORE Qsw    = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif
      ENDIF

#ifdef ALLOW_OFFLINE
      IF ( useOffLine ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OFFLINE_FIELDS_LOAD',myThid)
#endif /* ALLOW_DEBUG */
        CALL TIMER_START('OFFLINE_FLDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
        CALL OFFLINE_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('OFFLINE_FLDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
      ENDIF
#endif /* ALLOW_OFFLINE */

#ifdef ALLOW_GCHEM
      IF ( useGCHEM ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GCHEM_FIELDS_LOAD',myThid)
#endif /* ALLOW_DEBUG */
        CALL GCHEM_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_GCHEM */

#ifdef ALLOW_RBCS
      IF ( useRBCS ) THEN
        CALL RBCS_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
C-     Update AIM bottom boundary data
        CALL AIM_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE( 'LOAD_FIELDS_DRIVER', myThid )
#endif

      RETURN
      END
