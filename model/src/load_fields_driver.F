C $Header: /u/gcmpack/MITgcm/model/src/load_fields_driver.F,v 1.7 2008/08/05 22:02:23 jmc Exp $
C $Name:  $


#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

c#ifdef ALLOW_GMREDI
c# include "GMREDI_OPTIONS.h"
c#endif
c#ifdef ALLOW_OBCS
c# include "OBCS_OPTIONS.h"
c#endif

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
C       |-- EXTERNAL_FIELDS_LOAD
C       |
C       |-- OFFLINE_FIELDS_LOAD
C       |
C       |-- GCHEM_FIELDS_LOAD
C       |
C       |-- RBCS_FIELDS_LOAD
C       |
C       |-- AIM_FIELDS_LOAD
C       |
C       |-- CHEAPAML_FIELDS_LOAD


C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "DYNVARS.h"

c#ifdef ALLOW_SHAP_FILT
c# include "SHAP_FILT.h"
c#endif
c#ifdef ALLOW_ZONAL_FILT
c# include "ZONAL_FILT.h"
c#endif
c#ifdef COMPONENT_MODULE
c# include "CPL_PARAMS.h"
c#endif

#ifdef ALLOW_AUTODIFF_TAMC

# include "tamc.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "cost.h"
# include "FFIELDS.h"
# include "EOS.h"
c# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
c#  include "GRID.h"
c# endif
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif
c# ifdef ALLOW_OBCS
c#  include "OBCS.h"
c#  ifdef ALLOW_PTRACERS
c#   include "PTRACERS_SIZE.h"
c#   include "OBCS_PTRACERS.h"
c#  endif
c# endif
c# ifdef ALLOW_CD_CODE
c#  include "CD_CODE_VARS.h"
c# endif
c# ifdef ALLOW_THSICE
c#  include "THSICE_VARS.h"
c# endif
c# ifdef ALLOW_EBM
c#  include "EBM.h"
c# endif
c# ifdef EXACT_CONSERV
c#  include "SURFACE.h"
c# endif
c# ifdef ALLOW_KPP
c#  include "KPP.h"
c# endif
c# ifdef ALLOW_GMREDI
c#  include "GMREDI.h"
c# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     !LOCAL VARIABLES:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &     CALL DEBUG_ENTER( 'LOAD_FIELDS_DRIVER', myThid )
#endif

#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
C--   Bulk-Formulae pkg: load all forcing fields at current time
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('BULKF_FIELDS_LOAD',myThid)
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
      IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('EXF_GETFORCING',myThid)
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
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('CHEAPAML_FIELDS_LOAD',myThid)
#endif
          CALL CHEAPAML_FIELDS_LOAD( myTime,myIter, myThid )
       ENDIF
#endif /*ALLOW_CHEAPAML */



c     IF ( .NOT.useEXF ) THEN
C- note (useEBM): needs modifications to be compatible with this call
      IF ( .NOT.(useEXF.OR.useEBM) ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('EXTERNAL_FIELDS_LOAD',myThid)
#endif
        CALL TIMER_START('EXTERNAL_FLDS_LOAD [LOAD_FLDS_DRIVER]',myThid)
        CALL EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('EXTERNAL_FLDS_LOAD [LOAD_FLDS_DRIVER]',myThid)
#ifdef NONLIN_FRSURF
CADJ STORE SST    = comlev1, key = ikey_dynamics
CADJ STORE SSS    = comlev1, key = ikey_dynamics
# ifdef SHORTWAVE_HEATING
CADJ STORE Qsw    = comlev1, key = ikey_dynamics
# endif
#endif
      ENDIF

#ifdef ALLOW_OFFLINE
      IF ( useOffLine ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('OFFLINE_FIELDS_LOAD',myThid)
#endif /* ALLOW_DEBUG */
        CALL TIMER_START('OFFLINE_FLDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
        CALL OFFLINE_FIELDS_LOAD( myTime, myIter, myThid )
        CALL TIMER_STOP ('OFFLINE_FLDS_LOAD  [LOAD_FLDS_DRIVER]',myThid)
      ENDIF
#endif /* ALLOW_OFFLINE */

#ifdef ALLOW_GCHEM
      IF ( useGCHEM ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_CALL('GCHEM_FIELDS_LOAD',myThid)
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
      IF ( debugLevel .GE. debLevB )
     &     CALL DEBUG_LEAVE( 'LOAD_FIELDS_DRIVER', myThid )
#endif

      RETURN
      END
