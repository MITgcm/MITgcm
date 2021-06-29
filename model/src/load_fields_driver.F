#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

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
#include "FFIELDS.h"

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# ifdef ALLOW_CTRL
#  include "CTRL_SIZE.h"
#  include "ctrl.h"
#  include "CTRL_GENARR.h"
#  include "ctrl_dummy.h"
# endif
# ifdef ALLOW_COST
#  include "cost.h"
# endif
# include "EOS.h"
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  include "EXF_CONSTANTS.h"
# endif
#endif /* ALLOW_AUTODIFF */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#if (defined ALLOW_ADDFLUID) || (defined ALLOW_FRICTION_HEATING)
      INTEGER i,j,k,bi,bj
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'LOAD_FIELDS_DRIVER', myThid )
#endif

C--   Initialise forcing arrays that may receive multiple contributions
#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.NE.0 .AND. addMassFile.EQ.' ' ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             addMass(i,j,k,bi,bj) =  0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_ADDFLUID */

#ifdef ALLOW_FRICTION_HEATING
      IF ( addFrictionHeating .AND. staggerTimeStep ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             frictionHeating(i,j,k,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_FRICTION_HEATING */

C--   Map generic time varying controls to xx_gentim2d
#ifdef ALLOW_CTRL
      if (useCTRL) CALL CTRL_MAP_GENTIM2D( myTime, myIter, myThid )
# if (defined ALLOW_AUTODIFF_TAMC && defined ALLOW_GENTIM2D_CONTROL)
C     This store-directive avoids storing xx_gentim2d0/1 and subsequently
C     calling this routine in the AD-sweep.
CADJ STORE xx_gentim2d = comlev1, key = ikey_dynamics, kind = isbyte
# endif
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

      IF ( fluidIsWater ) THEN
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
