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
#include "GRID.h"
#include "FFIELDS.h"
#include "DYNVARS.h"

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# ifdef ALLOW_CTRL
#  include "CTRL_SIZE.h"
#  include "CTRL.h"
#  include "CTRL_GENARR.h"
#  include "CTRL_DUMMY.h"
# endif
# ifdef ALLOW_COST
#  include "cost.h"
# endif
# include "EOS.h"
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  include "EXF_CONSTANTS.h"
#  ifdef ALLOW_TAPENADE
#   include "EXF_PARAM.h"
#   include "EXF_INTERP_SIZE.h"
#   include "EXF_INTERP_PARAM.h"
#  endif
# endif
#endif /* ALLOW_AUTODIFF */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL consTemp_to_poTemp
      EXTERNAL consTemp_to_poTemp

C     !LOCAL VARIABLES:
#if (defined ALLOW_ADDFLUID) || (defined ALLOW_FRICTION_HEATING)
      INTEGER k
#endif
      INTEGER ks
      INTEGER i,j,bi,bj
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'LOAD_FIELDS_DRIVER', myThid )
#endif

C--   Initialise forcing arrays that may receive multiple contributions
#ifdef ALLOW_ADDFLUID
C-- 3 cases: a) prescribe constant 3-D field "addMass" (loaded from addMassFile)
C            b) prescribe time-variable 3-D field "addMass" (not yet coded)
C            c) compute "addMass" at each time-step (sum of contrib from pkgs)
C-  Note: Condition below might need an update once (b) is made available
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

C--   Determine surface temperature, usually just a copy of the surface
C--   level of theta, but for eosType='TEOS10', we need to convert from
C--   conservative temperature to in-situ surface temperature
      IF ( usingPCoords ) THEN
       ks = Nr
      ELSE
       ks = 1
      ENDIF
      IF ( fluidIsWater .AND. eosType .EQ. 'TEOS10' ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          CALL CONVERT_CT2PT(
     I         theta(1-OLx,1-OLy,ks,bi,bj),
     I         salt(1-OLx,1-OLy,ks,bi,bj),
     O         gcmSST(1-OLx,1-OLy,bi,bj),
     I         myTime, myIter, myThid )
C     apply mask (because theta=0 will lead to gcmSST<0)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gcmSST(i,j,bi,bj) = gcmSST(i,j,bi,bj)*maskC(i,j,ks,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF
      ELSE
#else
      ELSEIF ( fluidIsWater ) THEN
#endif
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gcmSST(i,j,bi,bj) = theta(i,j,ks,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C--   Map generic time varying controls to xx_gentim2d
#ifdef ALLOW_CTRL
      if (useCTRL) CALL CTRL_MAP_GENTIM2D( myTime, myIter, myThid )
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

#ifdef ALLOW_DIAGNOSTICS
      IF ( fluidIsWater .AND. useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( gcmSST, 'GCM_SST ',
     &                               0, 1, 0, 1, 1, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE( 'LOAD_FIELDS_DRIVER', myThid )
#endif

      RETURN
      END
