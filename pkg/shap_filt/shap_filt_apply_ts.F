C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_ts.F,v 1.1 2001/10/27 02:13:46 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C !ROUTINE: SHAP_FILT_APPLY_TS

C !INTERFACE: ==========================================================
      SUBROUTINE SHAP_FILT_APPLY_TS(
     U                     tFld, sFld,
     I                     myTime, myIter, myThid )

C !DESCRIPTION:
C Apply a Shapiro filter on active tracers tFld & sFld

C !USES: ===============================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_SHAP_FILT
#include "SHAP_FILT.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: iteration number
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !INPUT/OUTPUT PARAMETERS: ============================================
C tFld                  :: input and filtered temperature field
C sFld                  :: input and filtered salinity field
      _RL  tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_SHAP_FILT
#ifndef USE_OLD_SHAPIRO_FILTERS 

C !LOCAL VARIABLES: ====================================================
C none

CEOP

      IF (nShapT.GT.0) THEN

        IF ( tempStepping .AND. nShapT.GT.0) THEN
          IF (Shap_funct.EQ.1) THEN
            CALL SHAP_FILT_TRACER_S1(
     U           tFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.2) THEN
            CALL SHAP_FILT_TRACER_S2(
     U           tFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_TRACER_S4(
     U           tFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.20) THEN
            CALL SHAP_FILT_TRACER_S2G(
     U           tFld,
     I           myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY: Ooops! Bad Shap_funct in T block'
          ENDIF
        ENDIF
        IF ( saltStepping .AND. nShapT.GT.0) THEN 
          IF (Shap_funct.EQ.1) THEN
            CALL SHAP_FILT_TRACER_S1(
     U           sFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.2) THEN
            CALL SHAP_FILT_TRACER_S2(
     U           sFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_TRACER_S4(
     U           sFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.20) THEN
            CALL SHAP_FILT_TRACER_S2G(
     U           sFld,
     I           myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY: Ooops! Bad Shap_funct in S block'
          ENDIF
        ENDIF

      ENDIF
#endif /* USE_OLD_SHAPIRO_FILTERS */
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
