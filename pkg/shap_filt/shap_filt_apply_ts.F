C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_ts.F,v 1.2 2001/12/11 14:35:02 jmc Exp $
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

C !LOCAL VARIABLES: ====================================================
C none

CEOP

      IF (nShapT.GT.0) THEN

#ifdef USE_OLD_SHAPIRO_FILTERS

        IF ( tempStepping ) _EXCH_XYZ_R8( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_R8( sFld,myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1, Nr
            IF ( tempStepping ) 
     &       CALL SHAP_FILT_TRACEROLD( tFld,bi,bj,k,myTime,myThid )
            IF ( saltStepping ) 
     &       CALL SHAP_FILT_TRACEROLD( sFld,bi,bj,k,myTime,myThid )
          ENDDO
         ENDDO
        ENDDO

        IF ( tempStepping ) _EXCH_XYZ_R8( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_R8( sFld,myThid )

#else

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

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF

#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
