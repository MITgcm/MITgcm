C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_apply.F,v 1.4 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_APPLY(
     U                     uFld, vFld, tFld, sFld,
     I                     myTime, myIter, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_APPLY                                      |
C     | Shapiro filters the argments uFld, vFld, tFld & sFld     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_SHAP_FILT
#include "SHAP_FILT.h"
#endif

C     == Routine arguments ==
      _RL  uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
      _RL myTime
      INTEGER myIter

#ifdef ALLOW_SHAP_FILT
C     == Local variables ==
C     bi, bj, k
#ifdef USE_OLD_SHAPIRO_FILTERS
      INTEGER bi, bj, k
#endif /* USE_OLD_SHAPIRO_FILTERS */

      IF (nShapT.GT.0.OR.nShapUV.GT.0) THEN

#ifdef  USE_OLD_SHAPIRO_FILTERS
        _EXCH_XYZ_R8( uFld,myThid )
        _EXCH_XYZ_R8( vFld,myThid )
        IF ( tempStepping ) _EXCH_XYZ_R8( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_R8( sFld,myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1, Nr
C--        Update fields in top level according to tendency terms
            CALL SHAP_FILT_U( uFld,bi,bj,k,myTime,myThid )
            CALL SHAP_FILT_V( vFld,bi,bj,k,myTime,myThid )
              IF ( tempStepping ) 
     &       CALL SHAP_FILT_TRACEROLD( tFld,bi,bj,k,myTime,myThid )
            IF ( saltStepping ) 
     &       CALL SHAP_FILT_TRACEROLD( sFld,bi,bj,k,myTime,myThid )
          ENDDO
         ENDDO
        ENDDO

        _EXCH_XYZ_R8( uFld,myThid )
        _EXCH_XYZ_R8( vFld,myThid )
        IF ( tempStepping ) _EXCH_XYZ_R8( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_R8( sFld,myThid )
#else
        IF ( momStepping .AND. nShapUV.GT.0) THEN
          IF (Shap_funct.EQ.1) THEN
            CALL SHAP_FILT_UV_S1(
     U           uFld, vFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.2) THEN
            CALL SHAP_FILT_UV_S2(
     U           uFld, vFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_UV_S4(
     U           uFld, vFld,
     I           myTime, myThid )
          ELSEIF (Shap_funct.EQ.20) THEN
            CALL SHAP_FILT_UV_S2G(
     U           uFld, vFld,
     I           myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY: Ooops! Bad Shap_funct in UV block'
          ENDIF
        ENDIF

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
