C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_uv.F,v 1.1 2001/12/11 14:35:02 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_APPLY_UV(
     U                     uFld, vFld,
     I                     myTime, myIter, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_APPLY                                      |
C     | Shapiro filters the argments uFld & vFld                 |
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
      INTEGER myThid
      _RL myTime
      INTEGER myIter

#ifdef ALLOW_SHAP_FILT
C     == Local variables ==
C     bi, bj, k
#ifdef USE_OLD_SHAPIRO_FILTERS
      INTEGER bi, bj, k
#endif /* USE_OLD_SHAPIRO_FILTERS */

      IF (nShapUV.GT.0) THEN

#ifdef  USE_OLD_SHAPIRO_FILTERS
        _EXCH_XYZ_R8( uFld,myThid )
        _EXCH_XYZ_R8( vFld,myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1, Nr
            CALL SHAP_FILT_U( uFld,bi,bj,k,myTime,myThid )
            CALL SHAP_FILT_V( vFld,bi,bj,k,myTime,myThid )
          ENDDO
         ENDDO
        ENDDO

        _EXCH_XYZ_R8( uFld,myThid )
        _EXCH_XYZ_R8( vFld,myThid )
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

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
