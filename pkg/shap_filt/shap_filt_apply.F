C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_apply.F,v 1.3 2001/02/04 14:38:50 cnh Exp $
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
      INTEGER bi, bj, k

      IF (nShap.gt.0) THEN

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
        CALL SHAP_FILT_UV(
     U                    uFld, vFld,
     I                    myTime, myThid )
        IF ( tempStepping ) CALL SHAP_FILT_TRACER(
     U           tFld,
     I           myTime, myThid )
        IF ( saltStepping ) CALL SHAP_FILT_TRACER(
     U           sFld,
     I           myTime, myThid )
#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
