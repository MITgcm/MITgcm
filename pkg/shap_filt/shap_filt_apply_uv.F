C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_uv.F,v 1.2 2002/03/04 01:31:02 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_APPLY_UV
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_APPLY_UV(
     U                     uFld, vFld,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_cwAPPLY_UV
C     | o Apply Shapiro filter on momentum : 
C     |   filter the argments uFld & vFld.
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uFld :: velocity field (U component) on which filter applies
C     vFld :: velocity field (V component) on which filter applies
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of SHAP_FILT_APPLY_UV
      _RL  uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
#ifdef USE_OLD_SHAPIRO_FILTERS
C     == Local variables ==
C     bi,bj,k :: loop index
      INTEGER bi, bj, k
#endif /* USE_OLD_SHAPIRO_FILTERS */

CEOP

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
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.2) THEN
            CALL SHAP_FILT_UV_S2(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_UV_S4(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.20) THEN
            CALL SHAP_FILT_UV_S2G(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.21) THEN
            CALL SHAP_FILT_UV_S2C(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY_UV: Ooops! Bad Shap_funct in UV block'
          ENDIF
        ENDIF

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
