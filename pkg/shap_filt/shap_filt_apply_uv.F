C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_uv.F,v 1.7 2005/09/27 22:11:06 jmc Exp $
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

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
#ifdef USE_OLD_SHAPIRO_FILTERS
C     bi,bj,k :: loop index
      INTEGER bi, bj, k
#endif /* USE_OLD_SHAPIRO_FILTERS */
      CHARACTER*(MAX_LEN_MBUF) suff

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
          ELSEIF (Shap_funct.EQ.2 .OR. Shap_funct.EQ.20) THEN
            CALL SHAP_FILT_UV_S2(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_UV_S4(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
c         ELSEIF (Shap_funct.EQ.20) THEN
c           CALL SHAP_FILT_UV_S2G(
c    U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
c    I           Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.21) THEN
            CALL SHAP_FILT_UV_S2C(
     U           uFld, vFld, Shap_tmpFld1, Shap_tmpFld2,
     I           Nr, myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY_UV: Ooops! Bad Shap_funct in UV block'
          ENDIF

C-----    Diagnostic of Shapiro Filter effect on Momentum :
C         Note: Shap_tmpFld1,2 from shap_filt_tracer_s2 (and not s1, s4)
C               are directly proportional to Delta-U,V due to the Filter
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4 .AND.
     &     DIFFERENT_MULTIPLE(Shap_diagFreq,myTime,deltaTClock)
     &       ) THEN
           _BARRIER
           _BEGIN_MASTER( myThid )
           WRITE(suff,'(I10.10)') myIter
           CALL WRITE_FLD_XYZ_RL( 'shap_dU.', suff, Shap_tmpFld1,
     &                             myIter,myThid)
           CALL WRITE_FLD_XYZ_RL( 'shap_dV.', suff, Shap_tmpFld2,
     &                             myIter,myThid)
           _END_MASTER( myThid )
           _BARRIER
          ENDIF

#ifdef ALLOW_DIAGNOSTICS
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4
     &                         .AND. useDiagnostics ) THEN
            CALL DIAGNOSTICS_FILL(Shap_tmpFld1,'SHAP_dU ',0,Nr,
     &                                                  0,1,1,myThid)
            CALL DIAGNOSTICS_FILL(Shap_tmpFld2,'SHAP_dV ',0,Nr,
     &                                                  0,1,1,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        ENDIF

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
