C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_apply_ts.F,v 1.12 2009/04/28 18:20:30 jmc Exp $
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
#include "SHAP_FILT.h"

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

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES: ====================================================
C     == Local variables ==
#ifdef USE_OLD_SHAPIRO_FILTERS
C     bi,bj,k :: loop index
      INTEGER bi, bj, k
#endif /* USE_OLD_SHAPIRO_FILTERS */
      INTEGER exchInOut
      CHARACTER*(MAX_LEN_MBUF) suff

CEOP

      IF (nShapT.GT.0 .OR. nShapS.GT.0) THEN

C-    Apply Exchanges on Input field, before the filter (but not after):
        exchInOut = 1
C-    Apply Exchanges on Output field, after the filter (but not before):
        IF ( implicitIntGravWave ) exchInOut = 2

#ifdef USE_OLD_SHAPIRO_FILTERS

        IF ( tempStepping ) _EXCH_XYZ_RL( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_RL( sFld,myThid )

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

        IF ( tempStepping ) _EXCH_XYZ_RL( tFld,myThid )
        IF ( saltStepping ) _EXCH_XYZ_RL( sFld,myThid )

#else

        IF ( tempStepping .AND. nShapT.GT.0) THEN
          IF (Shap_funct.EQ.1) THEN
            CALL SHAP_FILT_TRACER_S1(
     U           tFld, Shap_tmpFld1,
     I           nShapT, Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.2 .OR. Shap_funct.EQ.20
     &                            .OR. Shap_funct.EQ.21) THEN
            CALL SHAP_FILT_TRACER_S2(
     U           tFld, Shap_tmpFld1,
     I           nShapT, exchInOut, Nr, myTime, myIter, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_TRACER_S4(
     U           tFld, Shap_tmpFld1,
     I           nShapT, Nr, myTime, myThid )
c         ELSEIF (Shap_funct.EQ.20) THEN
c           CALL SHAP_FILT_TRACER_S2G(
c    U           tFld, Shap_tmpFld1,
c    I           nShapT, Nr, myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY_TS: Ooops! Bad Shap_funct in T block'
          ENDIF

C-----    Diagnostic of Shapiro Filter effect on temperature :
C         Note: Shap_tmpFld1 from shap_filt_tracer_s2 (and not s1, s4)
C               is directly proportional to Delta-Tr due to the Filter
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4 .AND.
     &     DIFFERENT_MULTIPLE(Shap_diagFreq,myTime,deltaTClock)
     &       ) THEN
           _BARRIER
           WRITE(suff,'(I10.10)') myIter
           CALL WRITE_FLD_XYZ_RL( 'shap_dT.', suff, Shap_tmpFld1,
     &                            myIter, myThid)
           _BARRIER
          ENDIF

#ifdef ALLOW_DIAGNOSTICS
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4
     &                         .AND. useDiagnostics ) THEN
            CALL DIAGNOSTICS_FILL(Shap_tmpFld1,'SHAP_dT ',0,Nr,
     &                                                  0,1,1,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        ENDIF

        IF ( saltStepping .AND. nShapS.GT.0) THEN
          IF (Shap_funct.EQ.1) THEN
            CALL SHAP_FILT_TRACER_S1(
     U           sFld, Shap_tmpFld1,
     I           nShapS, Nr, myTime, myThid )
          ELSEIF (Shap_funct.EQ.2 .OR. Shap_funct.EQ.20
     &                            .OR. Shap_funct.EQ.21) THEN
            CALL SHAP_FILT_TRACER_S2(
     U           sFld, Shap_tmpFld1,
     I           nShapS, exchInOut, Nr, myTime, myIter, myThid )
          ELSEIF (Shap_funct.EQ.4) THEN
            CALL SHAP_FILT_TRACER_S4(
     U           sFld, Shap_tmpFld1,
     I           nShapS, Nr, myTime, myThid )
c         ELSEIF (Shap_funct.EQ.20) THEN
c           CALL SHAP_FILT_TRACER_S2G(
c    U           sFld, Shap_tmpFld1,
c    I           nShapS, Nr, myTime, myThid )
          ELSE
           STOP 'SHAP_FILT_APPLY_TS: Ooops! Bad Shap_funct in S block'
          ENDIF

C-----    Diagnostic of Shapiro Filter effect on salinity :
C         Note: Shap_tmpFld1 from shap_filt_tracer_s2 (and not s1, s4)
C               is directly proportional to Delta-Tr due to the Filter
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4 .AND.
     &     DIFFERENT_MULTIPLE(Shap_diagFreq,myTime,deltaTClock)
     &       ) THEN
           _BARRIER
           WRITE(suff,'(I10.10)') myIter
           CALL WRITE_FLD_XYZ_RL( 'shap_dS.', suff, Shap_tmpFld1,
     &                            myIter, myThid)
           _BARRIER
          ENDIF

#ifdef ALLOW_DIAGNOSTICS
          IF ( Shap_funct.NE.1 .AND. Shap_funct.NE.4
     &                         .AND. useDiagnostics ) THEN
            CALL DIAGNOSTICS_FILL(Shap_tmpFld1,'SHAP_dS ',0,Nr,
     &                                                  0,1,1,myThid)
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        ENDIF

#endif /* USE_OLD_SHAPIRO_FILTERS */

      ENDIF

#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
