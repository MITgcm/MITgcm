#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_OUTPUT

C     !INTERFACE:
      SUBROUTINE GMREDI_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_OUTPUT
C     | o general routine for GM/Redi output
C     *==========================================================*
C     |   write time-average & snap-shot output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GMREDI_TAVE.h"

C     !INPUT PARAMETERS:
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      CHARACTER*(10) suff
#ifdef ALLOW_TIMEAVE
      INTEGER bi, bj
#endif
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

      IF ( DIFFERENT_MULTIPLE(diagFreq,myTime,deltaTClock)
     &   ) THEN

        IF ( GM_MDSIO ) THEN
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I10.10)') myIter
          ELSE
            CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
          ENDIF
          CALL WRITE_FLD_XYZ_RL( 'GM_Kux.',suff,Kux,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL( 'GM_Kvy.',suff,Kvy,myIter,myThid)
#ifdef GM_EXTRA_DIAGONAL
          IF (GM_ExtraDiag) THEN
            CALL WRITE_FLD_XYZ_RL( 'GM_Kuz.',suff,Kuz,myIter,myThid)
            CALL WRITE_FLD_XYZ_RL( 'GM_Kvz.',suff,Kvz,myIter,myThid)
          ENDIF
#endif
        ENDIF

#ifdef ALLOW_MNC
        IF ( GM_MNC ) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
          ELSE
           pf(1:1) = 'R'
          ENDIF
          CALL MNC_CW_SET_UDIM('gm_inst', -1, myThid)
          CALL MNC_CW_RL_W_S('D','gm_inst',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('gm_inst', 0, myThid)
          CALL MNC_CW_I_W_S('I','gm_inst',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kux',Kux,myThid)
          CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kvy',Kvy,myThid)
#ifdef GM_EXTRA_DIAGONAL
          IF (GM_ExtraDiag) THEN
            CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kuz',Kuz,myThid)
            CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kvz',Kvz,myThid)
          ENDIF
#endif
        ENDIF
#endif /* ALLOW_MNC */

      ENDIF

#ifdef ALLOW_TIMEAVE
C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE( taveFreq, myTime, deltaTClock )
     &     ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZE( GM_Kwx_T, GM_timeAve,
     &                           Nr, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( GM_Kwy_T, GM_timeAve,
     &                           Nr, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( GM_Kwz_T, GM_timeAve,
     &                           Nr, bi, bj, myThid )
#ifdef GM_VISBECK_VARIABLE_K
        IF (GM_Visbeck_alpha.NE.0.)
     &   CALL TIMEAVE_NORMALIZE( Visbeck_K_T, GM_timeAve,
     &                           1 , bi, bj, myThid )
#endif
#ifdef GM_BOLUS_ADVEC
        IF (GM_AdvForm) THEN
         CALL TIMEAVE_NORMALIZE( GM_PsiXtave, GM_timeAve,
     &                           Nr, bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( GM_PsiYtave, GM_timeAve,
     &                           Nr, bi, bj, myThid )
        ENDIF
#endif
        ENDDO
       ENDDO

       IF ( GM_MDSIO ) THEN
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter
         ELSE
           CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
         ENDIF
         CALL WRITE_FLD_XYZ_RL('GM_Kwx-T.',suff,GM_Kwx_T,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('GM_Kwy-T.',suff,GM_Kwy_T,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('GM_Kwz-T.',suff,GM_Kwz_T,myIter,myThid)
#ifdef GM_VISBECK_VARIABLE_K
         IF (GM_Visbeck_alpha.NE.0.)
     &        CALL WRITE_FLD_XY_RL('Visbeck_K-T.',suff,Visbeck_K_T,
     &                             myIter,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
         IF (GM_AdvForm) THEN
           CALL WRITE_FLD_XYZ_RL('GM_PsiXtave.',suff,GM_PsiXtave,
     &                           myIter,myThid)
           CALL WRITE_FLD_XYZ_RL('GM_PsiYtave.',suff,GM_PsiYtave,
     &                           myIter,myThid)
         ENDIF
#endif
       ENDIF

#ifdef ALLOW_MNC
       IF ( GM_MNC ) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('gm_tave', -1, myThid)
         CALL MNC_CW_RL_W_S('D','gm_tave',0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM('gm_tave', 0, myThid)
         CALL MNC_CW_I_W_S('I','gm_tave',0,0,'iter',myIter,myThid)
         CALL MNC_CW_RL_W(pf,'gm_tave',0,0,'Kwx',GM_Kwx_T,myThid)
         CALL MNC_CW_RL_W(pf,'gm_tave',0,0,'Kwy',GM_Kwy_T,myThid)
         CALL MNC_CW_RL_W(pf,'gm_tave',0,0,'Kwz',GM_Kwz_T,myThid)
#ifdef GM_VISBECK_VARIABLE_K
         IF (GM_Visbeck_alpha.NE.0.) THEN
           CALL MNC_CW_RL_W(pf,'gm_tave',0,0,'VisbK',
     &          Visbeck_K_T, myThid)
         ENDIF
#endif
#ifdef GM_BOLUS_ADVEC
         IF (GM_AdvForm) THEN
           CALL MNC_CW_RL_W(
     &          pf,'gm_tave',0,0,'PsiX', GM_PsiXtave, myThid)
           CALL MNC_CW_RL_W(
     &          pf,'gm_tave',0,0,'PsiY', GM_PsiYtave, myThid)
         ENDIF
#endif
       ENDIF
#endif

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET( GM_Kwx_T, Nr, bi, bj, myThid )
         CALL TIMEAVE_RESET( GM_Kwy_T, Nr, bi, bj, myThid )
         CALL TIMEAVE_RESET( GM_Kwz_T, Nr, bi, bj, myThid )
#ifdef GM_VISBECK_VARIABLE_K
         IF (GM_Visbeck_alpha.NE.0.)
     &    CALL TIMEAVE_RESET( Visbeck_K_T, 1, bi, bj, myThid )
#endif
#ifdef GM_BOLUS_ADVEC
         IF (GM_AdvForm) THEN
          CALL TIMEAVE_RESET( GM_PsiXtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GM_PsiYtave, Nr, bi, bj, myThid )
         ENDIF
#endif
         GM_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GMREDI */

      RETURN
      END
