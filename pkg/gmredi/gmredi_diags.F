C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_diags.F,v 1.6 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CStartOfInterface
      SUBROUTINE GMREDI_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_DIAGS                                  |
C     | o Do GM/Redi diagnostic output                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#include "GMREDI_DIAGS.h"

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_GMREDI

C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      CHARACTER*(MAX_LEN_MBUF) suff


#if ( defined (GM_NON_UNITY_DIAGONAL) || defined (GM_EXTRA_DIAGONAL) ) 

      IF ( DIFFERENT_MULTIPLE(diagFreq,myTime,myTime-deltaTClock)
     &   ) THEN 
        WRITE(suff,'(I10.10)') myIter
#ifdef GM_NON_UNITY_DIAGONAL
        CALL WRITE_FLD_XYZ_RL( 'GM_Kux.',suff,Kux,myIter,myThid) 
        CALL WRITE_FLD_XYZ_RL( 'GM_Kvy.',suff,Kvy,myIter,myThid) 
#endif
#ifdef GM_EXTRA_DIAGONAL
       IF (GM_ExtraDiag) THEN
        CALL WRITE_FLD_XYZ_RL( 'GM_Kuz.',suff,Kuz,myIter,myThid) 
        CALL WRITE_FLD_XYZ_RL( 'GM_Kvz.',suff,Kvz,myIter,myThid) 
       ENDIF 
#endif
      ENDIF 

#endif /* GM_NON_UNITY_DIAGONAL || GM_EXTRA_DIAGONAL */


#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock)
     &     ) THEN
         
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(GM_Kwx_T,GM_timeave, Nr,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(GM_Kwy_T,GM_timeave, Nr,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(GM_Kwz_T,GM_timeave, Nr,
     &                 bi,bj,myThid)
#ifdef GM_VISBECK_VARIABLE_K
        IF (GM_Visbeck_alpha.NE.0.)
     &   CALL TIMEAVE_NORMALIZ(Visbeck_K_T,GM_timeave, 1,
     &                 bi,bj,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
        IF (GM_AdvForm) THEN
         CALL TIMEAVE_NORMALIZ(GM_PsiXtave,GM_timeave, Nr,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(GM_PsiYtave,GM_timeave, Nr,
     &                 bi,bj,myThid)
        ENDIF
#endif
        ENDDO
       ENDDO
         
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XYZ_RL('GM_Kwx-T.',suff,GM_Kwx_T,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GM_Kwy-T.',suff,GM_Kwy_T,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GM_Kwz-T.',suff,GM_Kwz_T,myIter,myThid)
#ifdef GM_VISBECK_VARIABLE_K
       IF (GM_Visbeck_alpha.NE.0.)
     &  CALL WRITE_FLD_XY_RL('Visbeck_K-T.',suff,Visbeck_K_T,
     &                        myIter,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
       IF (GM_AdvForm) THEN
        CALL WRITE_FLD_XYZ_RL('GM_PsiXtave.',suff,GM_PsiXtave,
     &                        myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('GM_PsiYtave.',suff,GM_PsiYtave,
     &                        myIter,myThid)
       ENDIF
#endif
         
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(GM_Kwx_T,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GM_Kwy_T,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GM_Kwz_T,Nr,bi,bj,myThid)
#ifdef GM_VISBECK_VARIABLE_K
         IF (GM_Visbeck_alpha.NE.0.)
     &    CALL TIMEAVE_RESET(Visbeck_K_T,1,bi,bj,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
         IF (GM_AdvForm) THEN
          CALL TIMEAVE_RESET(GM_PsiXtave,Nr,bi,bj,myThid)
          CALL TIMEAVE_RESET(GM_PsiYtave,Nr,bi,bj,myThid)
         ENDIF
#endif
         DO k=1,Nr
          GM_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
         
      ENDIF

#endif /* ALLOW_TIMEAVE */
      
#endif /* ALLOW_GMREDI */
      
      RETURN
      END
