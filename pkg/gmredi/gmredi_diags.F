C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_diags.F,v 1.1 2000/06/21 19:42:54 adcroft Exp $

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


#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE


C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(taveFreq,myTime,
     &     myTime-deltaTClock)
     &     ) THEN
         
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          CALL TIMEAVER_FINAL_XYZ(GM_Kwx_T,GM_timeave,
     &                 bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(GM_Kwy_T,GM_timeave,
     &                 bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(GM_Kwz_T,GM_timeave,
     &                 bi,bj,K,myThid)
         ENDDO
#ifdef GM_VISBECK_VARIABLE_K
         CALL TIMEAVER_FINAL_XY(Visbeck_K_T,GM_timeave,
     &              bi,bj,myThid)
#endif
        ENDDO
       ENDDO
         
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XYZ_RL('GM_Kwx-T',suff,GM_Kwx_T,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GM_Kwy-T',suff,GM_Kwy_T,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GM_Kwz-T',suff,GM_Kwz_T,myIter,myThid)
#ifdef GM_VISBECK_VARIABLE_K
       CALL WRITE_FLD_XY_RL('Visbeck_K-T',suff,Visbeck_K_T,
     &                        myIter,myThid)
#endif
         
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          CALL TIMEAVER_INI_XYZ(GM_Kwx_T,bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(GM_Kwy_T,bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(GM_Kwz_T,bi,bj,K,myThid)
          GM_TimeAve(k,bi,bj)=0.
         ENDDO
#ifdef GM_VISBECK_VARIABLE_K
         CALL TIMEAVER_INI_XY(Visbeck_K_T,bi,bj,myThid)
#endif
        ENDDO
       ENDDO
         
      ENDIF

#endif /* INCLUDE_DIAGNOSTICS_INTERFACE_CODE */
      
#endif /* ALLOW_GMREDI */
      
      RETURN
      END
