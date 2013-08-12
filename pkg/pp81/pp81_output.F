C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_output.F,v 1.4 2013/08/12 00:05:12 jmc Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

CBOP
C     !ROUTINE: PP81_OUTPUT
C     !INTERFACE:
      SUBROUTINE PP81_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PP81_OUTPUT
C     | o Do PP81 diagnostic output.
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_PP81_STATE_FILES) is
C     | #define/#undefed here since it is specific to this routine
C     | and very user-preference specific.
C     |
C     | If #undefed (default) the state files are written as in all versions
C     | prior to checkpoint32, where a file is created per variable, per time
C     | and per tile. This *has* to be the default because most users use this
C     | mode and all utilities and scripts (diagnostic) assume this form.
C     | It is also robust, as explained below.
C     |
C     | If #defined, subsequent snap-shots are written as records in the
C     | same file (no iteration number in filenames).
C     | Advantages: - fewer files
C     |       - for small problems, is easy to copy the output around
C     | Disadvantages:
C     |       - breaks a lot of diagnostic scripts
C     |       - for large or long problems this creates huge files
C     |       - is an unexpected, unsolicited change in behaviour which came
C     |         as a surprise (in c32) and inconvenience to several users
C     |       - can not accomodate changing the frequency of output
C     |         after a pickup (this is trivial in previous method
C     |         but needs new code and parameters in this new method)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PP81.h"
#include "PP81_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PP81

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
#ifdef ALLOW_TIMEAVE
      INTEGER bi, bj
      _RL DDTT
#endif
CEOP

C----------------------------------------------------------------
C     Dump snapshot of PP variables.
C----------------------------------------------------------------

      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE( PPdumpFreq, myTime, deltaTClock )
     &   ) THEN

       IF (PPwriteState) THEN
C       Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL('PPviscAr.',suff,PPviscAr,
     &       myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('PPdiffKr.',suff,PPdiffKr,
     &       myIter,myThid)
       ENDIF

      ENDIF

C----------------------------------------------------------------
C     Do PP time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( PPtaveFreq.GT.0. _d 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C     Initialize averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( PPviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( PPdiffKrtave, Nr, bi, bj, myThid )
          PP_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

       ELSE
C     Cumulate PP fields (for Time Average)
        DDTT=deltaTclock
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( PPviscArtave, PPviscAr,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( PPdiffKrtave, PPdiffKr,
     &                           Nr, DDTT, bi, bj, myThid )
C     Keep record of how much time has been integrated over
          PP_timeAve(bi,bj) = PP_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE( PPtaveFreq, myTime, deltaTClock )
     &    ) THEN

C     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( PPviscArtave,
     &                            PP_timeAve, Nr, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( PPdiffKrtave,
     &                            PP_timeAve, Nr, bi, bj, myThid )
         ENDDO
        ENDDO

C     Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'PPviscAr-T.', suff,
     &                          PPviscArTave, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL( 'PPdiffKr-T.', suff,
     &                          PPdiffKrTave, myIter, myThid )

C     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( PPviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( PPdiffKrtave, Nr, bi, bj, myThid )
          PP_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C-    end dump-files block
       ENDIF

C-    end if PPtaveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
C- note: should move this part to S/R MY82_CALC (like we do in pkg/ggl90)
C     do not fill during call from INITIALISE_VARIA
      IF ( useDiagnostics .AND. myIter.NE.nIter0 ) THEN
       CALL DIAGNOSTICS_FILL(PPviscAr,'PPviscAr',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(PPdiffKr,'PPdiffKr',0,Nr,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_PP81 */

      RETURN
      END
