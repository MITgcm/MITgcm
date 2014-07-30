C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_output.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

CBOP
C     !ROUTINE: KL10_OUTPUT
C     !INTERFACE:
      SUBROUTINE KL10_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KL10_OUTPUT
C     | o Do KL10 diagnostic output.
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_KL10_STATE_FILES) is
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
#include "KL10.h"
#include "KL10_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_KL10

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

C JMK TODO: average KLdiffAr as well as KLviscAr.  Though they are
C never different, but some folks may complain.

C----------------------------------------------------------------
C     Dump snapshot of KL variables.
C----------------------------------------------------------------

      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE( KLdumpFreq, myTime, deltaTClock )
     &   ) THEN

       IF (KLwriteState) THEN
C       Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL('KLviscAr.',suff,KLviscAr,
     &       myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('KLeps.',suff,KLeps,
     &       myIter,myThid)
       ENDIF

      ENDIF

C----------------------------------------------------------------
C     Do KL time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( KLtaveFreq.GT.0. _d 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C     Initialize averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( KLviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( KLdiffKrtave, Nr, bi, bj, myThid )
          KL_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

       ELSE
C     Cumulate KL fields (for Time Average)
        DDTT=deltaTClock
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( KLviscArtave, KLviscAr,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( KLdiffKrtave, KLeps,
     &                           Nr, DDTT, bi, bj, myThid )
C     Keep record of how much time has been integrated over
          KL_timeAve(bi,bj) = KL_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE( KLtaveFreq, myTime, deltaTClock )
     &    ) THEN

C     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( KLviscArtave,
     &                            KL_timeAve, Nr, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( KLdiffKrtave,
     &                            KL_timeAve, Nr, bi, bj, myThid )
         ENDDO
        ENDDO

C     Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'KLviscAr-T.', suff,
     &                          KLviscArTave, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL( 'KLeps-T.', suff,
     &                          KLdiffKrTave, myIter, myThid )

C     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( KLviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( KLdiffKrtave, Nr, bi, bj, myThid )
          KL_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C-    end dump-files block
       ENDIF

C-    end if KLtaveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. myIter.NE.nIter0 ) THEN
       CALL DIAGNOSTICS_FILL(KLviscAr,'KLviscAr',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KLdiffKr,'KLdiffKr',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(KLeps,   'KLeps   ',0,Nr,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_KL10 */

      RETURN
      END
