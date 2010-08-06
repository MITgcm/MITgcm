C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_output.F,v 1.3 2010/08/06 18:37:05 gforget Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_OUTPUT
C     !INTERFACE:
      SUBROUTINE GGL90_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GGL90_OUTPUT
C     | o Do GGL90 diagnostic output.
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_GGL90_STATE_FILES) is
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
#include "GGL90.h"
#include "GGL90_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

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
C     Dump snapshot of GGL90 variables.
C----------------------------------------------------------------

      IF (
     &     DIFFERENT_MULTIPLE(GGL90dumpFreq,myTime,deltaTClock)
     &     ) THEN

       IF (GGL90writeState) THEN
C       Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        IF ( myIter.NE.nIter0 ) THEN
         CALL WRITE_FLD_XYZ_RL( 'GGL90viscArU.', suff,
     &                           GGL90viscArU, myIter, myThid )
         CALL WRITE_FLD_XYZ_RL( 'GGL90viscArV.', suff,
     &                           GGL90viscArV, myIter, myThid )
         CALL WRITE_FLD_XYZ_RL( 'GGL90diffKr.', suff,
     &                           GGL90diffKr, myIter, myThid )
        ENDIF
        CALL WRITE_FLD_XYZ_RL(  'GGL90TKE.',    suff,
     &                           GGL90TKE,    myIter, myThid )
       ENDIF

      ENDIF

C----------------------------------------------------------------
C     Do GGL90 time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( GGL90taveFreq .GT. 0. _d 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C     Initialize averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( GGL90viscArUtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90viscArVtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90diffKrtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90TKEtave,    Nr, bi, bj, myThid )
          GGL90_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

       ELSE
C     Cumulate GGL90 fields (for Time Average)
        DDTT=deltaTclock
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( GGL90viscArUtave, GGL90viscArU,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( GGL90viscArVtave, GGL90viscArV,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( GGL90diffKrtave, GGL90diffKr,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( GGL90TKEtave,    GGL90TKE,
     &                           Nr, DDTT, bi, bj, myThid )
C     Keep record of how much time has been integrated over
          GGL90_timeAve(bi,bj) = GGL90_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE(GGL90taveFreq,myTime,deltaTClock)
     &    ) THEN

C     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( GGL90viscArUtave,
     &                            GGL90_timeAve, Nr, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( GGL90viscArVtave,
     &                            GGL90_timeAve, Nr, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( GGL90diffKrtave,
     &                            GGL90_timeAve, Nr, bi, bj, myThid )
          CALL TIMEAVE_NORMALIZE( GGL90TKEtave,
     &                            GGL90_timeAve, Nr, bi, bj, myThid )
         ENDDO
        ENDDO

C     Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'GGL90viscArU-T.', suff,
     &                          GGL90viscArUtave, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL( 'GGL90viscArV-T.', suff,
     &                          GGL90viscArVtave, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL( 'GGL90diffKr-T.', suff,
     &                          GGL90diffKrTave, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL( 'GGL90TKE-T.',    suff,
     &                          GGL90TKETave,    myIter, myThid )

C     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( GGL90viscArUtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90viscArVtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90diffKrtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( GGL90TKEtave,    Nr, bi, bj, myThid )
          GGL90_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C-    end dump-files block
       ENDIF

C-    end if GGL90taveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GGL90 */

      RETURN
      END
