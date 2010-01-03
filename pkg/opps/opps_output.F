C $Header: /u/gcmpack/MITgcm/pkg/opps/opps_output.F,v 1.2 2010/01/03 19:15:31 jmc Exp $
C $Name:  $

#include "OPPS_OPTIONS.h"

CBOP
C     !ROUTINE: OPPS_OUTPUT
C     !INTERFACE:
      SUBROUTINE OPPS_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OPPS_OUTPUT
C     | o Do OPPS diagnostic output.
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_OPPS_STATE_FILES) is
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
#include "OPPS.h"
#include "OPPS_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OPPS
#ifdef ALLOW_TIMEAVE

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER bi, bj
      _RL DDTT
#endif /* ALLOW_TIMEAVE */
CEOP

CMLC----------------------------------------------------------------
CMLC     Dump snapshot of OPPS variables.
CMLC----------------------------------------------------------------
CML
CML      IF ( myIter.NE.nIter0 .AND.
CML     &     DIFFERENT_MULTIPLE( OPPSdumpFreq, myTime, deltaTClock )
CML     &     ) THEN
CML
CML       if (OPPSwriteState) then
CMLC       Write each snap-shot as a new file
CMLC       - creates many files but for large configurations is easier to
CMLC         transfer analyse a particular snap-shots
CML        WRITE(suff,'(I10.10)') myIter
CML        CALL WRITE_FLD_XYZ_RL('OPPSconv.',suff,OPPSconvectCount,
CML     &       myIter,myThid)
CML       endif
CML
CML      ENDIF

C----------------------------------------------------------------
C     Do OPPS time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( OPPStaveFreq.GT.0. _d 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C     Initialize averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( OPPSconvCountTave, Nr, bi, bj, myThid )
          OPPS_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

       ELSE
C     Cumulate OPPS fields (for Time Average)
        DDTT=deltaTclock
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( OPPSconvCountTave, OPPSconvectCount,
     &                           Nr, DDTT, bi, bj, myThid )
C     Keep record of how much time has been integrated over
          OPPS_timeAve(bi,bj) = OPPS_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE( OPPStaveFreq, myTime, deltaTClock )
     &    ) THEN

C     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( OPPSconvCountTave,
     &                            OPPS_timeAve, Nr, bi, bj, myThid )
         ENDDO
        ENDDO

C     Write each snap-shot as a new file
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL('OPPSconvtave.',suff,OPPSconvCountTave,
     &      myIter,myThid)

C     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( OPPSconvCountTave, Nr, bi, bj, myThid )
          OPPS_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C-    end dump-files block
       ENDIF

C-    end if OPPStaveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_OPPS */

      RETURN
      END
