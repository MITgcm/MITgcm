C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_output.F,v 1.3 2013/08/12 00:05:44 jmc Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

CBOP
C     !ROUTINE: MY82_OUTPUT
C     !INTERFACE:
      SUBROUTINE MY82_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_OUTPUT
C     | o Do MY82 diagnostic output.
C     *==========================================================
C     | The following CPP flag (MULTIPLE_RECORD_MY82_STATE_FILES) is
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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "MY82.h"
#include "MY82_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MY82

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
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C----------------------------------------------------------------
C     Dump snapshot of MY82 variables.
C----------------------------------------------------------------

      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE( MYdumpFreq, myTime, deltaTClock )
     &   ) THEN

       IF (MYwriteState .AND. snapshot_mdsio) THEN
C       Write each snap-shot as a new file
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL('MYviscAr.',suff,MYviscAr,
     &       myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('MYdiffKr.',suff,MYdiffKr,
     &       myIter,myThid)
        CALL WRITE_FLD_XY_RL('MYhbl.',suff,MYhbl,
     &       myIter,myThid)
       ENDIF

#ifdef ALLOW_MNC
       IF (MYwriteState .AND. useMNC .AND. snapshot_mnc ) THEN
        IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
         pf(1:1) = 'D'
        ELSE
         pf(1:1) = 'R'
        ENDIF
        CALL MNC_CW_SET_UDIM('my_state', -1, myThid)
        CALL MNC_CW_RL_W_S('D','my_state',0,0,'T',myTime,myThid)
        CALL MNC_CW_SET_UDIM('my_state', 0, myThid)
        CALL MNC_CW_I_W_S('I','my_state',0,0,'iter',myIter,myThid)
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYviscAr', MYviscAr, myThid)
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYdiffKr', MYdiffKr, myThid)
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYhbl', MYhbl, myThid)
       ENDIF
#endif /*  ALLOW_MNC  */

      ENDIF

C----------------------------------------------------------------
C     Do MY82 time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE
      IF ( MYtaveFreq.GT.0. _d 0 ) THEN

       IF ( myIter.EQ.nIter0 ) THEN
C     Initialize averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( MYviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( MYdiffKrtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( MYhbltave,    1 , bi, bj, myThid )
          MY_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

       ELSE
C     Cumulate MY82 fields (for Time Average)
        DDTT=deltaTclock
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_CUMULATE( MYviscArtave, MYviscAr,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( MYdiffKrtave, MYdiffKr,
     &                           Nr, DDTT, bi, bj, myThid )
          CALL TIMEAVE_CUMULATE( MYhbltave,    MYhbl,
     &                           1 , DDTT, bi, bj, myThid )
C     Keep record of how much time has been integrated over
          MY_timeAve(bi,bj) = MY_timeAve(bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDIF

C     Dump files and restart average computation if needed
       IF ( myIter.NE.nIter0 .AND.
     &      DIFFERENT_MULTIPLE( MYtaveFreq, myTime, deltaTClock )
     &    ) THEN

C     Normalize by integrated time
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZE( MYviscArtave,
     &                            MY_timeAve, Nr, bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE( MYdiffKrtave,
     &                            MY_timeAve, Nr, bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE( MYhbltave,
     &                            MY_timeAve, 1 , bi,bj,myThid)
         ENDDO
        ENDDO

        IF (MYwriteState .AND. timeave_mdsio) THEN
C     Write each snap-shot as a new file
         WRITE(suff,'(I10.10)') myIter
         CALL WRITE_FLD_XYZ_RL('MYviscAr-T.', suff,
     &                          MYviscArTave, myIter, myThid )
         CALL WRITE_FLD_XYZ_RL('MYdiffKr-T.', suff,
     &                          MYdiffKrTave, myIter, myThid )
         CALL WRITE_FLD_XY_RL( 'MYhbl-T.',    suff,
     &                          MYhblTave,    myIter, myThid )
        ENDIF

#ifdef ALLOW_MNC
        IF (MYwriteState .AND. useMNC .AND. timeave_mnc) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
          pf(1:1) = 'D'
         ELSE
          pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('my_timeave', -1, myThid)
         CALL MNC_CW_RL_W_S('D','my_timeave',0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM('my_timeave', 0, myThid)
         CALL MNC_CW_I_W_S('I','my_timeave',0,0,'iter',myIter,myThid)
         CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &        'MYviscAr', MYviscArTave, myThid)
         CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &        'MYdiffKr', MYdiffKrTave, myThid)
         CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &        'MYhbl', MYhblTave, myThid)
        ENDIF
#endif /*  ALLOW_MNC  */

C     Reset averages to zero
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET( MYviscArtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( MYdiffKrtave, Nr, bi, bj, myThid )
          CALL TIMEAVE_RESET( MYhbltave,    1 , bi, bj, myThid )
          MY_timeAve(bi,bj) = 0.
         ENDDO
        ENDDO

C-    end dump-files block
       ENDIF

C-    end if MYtaveFreq > 0
      ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
C- note: should move this part to S/R MY82_CALC (like we do in pkg/ggl90)
C     do not fill during call from INITIALISE_VARIA
      IF ( useDiagnostics .AND. myIter.NE.nIter0 ) THEN
       CALL DIAGNOSTICS_FILL(MYviscAr,'MYVISCAR',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(MYdiffKr,'MYDIFFKR',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(MYhbl   ,'MYHBL   ',0,1 ,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MY82 */

      RETURN
      END
