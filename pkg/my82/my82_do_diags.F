C $Header: /u/gcmpack/MITgcm/pkg/my82/Attic/my82_do_diags.F,v 1.5 2008/04/14 10:09:34 mlosch Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

#undef  MULTIPLE_RECORD_MY82_STATE_FILES
CBOP
C     !ROUTINE: MY82_DO_DIAGS
C     !INTERFACE:
      SUBROUTINE MY82_DO_DIAGS( myCurrentTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_DO_DIAGS                                  
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
#include "MY82_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_MY82

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT
      CHARACTER*(1) pf
CEOP

      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
       pf(1:1) = 'D'
      ELSE
       pf(1:1) = 'R'
      ENDIF

C----------------------------------------------------------------
C     Dump snapshot of MY82 variables.
C----------------------------------------------------------------

      IF (
     &     DIFFERENT_MULTIPLE(MYdumpFreq,
     &                        myCurrentTime,deltaTClock)
     &     ) THEN
         
       IF (MYmixingMaps) THEN
        CALL PLOT_FIELD_XYRL  ( MYhbl   , 'MYhbl'   ,    
     &       myIter, myThid )
        CALL PLOT_FIELD_XYZRL ( MYviscAr, 'MYviscAr',
     &       Nr, myIter, myThid )
        CALL PLOT_FIELD_XYZRL ( MYdiffKr, 'MYdiffKr',
     &       Nr, myIter, myThid )
       ENDIF

       IF (MYwriteState .AND. snapshot_mdsio) THEN
#ifdef MULTIPLE_RECORD_MY82_STATE_FILES
C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
        CALL WRITE_REC_XYZ_RL('MYviscAr',MYviscAr,my_drctrec,
     &       myIter,myThid)
        CALL WRITE_REC_XYZ_RL('MYdiffKr',MYdiffKr,my_drctrec,
     &       myIter,myThid)
        CALL WRITE_REC_XY_RL('MYhbl',MYhbl,my_drctrec,
     &       myIter,myThid)
#else /* MULTIPLE_RECORD_MY82_STATE_FILES */
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
#endif /* MULTIPLE_RECORD_MY82_STATE_FILES */
       ENDIF

#ifdef ALLOW_MNC
       IF (MYwriteState .AND. useMNC .AND. snapshot_mnc ) THEN
        CALL MNC_CW_SET_UDIM('my_state', -1, myThid)
        CALL MNC_CW_RL_W_S('D','my_state',0,0,'T',myCurrentTime,myThid)
        CALL MNC_CW_SET_UDIM('my_state', 0, myThid)
        CALL MNC_CW_I_W_S('I','my_state',0,0,'iter',myIter,myThid)
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYviscAr', MYviscAr, myThid)
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYdiffKr', MYdiffKr, myThid)
        print *, 'ml-hbl 1'
        CALL MNC_CW_RL_W(pf,'my_state',0,0,
     &       'MYhbl', MYhbl, myThid)
        print *, 'ml-hbl 2'
       ENDIF
#endif /*  ALLOW_MNC  */

C--   Increment record counter
       my_drctrec = my_drctrec + 1

      ENDIF

C----------------------------------------------------------------
C     Do MY82 time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(MYviscArtave, Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(MYdiffKrtave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(MYhbltave,    1, bi,bj,myThid)
          DO k=1,Nr
          my_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C     Time Average MY82 fields
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE(MYtaveFreq,myCurrentTime,deltaTClock)
     &   ) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYviscArtave,MYviscAr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYdiffKrtave,MYdiffKr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYhbltave,   MYhbl,    1, DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
        DO k=1,Nr
         my_TimeAve(k,bi,bj)=my_TimeAve(k,bi,bj)+DDTT
        ENDDO
       ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(MYtaveFreq,myCurrentTime,deltaTClock)
     &     ) THEN
         
C     Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(MYviscArtave,my_timeave,
     &        Nr, bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(MYdiffKrtave,my_timeave,
     &        Nr, bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(MYhbltave   ,my_timeave,
     &        1,  bi,bj,myThid)
        ENDDO
       ENDDO
       
      IF (MYwriteState .AND. timeave_mdsio) THEN
#ifdef MULTIPLE_RECORD_MY82_STATE_FILES
C     Write each snap-shot as a new record in one file per variable
C     - creates relatively few files but these files can become huge
       CALL WRITE_REC_XYZ_RL('MYviscAr-T',MYviscArTave,
     &      my_drctrecTave,myIter,myThid)
       CALL WRITE_REC_XYZ_RL('MYdiffKr-T',MYdiffKrTave,
     &      my_drctrecTave,myIter,myThid)
       CALL WRITE_REC_XY_RL('MYhbl-T',    MYhblTave,
     &      my_drctrecTave,myIter,myThid)
C--   Increment record counter
       my_drctrecTave = my_drctrecTave + 1
         
#else /* MULTIPLE_RECORD_MY82_STATE_FILES */

C     Write each snap-shot as a new file 
C     - creates many files but for large configurations is easier to
C     transfer analyse a particular snap-shots
       WRITE(suff,'(I10.10)') myIter
       CALL WRITE_FLD_XYZ_RL('MYviscAr-T.',suff,MYviscArTave,
     &      myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('MYdiffKr-T.',suff,MYdiffKrTave,
     &      myIter,myThid)
       CALL WRITE_FLD_XY_RL('MYhbl-T.',    suff,MYhblTave,
     &      myIter,myThid)
#endif /* MULTIPLE_RECORD_MY82_STATE_FILES */          
      ENDIF
       
#ifdef ALLOW_MNC
      IF (MYwriteState .AND. useMNC .AND. timeave_mnc) THEN
       CALL MNC_CW_SET_UDIM('my_timeave', -1, myThid)
       CALL MNC_CW_RL_W_S('D','my_timeave',0,0,'T',myCurrentTime,myThid)
       CALL MNC_CW_SET_UDIM('my_timeave', 0, myThid)
       CALL MNC_CW_I_W_S('I','my_timeave',0,0,'iter',myIter,myThid)
       CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &      'MYviscAr', MYviscArTave, myThid)
       CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &      'MYdiffKr', MYdiffKrTave, myThid)
       CALL MNC_CW_RL_W(pf,'my_timeave',0,0,
     &      'MYhbl', MYhblTave, myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

C     Reset averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(MYviscArtave,Nr,bi,bj,myThid)
        CALL TIMEAVE_RESET(MYdiffKrtave,Nr,bi,bj,myThid)
        CALL TIMEAVE_RESET(MYhbltave,    1,bi,bj,myThid)
        DO k=1,Nr
         my_TimeAve(k,bi,bj)=0.
        ENDDO
       ENDDO
      ENDDO
         
C     Time Average MY fields
      DDTT=0.5*deltaTclock
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYviscArtave,MYviscAr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYdiffKrtave,MYdiffKr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       MYhbltave,    MYhbl,    1, DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
        DO k=1,Nr
         my_TimeAve(k,bi,bj)=my_TimeAve(k,bi,bj)+DDTT
        ENDDO
       ENDDO
      ENDDO
      ENDIF

#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(MYviscAr,'MYVISCAR',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(MYdiffKr,'MYDIFFKR',0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(MYhbl   ,'MYHBL   ',0,1 ,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      
#endif /* ALLOW_MY82 */
      
      RETURN
      END
