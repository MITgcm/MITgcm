C $Header: /u/gcmpack/MITgcm/pkg/ggl90/Attic/ggl90_do_diags.F,v 1.3 2005/05/15 03:04:56 jmc Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

#undef  MULTIPLE_RECORD_GGL90_STATE_FILES
CBOP
C     !ROUTINE: GGL90_DO_DIAGS
C     !INTERFACE:
      SUBROUTINE GGL90_DO_DIAGS( myCurrentTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GGL90_DO_DIAGS                                  
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
#include "GGL90_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT
CEOP

C----------------------------------------------------------------
C     Dump snapshot of GGL90 variables.
C----------------------------------------------------------------

      IF (
     &     DIFFERENT_MULTIPLE(GGL90dumpFreq,myCurrentTime,deltaTClock)
     &     ) THEN
         
       IF (GGL90mixingMaps) THEN
        CALL PLOT_FIELD_XYZRL ( GGL90TKE   , 'GGL90TKE'   ,    
     &       Nr, myIter, myThid )
        CALL PLOT_FIELD_XYZRL ( GGL90viscAr, 'GGL90viscAr',
     &       Nr, myIter, myThid )
        CALL PLOT_FIELD_XYZRL ( GGL90diffKr, 'GGL90diffKr',
     &       Nr, myIter, myThid )
       ENDIF

       if (GGL90writeState) then
#ifdef MULTIPLE_RECORD_GGL90_STATE_FILES
C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
        CALL WRITE_REC_XYZ_RL('GGL90viscAr',GGL90viscAr,ggl90_drctrec,
     &       myIter,myThid)
        CALL WRITE_REC_XYZ_RL('GGL90diffKr',GGL90diffKr,ggl90_drctrec,
     &       myIter,myThid)
        CALL WRITE_REC_XYZ_RL('GGL90TKE'   ,GGL90TKE   ,ggl90_drctrec,
     &       myIter,myThid)
#else /* MULTIPLE_RECORD_GGL90_STATE_FILES */
C       Write each snap-shot as a new file 
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL('GGL90viscAr.',suff,GGL90viscAr,
     &       myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('GGL90diffKr.',suff,GGL90diffKr,
     &       myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('GGL90TKE.'   ,suff,GGL90TKE,
     &       myIter,myThid)
#endif /* MULTIPLE_RECORD_GGL90_STATE_FILES */
       endif

C--   Increment record counter
       ggl90_drctrec = ggl90_drctrec + 1

      ENDIF

C----------------------------------------------------------------
C     Do GGL90 time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(GGL90viscArtave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GGL90diffKrtave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GGL90TKEtave,   Nr,bi,bj,myThid)
          DO k=1,Nr
          ggl90_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C     Time Average GGL90 fields
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE(GGL90taveFreq,myCurrentTime,deltaTClock)
     &   ) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_CUMULATE(
     &       GGL90viscArtave,GGL90viscAr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       GGL90diffKrtave,GGL90diffKr,Nr,DDTT,bi,bj,myThid)
        CALL TIMEAVE_CUMULATE(
     &       GGL90TKEtave,   GGL90TKE,   Nr,DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
        DO k=1,Nr
         ggl90_TimeAve(k,bi,bj)=ggl90_TimeAve(k,bi,bj)+DDTT
        ENDDO
       ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(GGL90taveFreq,myCurrentTime,deltaTClock)
     &     ) THEN
         
C     Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(GGL90viscArtave,ggl90_timeave,
     &        Nr, bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(GGL90diffKrtave,ggl90_timeave,
     &        Nr, bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(GGL90TKEtave   ,ggl90_timeave,
     &        Nr, bi,bj,myThid)
        ENDDO
       ENDDO
       
#ifdef MULTIPLE_RECORD_GGL90_STATE_FILES
C     Write each snap-shot as a new record in one file per variable
C     - creates relatively few files but these files can become huge
       CALL WRITE_REC_XYZ_RL('GGL90viscAr-T',GGL90viscArTave,
     &      ggl90_drctrecTave,myIter,myThid)
       CALL WRITE_REC_XYZ_RL('GGL90diffKr-T',GGL90diffKrTave,
     &      ggl90_drctrecTave,myIter,myThid)
       CALL WRITE_REC_XYZ_RL('GGL90TKE-T',   GGL90TKETave,
     &      ggl90_drctrecTave,myIter,myThid)
C--   Increment record counter
       ggl90_drctrecTave = ggl90_drctrecTave + 1
         
#else /* MULTIPLE_RECORD_GGL90_STATE_FILES */

C     Write each snap-shot as a new file 
C     - creates many files but for large configurations is easier to
C     transfer analyse a particular snap-shots
       WRITE(suff,'(I10.10)') myIter
       CALL WRITE_FLD_XYZ_RL('GGL90viscAr-T.',suff,GGL90viscArTave,
     &      myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GGL90diffKr-T.',suff,GGL90diffKrTave,
     &      myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('GGL90TKE-T.',   suff,GGL90TKETave,
     &      myIter,myThid)
#endif /* MULTIPLE_RECORD_GGL90_STATE_FILES */          
         
C     Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(GGL90viscArtave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GGL90diffKrtave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(GGL90TKEtave,   Nr,bi,bj,myThid)
         DO k=1,Nr
          ggl90_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
         
C     Time Average GGL90 fields
       DDTT=0.5*deltaTclock
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_CUMULATE(
     &        GGL90viscArtave,GGL90viscAr,Nr,DDTT,bi,bj,myThid)
         CALL TIMEAVE_CUMULATE(
     &        GGL90diffKrtave,GGL90diffKr,Nr,DDTT,bi,bj,myThid)
         CALL TIMEAVE_CUMULATE(
     &        GGL90TKEtave,   GGL90TKE,   Nr,DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
         DO k=1,Nr
          ggl90_TimeAve(k,bi,bj)=ggl90_TimeAve(k,bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif
      
#endif
      
      RETURN
      END
