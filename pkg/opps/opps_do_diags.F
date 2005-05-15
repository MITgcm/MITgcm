C $Header: /u/gcmpack/MITgcm/pkg/opps/Attic/opps_do_diags.F,v 1.3 2005/05/15 03:04:57 jmc Exp $
C $Name:  $

#include "OPPS_OPTIONS.h"

#undef  MULTIPLE_RECORD_OPPS_STATE_FILES
CBOP
C     !ROUTINE: OPPS_DO_DIAGS
C     !INTERFACE:
      SUBROUTINE OPPS_DO_DIAGS( myCurrentTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OPPS_DO_DIAGS                                  
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
#include "OPPS_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myCurrentTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myCurrentTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OPPS

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, K
      _RL DDTT
CEOP

CMLC----------------------------------------------------------------
CMLC     Dump snapshot of OPPS variables.
CMLC----------------------------------------------------------------
CML
CML      IF (
CML     &     DIFFERENT_MULTIPLE(OPPSdumpFreq,
CML     &     myCurrentTime,deltaTClock)
CML     &     ) THEN
CML         
CML       if (OPPSwriteState) then
CML#ifdef MULTIPLE_RECORD_OPPS_STATE_FILES
CMLC       Write each snap-shot as a new record in one file per variable
CMLC       - creates relatively few files but these files can become huge
CML        CALL WRITE_REC_XYZ_RL('OPPSconv',OPPSconvectCount,opps_drctrec,
CML     &       myIter,myThid)
CML#else /* MULTIPLE_RECORD_OPPS_STATE_FILES */
CMLC       Write each snap-shot as a new file 
CMLC       - creates many files but for large configurations is easier to
CMLC         transfer analyse a particular snap-shots
CML        WRITE(suff,'(I10.10)') myIter
CML        CALL WRITE_FLD_XYZ_RL('OPPSconv.',suff,OPPSconvectCount,
CML     &       myIter,myThid)
CML#endif /* MULTIPLE_RECORD_OPPS_STATE_FILES */
CML       endif
CML
CMLC--   Increment record counter
CML       opps_drctrec = opps_drctrec + 1
CML
CML      ENDIF

C----------------------------------------------------------------
C     Do OPPS time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

C     Initialize averages to zero
      IF ( myIter.EQ.nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(OPPSconvectCountTave,Nr,bi,bj,myThid)
         DO k=1,Nr
          opps_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C     Time Average OPPS fields
      IF ( myIter .EQ. nIter0 .OR.
     &     DIFFERENT_MULTIPLE(OPPStaveFreq,myCurrentTime,deltaTClock)
     &   ) THEN
       DDTT=0.5*deltaTclock
      ELSE
       DDTT=deltaTclock
      ENDIF
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C     Keep record of how much time has been integrated over
        DO k=1,Nr
         opps_TimeAve(k,bi,bj)=opps_TimeAve(k,bi,bj)+DDTT
        ENDDO
       ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND. 
     &     DIFFERENT_MULTIPLE(OPPStaveFreq,
     &     myCurrentTime,deltaTClock)
     &     ) THEN
         
C     Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(OPPSconvectCountTave,opps_timeave,
     &        Nr, bi,bj,myThid)
        ENDDO
       ENDDO
       
#ifdef MULTIPLE_RECORD_OPPS_STATE_FILES
C     Write each snap-shot as a new record in one file per variable
C     - creates relatively few files but these files can become huge
       CALL WRITE_REC_XYZ_RL('OPPSconvtave',OPPSconvectCountTave,
     &      opps_drctrecTave,myIter,myThid)
C--   Increment record counter
       opps_drctrecTave = opps_drctrecTave + 1
         
#else /* MULTIPLE_RECORD_OPPS_STATE_FILES */

C     Write each snap-shot as a new file 
C     - creates many files but for large configurations is easier to
C     transfer analyse a particular snap-shots
       WRITE(suff,'(I10.10)') myIter
       CALL WRITE_FLD_XYZ_RL('OPPSconvtave.',suff,OPPSconvectCountTave,
     &      myIter,myThid)
#endif /* MULTIPLE_RECORD_OPPS_STATE_FILES */          
         
C     Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(OPPSconvectCountTave, Nr,bi,bj,myThid)
         DO k=1,Nr
          opps_TimeAve(k,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
         
C     Time Average OPPS fields
       DDTT=0.5*deltaTclock
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_CUMULATE(
     &        OPPSconvectCountTave, OPPSconvectCount, 
     &        Nr,DDTT,bi,bj,myThid)
C     Keep record of how much time has been integrated over
         DO k=1,Nr
          opps_TimeAve(k,bi,bj)=opps_TimeAve(k,bi,bj)+DDTT
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_TIMEAVE */
      
#endif /* ALLOW_OPPS */
      
      RETURN
      END
