C $Header: /u/gcmpack/MITgcm/model/src/write_state.F,v 1.10 2001/03/08 20:07:43 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

C--  File read_write.F: Routines to handle mid-level I/O interface.
C--   o WRITE_STATE - Write out model state.

C The following CPP flag (MULTIPLE_RECORD_STATE_FILES) is
C #define/#undefed here since it is specific to this routine and
C very user-preference specific.
C
C If #undefed (default) the state files are written as in all versions
C prior to checkpoint32, where a file is created per variable, per time
C and per tile. This *has* to be the default because most users use this
C mode and all utilities and scripts (diagnostic) assume this form.
C It is also robust, as explained below.
C 
C If #defined, subsequent snap-shots are written as records in the
C same file (no iteration number in filenames).
C Advantages: - fewer files
C             - for small problems, is easy to copy the output around
C Disadvantages:
C             - breaks a lot of diagnostic scripts
C             - for large or long problems this creates huge files
C             - is an unexpected, unsolicited change in behaviour which came
C               as a surprise (in c32) and inconvenience to several users
C             - can not accomodate changing the frequency of output
C               after a pickup (this is trivial in previous method
C               but needs new code and parameters in this new method)
C Known Bugs:
C             - if the length of integration is not exactly an integer
C               times the output frequency then the last record written
C               (at end of integration) overwrites a previously written
C               record corresponding to an earier time. *BE WARNED*

#undef  MULTIPLE_RECORD_STATE_FILES
  


CStartofinterface
      SUBROUTINE WRITE_STATE ( forceOutput, myCurrentTime, 
     &    myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE WRITE_STATE                                   |
C     | o Controlling routine for IO to dump model state.        |
C     |==========================================================|
C     | Write model state files for post-processing. This file   |
C     | includes code for diagnosing W and RHO for output.       |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "CG3D.h"
#include "GW.h"
#endif

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      LOGICAL forceOutput
      _RL     myCurrentTime
      INTEGER myThid
      INTEGER myIter
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER iRec

      IF ( 
     &     ( DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,
     &     myCurrentTime-deltaTClock) .AND. myCurrentTime.NE.endTime )
     &     .OR. forceOutput
     &     ) THEN
         iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
         
C--   Going to really do some IO. Make everyone except master thread wait.
         _BARRIER
         _BEGIN_MASTER( myThid )

C--     Write model fields
#ifdef MULTIPLE_RECORD_STATE_FILES
C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
        CALL WRITE_REC_XYZ_RL( 'U',  uVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'V',  vVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'T', theta,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'S',  salt,iRec,myIter,myThid)
        CALL WRITE_REC_XY_RL('Eta',etaN,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'W',wVel,iRec,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_REC_XYZ_RL( 'PNH',cg3d_x,iRec,myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#else
C       Write each snap-shot as a new file (original and default method)
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'U.',suff,uVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'V.',suff,vVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'T.',suff,theta,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'S.',suff,salt,myIter,myThid)
        CALL WRITE_FLD_XY_RL('Eta.',suff,etaN,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'W.',suff,wVel,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_FLD_XYZ_RL( 'PNH.',suff,cg3d_x,myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */
#endif /* MULTIPLE_RECORD_STATE_FILES */

         _END_MASTER( myThid )
         _BARRIER

      ENDIF

      RETURN
      END
