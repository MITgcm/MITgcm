C $Header: /u/gcmpack/MITgcm/model/src/write_state.F,v 1.1 1999/05/05 18:32:35 adcroft Exp $
#include "CPP_OPTIONS.h"

C--  File read_write.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o WRITE_STATE - Write out model state.

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
#include "CG2D.h"
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
      REAL    myCurrentTime
      INTEGER myThid
      INTEGER myIter
CEndofinterface

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(80) fName

      IF ( 
     &  ( DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,
     &    myCurrentTime-deltaTClock) .AND. myCurrentTime.NE.startTime )
     &  .OR. forceOutput
     & ) THEN

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

        WRITE(suff,'(I10.10)') myIter

C--     Write model fields
        CALL WRITE_FLD_XYZ_RL( 'U.',suff,uVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'V.',suff,vVel,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'T.',suff,theta,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL( 'S.',suff,salt,myIter,myThid)
        CALL WRITE_FLD_XY_RL( 'PS.',suff,cg2d_x,myIter,myThid)

#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_FLD_XYZ_RL( 'PNH.',suff,cg3d_x,myIter,myThid)
        ENDIF
        CALL WRITE_FLD_XYZ_RL( 'W.',suff,wVel,myIter,myThid)
#endif

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      RETURN
      END
