C $Header: /u/gcmpack/MITgcm/model/src/write_state.F,v 1.4 2000/11/13 16:32:58 heimbach Exp $
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
      _RL     myCurrentTime
      INTEGER myThid
      INTEGER myIter
CEndofinterface

C     == Local variables ==
      INTEGER iRec

      IF ( 
     &     ( DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,
     &     myCurrentTime-deltaTClock) .AND. myCurrentTime.NE.startTime )
     &     .OR. forceOutput
     &     ) THEN
         iRec = int ( (myCurrentTime-startTime) / dumpFreq +1.5 )
         
C--   Going to really do some IO. Make everyone except master thread wait.
         _BARRIER
         _BEGIN_MASTER( myThid )

C--     Write model fields
        CALL WRITE_REC_XYZ_RL( 'U',  uVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'V',  vVel,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'T', theta,iRec,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'S',  salt,iRec,myIter,myThid)
        CALL WRITE_REC_XY_RL ('PS',cg2d_x,iRec,myIter,myThid)

#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydroStatic) THEN
         CALL WRITE_REC_XYZ_RL( 'PNH',cg3d_x,iRec,myIter,myThid)
        ENDIF
        CALL WRITE_REC_XYZ_RL( 'W',wVel,iRec,myIter,myThid)
#endif

         _END_MASTER( myThid )
         _BARRIER

      ENDIF

      RETURN
      END
