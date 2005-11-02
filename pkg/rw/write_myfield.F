C $Header: /u/gcmpack/MITgcm/pkg/rw/Attic/write_myfield.F,v 1.1 2005/11/02 15:37:54 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

#undef  MULTIPLE_RECORD_STATE_FILES

CBOP
C     !ROUTINE: WRITE_MYFIELD 
C     !INTERFACE:
      SUBROUTINE WRITE_MYFIELD ( 
     & myField, myFile, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_MYFIELD                                    
C     | o Template routine for probing one array at dumpFreq rate
C     |   place call to this routine into code          
C     *==========================================================*
C     | Write model state files for post-processing. This file   
C     | includes code for diagnosing W and RHO for output.       
C     | File write_state.F: Routines to handle mid-level I/O
C     | interface.
C     | Adapted from S/R WRITE_STATE
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      _RL     myField(sNx,sNy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myThid
      INTEGER myIter
      CHARACTER*(*) myFile 

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(80) localfile
      INTEGER iRec, il

c     == external ==

      integer  ilnblnk
      external ilnblnk

CEOP

      IF ( 
     &     DIFFERENT_MULTIPLE(dumpFreq,myTime,deltaTClock)
     &     .OR. myTime.EQ.endTime
     &     .OR. myTime.EQ.startTime
     &     ) THEN
         IF ( dumpFreq .EQ. 0.0 ) THEN
            iRec = 1
         ELSE
            iRec = NINT ( (myTime-startTime) / dumpFreq + 1. )
         ENDIF
         
C--   Going to really do some IO. Make everyone except master thread wait.
         _BARRIER
         _BEGIN_MASTER( myThid )

C--     Write model fields
        il=ilnblnk( myFile )
        write(localfile(1:80),'(80a)') ' '
        write(localfile(1:80),'(2a)') myFile(1:il),'.'

#ifdef MULTIPLE_RECORD_STATE_FILES

C       Write each snap-shot as a new record in one file per variable
C       - creates relatively few files but these files can become huge
        CALL WRITE_REC_XYZ_RL( localfile,  myField,iRec,myIter,myThid)

#else /* MULTIPLE_RECORD_STATE_FILES */

C       Write each snap-shot as a new file (original and default method)
C       - creates many files but for large configurations is easier to
C         transfer analyse a particular snap-shots
        WRITE(suff,'(I10.10)') myIter
        STOP 'WRITE_FLD_XYZ_RL CALL INCORRECT - CONTACT SUPPORT'
C       CALL WRITE_FLD_XYZ_RL( localfile,suff,myField,myIter,myThid)

#endif /* MULTIPLE_RECORD_STATE_FILES */

         _END_MASTER( myThid )
         _BARRIER

      ENDIF

      RETURN
      END
