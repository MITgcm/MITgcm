C $Header:

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE SBO_READPARMS                                 |
C     | o Routine to read in file data.sbo                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SBO.h"

C     === Routine arguments ===
C     myThid - Number of this instance of SBO_READPARMS
      INTEGER myThid

#ifdef ALLOW_SBO

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   SBO parameters
      NAMELIST /SBO_PARM01/ sbo_taveFreq

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' SBO_INIT: opening data.sbo'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.sbo', 'SBO_INIT',
     O                          iUnit,
     I                          myThid )

C--   set default SBO parameters
      sbo_taveFreq  = 0.

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.sbo".
      READ(UNIT=iUnit,NML=SBO_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading SBO package'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.sbo"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist SBO_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R SBO_INIT'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' SBO_INIT: finished reading data.sbo'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif ALLOW_SBO

      return
      end
