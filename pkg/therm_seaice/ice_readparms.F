
#include "CPP_OPTIONS.h"

      SUBROUTINE ICE_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE ICE_READPARMS                                 |
C     | o Routine to initialize ICE variables and constants.     |
C     |==========================================================|
C     | Initialize ICE    parameters, read in data.rbc          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
cswdice
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#endif
cswdice

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_THERM_SEAICE

C--   ICE parameter
      NAMELIST /ICE_PARM01/
     &         startIceModel,relaxlat, hicemin


C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' ICE_READPARMS: opening data.ice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.ice', 'ICE_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values 
      StartIceModel=0
      relaxlat=90.
      hicemin=0.d0

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=ICE_PARM01)

      WRITE(msgBuf,'(A)') ' ICE_READPARMS: finished reading data.ice'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER


#endif /* ALLOW_THERM_SEAICE */

      return
      end
