#include "MY82_OPTIONS.h"

CBOP
C !ROUTINE: MY82_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE MY82_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize MY82 parameters, read in data.pp

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MY82.h"

C !INPUT PARAMETERS: ===================================================
C  myThid       :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_MY82

C !LOCAL VARIABLES: ====================================================
C  iUnit        :: unit number for I/O
C  msgBuf       :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /MY_PARM01/
     &     MYviscMax,
     &     MYdiffMax,
     &     MYhblScale,
     &     MYdumpFreq,
     &     MYMixingMaps,
     &     MYwriteState

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useMY82 ) THEN
C-    pkg MY82 is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         MYisON = .FALSE.
C     print a (weak) warning if data.my82 is found
         CALL PACKAGES_UNUSED_MSG( 'useMY82', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      MYisON = .TRUE.

C Set defaults values for parameters in MY82.h
      MYviscMax    = 1. _d 0
      MYdiffMax    = 1. _d 0
      MYhblScale   = 0.1 _d 0
      RiMax        = 0.1950 _d 0
      MYdumpFreq   = dumpFreq
      MYMixingMaps = .FALSE.
      MYwriteState = .FALSE.

C Open and read the data.pp file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' MY82_READPARMS: opening data.my82'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.my82', 'MY82_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=MY_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' MY82_READPARMS: finished reading data.my82'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

C Now set-up any remaining parameters that result from the input parameters
      IF ( MYviscMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'MYviscMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MY82_READPARMS'
      ENDIF
      IF ( MYdiffMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'MYdiffMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MY82_READPARMS'
      ENDIF
#endif /* ALLOW_MY82 */

      RETURN
      END
