#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: NF90IO_READPARMS

C !INTERFACE:
      SUBROUTINE NF90IO_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize NF90IO variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "NF90IO.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_NF90IO

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /NF90IO_PARM01/
     &       nf90io_fileFormat

      IF ( .NOT.useNF90IO ) THEN
C-    pkg NF90IO is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.nf90io is found
         CALL PACKAGES_UNUSED_MSG( 'useNF90IO', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for NF90IO
      nf90io_fileFormat = 4

      WRITE(msgBuf,'(A)') 'NF90IO_READPARMS: opening data.nf90io'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.nf90io', 'NF90IO_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=NF90IO_PARM01)
      WRITE(msgBuf,'(A)')
     &    'NF90IO_READPARMS: finished reading data.nf90io'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_NF90IO */

      RETURN
      END
