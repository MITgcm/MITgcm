#include "STIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: STIC_READPARMS

C !INTERFACE:
      SUBROUTINE STIC_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize STIC variables and constants.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STIC.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /STIC_PARM01/
     &     STIClengthFile, STICdepthFile

      IF ( .NOT.useSTIC ) THEN
C-    pkg STIC is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.stic is found
         CALL PACKAGES_UNUSED_MSG( 'useSTIC', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for STIC
C-    file names for initial conditions:
C     the default topography should be the same as for pkg/shelfice, but
C     we allow different files and issue a warning. For the following
C     line to work, S/R SHELFICE_READPARMS needs to be called first.
      STICdepthFile  = SHELFICEtopoFile
      STIClengthFile = ' '

      WRITE(msgBuf,'(A)') 'STIC_READPARMS: opening data.stic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.stic', 'STIC_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=STIC_PARM01)
      WRITE(msgBuf,'(A)')
     &    'STIC_READPARMS: finished reading data.stic'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END
