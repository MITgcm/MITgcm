#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_INI_IO
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INI_IO( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGNOSTICS_INI_IO
C     | o create directory for mds output if needed
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER iL, pIL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      _BEGIN_MASTER( myThid )

      IF ( diag_mdsio .AND. (diagMdsDir.NE.' ') .AND.
     &     diagMdsDirCreate .AND. (mdsioLocalDir.EQ.' ') ) THEN
#ifdef HAVE_SYSTEM
C      create directory
        iL = ILNBLNK( diagMdsDir )
        WRITE(namBuf,'(3A)') ' mkdir -p ', diagMdsDir(1:iL), ' '
        pIL = 1 + ILNBLNK( namBuf )
        WRITE(standardMessageUnit,'(4A)')
     &       ' ==> SYSTEM CALL (from DIAGNOSTICS_INI_IO): ',
     &       '>', namBuf(1:pIL), '<'
        CALL SYSTEM( namBuf(1:pIL) )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
#else
        WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_INI_IO: ',
     &       'cannot call mkdir -> please create diagMdsDir manually'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_INI_IO: ',
     &       'and set diagMdsDirCreate=.FALSE. in data.diagnostics'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_INI_IO'
#endif
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
