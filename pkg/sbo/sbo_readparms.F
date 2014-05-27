C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_readparms.F,v 1.7 2014/05/27 23:41:27 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"

CBOP
C !ROUTINE: SBO_READPARMS

C !INTERFACE:
      SUBROUTINE SBO_READPARMS( myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SBO_READPARMS
C     | o Routine to read in file data.sbo
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SBO.h"

C !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_SBO
C !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
C     iUnit    :: Work variable for IO unit number
C     nRetired :: Counter used to trap namelists containing "retired" parameters

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER nRetired, errCount
      _RL     sbo_taveFreq
CEOP

C--   SBO parameters
      NAMELIST /SBO_PARM01/ sbo_taveFreq, sbo_monFreq

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useSBO ) THEN
C-    pkg SBO is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.sbo is found
         CALL PACKAGES_UNUSED_MSG( 'useSBO', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(A)') ' SBO_READPARMS: opening data.sbo'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.sbo', 'SBO_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default SBO parameters
      sbo_monFreq  = monitorFreq

C--   Initialise retired parameters to unlikely value
      nRetired = 0
      sbo_taveFreq = UNSET_RL

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.sbo".
      READ(UNIT=iUnit,NML=SBO_PARM01)

C     Close the open data file
      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' SBO_READPARMS: finished reading data.sbo'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C     Check for retired parameters still being used
      IF ( sbo_taveFreq .NE. UNSET_RL ) THEN
        nRetired = nRetired + 1
        WRITE(msgBuf,'(A,A)') 'S/R SBO_READPARMS: "sbo_taveFreq" ',
     &  'is no longer allowed in file "data.sbo"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C--   Check whether any retired parameters were found.
      IF ( nRetired .GT. 0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R SBO_READPARMS: ',
     &   'Error reading parameter file "data.sbo"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'some out of date parameters were found in the namelist'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Stop if any error was found (including retired params):
      IF ( errCount .GE. 1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &   'S/R SBO_READPARMS: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R SBO_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_SBO */

      RETURN
      END
