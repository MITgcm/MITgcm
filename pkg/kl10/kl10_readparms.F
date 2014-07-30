C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_readparms.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

CBOP
C !ROUTINE: KL10_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE KL10_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize KL10 parameters, read in data.kl10

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "KL10.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_KL10

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /KL10_PARM01/
     &     KLviscMax,
     &     KLdumpFreq,
     &     KLwriteState,
     &     KLtaveFreq

      IF ( .NOT.useKL10 ) THEN
C-    pkg KL10 is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         KL10isON = .FALSE.
C     print a (weak) warning if data.kl10 is found
         CALL PACKAGES_UNUSED_MSG( 'useKL10', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      KL10isON = .TRUE.

C Set defaults values for parameters in KL10.h
      KLviscMax    = 300. _d 0
      KLdumpFreq   = dumpFreq
      KLtaveFreq   = taveFreq
      KLwriteState = .FALSE.

C Open and read the data.kl10 file
      WRITE(msgBuf,'(A)') ' KL10_READPARMS: opening data.kl10'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.kl10', 'KL10_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=KL10_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' KL10_READPARMS: finished reading data.kl10'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

C Now set-up any remaining parameters that result from the input parameters
      IF ( KLviscMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'KLviscMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R KL10_READPARMS'
      ENDIF
#endif /* ALLOW_KL10 */

      RETURN
      END
