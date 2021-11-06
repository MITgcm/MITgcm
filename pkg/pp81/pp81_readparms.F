#include "PP81_OPTIONS.h"

CBOP
C !ROUTINE: PP81_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE PP81_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize PP81 parameters, read in data.pp81

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PP81.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PP81

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /PP81_PARM01/
     &     PPnRi,
     &     PPviscMin,
     &     PPdiffMin,
     &     PPviscMax,
     &     PPnu0,
     &     PPalpha,
     &     PPdumpFreq,
     &     PPMixingMaps,
     &     PPwriteState

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.usePP81 ) THEN
C-    pkg PP81 is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         PP81isON = .FALSE.
C     print a (weak) warning if data.pp81 is found
         CALL PACKAGES_UNUSED_MSG( 'usePP81', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      PP81isON = .TRUE.

C Set defaults values for parameters in PP81.h
      PPnRi        = 2
      PPviscMin    = 0. _d 0
      PPdiffMin    = 0. _d 0
      PPviscMax    = 1. _d 0
      PPnu0        = 1. _d -02
      PPalpha      = 5. _d 0
      RiLimit      = UNSET_RL
      PPdumpFreq   = dumpFreq
      PPMixingMaps = .FALSE.
      PPwriteState = .FALSE.

C Open and read the data.pp81 file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' PP81_READPARMS: opening data.pp81'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.pp81', 'PP81_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=PP81_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' PP81_READPARMS: finished reading data.pp81'
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
      IF ( PPviscMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'PPviscMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PP81_READPARMS'
      ENDIF
      IF ( PPalpha .EQ. 0. ) THEN
       WRITE(msgBuf,'(A)') 'PPalpha must not be zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PP81_READPARMS'
      ENDIF
      IF ( PPnRi .EQ. 0 ) THEN
       WRITE(msgBuf,'(A)') 'PPnRi must not be zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R PP81_READPARMS'
      ENDIF
      IF ( RiLimit .EQ. UNSET_RL ) THEN
       RiLimit = PPnRi
       RiLimit = (
     &             ((PPnu0+viscArNr(1))/PPviscMax)**(1. _d 0/RiLimit)
     &            -1. _d 0
     &           )/PPalpha
      ENDIF
#endif /* ALLOW_PP81 */

      RETURN
      END
