C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_readparms.F,v 1.1 2004/09/02 09:11:54 mlosch Exp $
C $Name:  $

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
C  iUnit		:: unit number for I/O
C  msgBuf		:: message buffer
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
     &     PPwriteState,
     &     PPtaveFreq

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      PP81isON=.TRUE.

C Set defaults values for parameters in PP81.h
      PPnRi        = 2
      PPviscMin    = 0. _d 0
      PPdiffMin    = 0. _d 0
      PPviscMax    = 1. _d 0
      PPnu0        = 1. _d -02
      PPalpha      = 5. _d 0
      RiLimit      = UNSET_RL
      PPdumpFreq   = dumpFreq
      PPtaveFreq   = taveFreq
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
      CLOSE(iUnit)
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
       RiLimit=(((PPnu0+viscAr)/PPviscMax)**(1./REAL(PPnRi))-1.)
     &      /PPalpha
      ENDIF
#endif /* ALLOW_PP81 */

      RETURN
      END
