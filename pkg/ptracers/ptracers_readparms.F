C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_readparms.F,v 1.4 2003/11/13 06:35:15 dimitri Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize PTRACERS parameters, read in data.ptracers

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer              :: loop indices
C  iUnit		:: unit number for I/O
C  msgBuf		:: message buffer
      INTEGER iTracer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      PTRACERSisON=.TRUE.

C Set defaults values for parameters in PTRACERS.h
      PTRACERS_taveFreq=taveFreq
      PTRACERS_numInUse=-1
      DO iTracer=1,PTRACERS_num
       PTRACERS_advScheme(iTracer)=saltAdvScheme
       PTRACERS_diffKh(iTracer)=diffKhS
       PTRACERS_diffK4(iTracer)=diffK4S
       PTRACERS_diffKr(iTracer)=diffKrS
       PTRACERS_useGMRedi(iTracer)=useGMRedi
       PTRACERS_useKPP(iTracer)=useKPP
       PTRACERS_initialFile(iTracer)=' '
      ENDDO
      PTRACERS_useRecords=.FALSE.

C Open and read the data.ptracers file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' PTRACERS_READPARMS: opening data.ptracers'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.ptracers', 'PTRACERS_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=PTRACERS_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' PTRACERS_READPARMS: finished reading data.ptracers'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

C Now set-up any remaining parameters that result from the input parameters

C If PTRACERS_numInUse was not set in data.ptracers then we can assume
C that all PTRACERS fields will be in use
      IF (PTRACERS_numInUse.LT.0) THEN
       PTRACERS_numInUse=PTRACERS_num
      ENDIF
C Check we are not trying to use more tracers than allowed
      IF (PTRACERS_numInUse.GT.PTRACERS_num) THEN
       WRITE(msgBuf,'(A,I2,A,I2,A)')
     & ' PTRACERS_READPARMS: You requested ',PTRACERS_numInUse,
     & ' tracers at run time when only ',PTRACERS_num,
     & ' were specified at compile time. Naughty! '
       CALL PRINT_ERROR(msgBuf, 1)
       STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
      ENDIF
C Check that enough parameters were specified
      DO iTracer=1,PTRACERS_numInUse
       IF (PTRACERS_advScheme(iTracer).EQ.0) THEN
        WRITE(msgBuf,'(A,I2)')
     &  ' PTRACERS_READPARMS: No advect. scheme specified for tracer #',
     &  iTracer
        CALL PRINT_ERROR(msgBuf, 1)
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
       ENDIF
      ENDDO


#endif /* ALLOW_PTRACERS */

      RETURN
      END
