C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_readparms.F,v 1.6 2004/09/03 20:10:47 edhill Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_READPARMS
      
C     !INTERFACE:
      SUBROUTINE PTRACERS_READPARMS( myThid )
      
C     !DESCRIPTION:
C     Initialize PTRACERS parameters, read in data.ptracers

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "PARAMS.h"
      
C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer    :: loop indices
C     iUnit      :: unit number for I/O
C     msgBuf     :: message buffer
      INTEGER iTracer
      INTEGER iUnit
      INTEGER ic
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     PTRACERS_taveFreq :: Frequency with which time-averaged PTRACERS
C                          are written to post-processing files.
      NAMELIST /PTRACERS_PARM01/
     &     PTRACERS_taveFreq,
     &     PTRACERS_advScheme,
     &     PTRACERS_diffKh,
     &     PTRACERS_diffK4,
     &     PTRACERS_diffKr,
     &     PTRACERS_useGMRedi,
     &     PTRACERS_useKPP,
     &     PTRACERS_numInUse,
     &     PTRACERS_initialFile,
     &     PTRACERS_useRecords,
     &     PTRACERS_names,
     &     PTRACERS_long_names,
     &     PTRACERS_units,
     &     PTRACERS_mnc_read, 
     &     PTRACERS_mnc_write,
     &     PTRACERS_iotypes

C     This routine has been called by the main model so we set our
C     internal flag to indicate we are in business
      PTRACERSisON=.TRUE.

C     Set defaults values for parameters in PTRACERS.h
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
        DO ic = 1,MAX_LEN_FNAM
          PTRACERS_names(iTracer)(ic:ic) = ' '
          PTRACERS_long_names(iTracer)(ic:ic) = ' '
          PTRACERS_units(iTracer)(ic:ic) = ' '
        ENDDO
      ENDDO
      PTRACERS_useRecords=.FALSE.
      PTRACERS_iotypes = -1
      PTRACERS_mnc_read = .true.
      PTRACERS_mnc_write = .true.

C     Open and read the data.ptracers file
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

C     Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be loaded
      _BARRIER

C     Now set-up any remaining parameters that result from the input
C     parameters

C     If PTRACERS_numInUse was not set in data.ptracers then we can
C     assume that all PTRACERS fields will be in use
      IF (PTRACERS_numInUse.LT.0) THEN
        PTRACERS_numInUse=PTRACERS_num
      ENDIF
C     Check we are not trying to use more tracers than allowed
      IF (PTRACERS_numInUse.GT.PTRACERS_num) THEN
        WRITE(msgBuf,'(A,I2,A,I2,A)')
     &       ' PTRACERS_READPARMS: You requested ',PTRACERS_numInUse,
     &       ' tracers at run time when only ',PTRACERS_num,
     &       ' were specified at compile time. Naughty! '
        CALL PRINT_ERROR(msgBuf, 1)
        STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
      ENDIF
C     Check that enough parameters were specified
      DO iTracer=1,PTRACERS_numInUse
        IF (PTRACERS_advScheme(iTracer).EQ.0) THEN
          WRITE(msgBuf,'(A,A,I2)')
     &         ' PTRACERS_READPARMS: ',
     &         'No advect. scheme specified for tracer #',
     &         iTracer
          CALL PRINT_ERROR(msgBuf, 1)
          STOP 'ABNORMAL END: S/R PTRACERS_READPARMS'
        ENDIF
      ENDDO

C     Set the default I/O Types
      IF (PTRACERS_iotypes .EQ. -1) THEN
        PTRACERS_iotypes = 1
      ENDIF

#ifdef ALLOW_MNC
C     Initialize the MNC variable types for PTRACERS 
      IF (useMNC) THEN
        CALL PTRACERS_MNC_INIT( myThid )
      ENDIF
#endif /*  ALLOW_MNC  */
      
#endif /* ALLOW_PTRACERS */

      RETURN
      END

