C $Header: /u/gcmpack/MITgcm/model/src/Attic/open_copy_data_file.F,v 1.3 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: OPEN_COPY_DATA_FILE
C     !INTERFACE:
      SUBROUTINE OPEN_COPY_DATA_FILE(
     I                                data_file, caller_sub,
     O                                iUnit,
     I                                myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OPEN_COPY_DATA_FILE                            
C     | o Routine to open and copy a data.* file to STDOUT        
C     |   and return the open unit in iUnit                       
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Number of this instance of INI_PARMS
      CHARACTER*(*) data_file
      CHARACTER*(*) caller_sub
      INTEGER iUnit
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf    - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  errIO,IL
      LOGICAL  exst
CEOP

C
      _BEGIN_MASTER(myThid)

C--   Open the parameter file
      INQUIRE( FILE=data_file, EXIST=exst )
      IF (exst) THEN
       WRITE(msgbuf,'(A,A)')
     &   ' OPEN_COPY_DATA_FILE: opening file ',data_file
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , mythid)
      ELSE
       WRITE(msgBuf,'(A,A,A)')
     &  'File ',data_file,' does not exist!'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') 'S/R CALLED BY ',caller_sub
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R OPEN_COPY_DATA_FILE'
      ENDIF

      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
      OPEN(UNIT=modelDataUnit,FILE=data_file,STATUS='OLD',
     &     IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'Unable to open data file: ',data_file
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A,A)') 'S/R CALLED BY ',caller_sub
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R OPEN_COPY_DATA_FILE'
      ENDIF     

      DO WHILE ( .TRUE. )
       READ(modelDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter )
     &     WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
        WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
      ENDDO
 1001 CONTINUE
      CLOSE(modelDataUnit)

C--   Report contents of model parameter file
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A,A,A)') '// Parameter file "',data_file,'"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      iUnit = scrUnit2
      REWIND(iUnit)
      DO WHILE ( .TRUE. )
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      ENDDO
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)


C--   Return open unit to caller
      iUnit = scrUnit1
      REWIND(iUnit)

      _END_MASTER(myThid)


      RETURN
      END

