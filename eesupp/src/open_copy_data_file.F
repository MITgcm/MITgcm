#include "CPP_EEOPTIONS.h"

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
#ifdef SINGLE_DISK_IO
# include "EESUPPORT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     data_file  :: parameter file to open and copy
C     caller_sub :: name of subroutine which is calling this S/R
C     iUnit      :: IO unit of parameter-file copy (already opened)
C     myThid     :: my Thread Id number
      CHARACTER*(*) data_file
      CHARACTER*(*) caller_sub
      INTEGER iUnit
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
#if !defined(USE_FORTRAN_SCRATCH_FILES) || defined(SINGLE_DISK_IO)
      CHARACTER*(MAX_LEN_FNAM) scratchFile1
#endif
      INTEGER  errIO,IL
      LOGICAL  exst
#ifdef SINGLE_DISK_IO
C     mpiRC  :: Error code reporting variable used with MPI.
      INTEGER mpiRC
#endif
#ifdef USE_PDAF
      INTEGER mpi_task_id
#endif
CEOP

      _BEGIN_MASTER(myThid)

C--   Check for parameter file
      INQUIRE( FILE=data_file, EXIST=exst )
      IF (exst) THEN
       WRITE(msgBuf,'(A,A)')
     &   ' OPEN_COPY_DATA_FILE: opening file ',data_file
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ELSE
       WRITE(msgBuf,'(A,A,A)')
     &  'File ',data_file,' does not exist!'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CALLED BY ',caller_sub
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OPEN_COPY_DATA_FILE'
      ENDIF

C     Make a scratch copy of parameter file without comments
#ifdef SINGLE_DISK_IO
      WRITE(scratchFile1,'(A,A)') 'scratch1_', data_file
      IF ( myProcId .EQ. 0 ) THEN
       OPEN( UNIT=scrUnit1, FILE=scratchFile1, STATUS='UNKNOWN' )
      ENDIF
#else /* ifndef SINGLE_DISK_IO */
#ifdef USE_FORTRAN_SCRATCH_FILES
C     this is the old default, which can cause filename conflicts on some
C     multi-node/multi-processor systems
      OPEN( UNIT=scrUnit1, STATUS='SCRATCH' )
#else
C     After opening regular files here, they are closed with STATUS='DELETE'
#ifdef USE_PDAF
      CALL GET_TASKID_PDAF(mpi_task_id)
      WRITE(scratchFile1,'(A,'//FMT_PROC_ID//',A,'//FMT_TSK_ID//')')
     &     'scratch1.', myProcId, '.', mpi_task_id
#else
      WRITE(scratchFile1,'(A,'//FMT_PROC_ID//')') 'scratch1.', myProcId
#endif
      OPEN( UNIT=scrUnit1, FILE=scratchFile1, STATUS='UNKNOWN' )
#endif /* USE_FORTRAN_SCRATCH_FILES */
#endif /* SINGLE_DISK_IO */

#ifdef SINGLE_DISK_IO
      IF ( myProcId .EQ. 0 ) THEN
#endif

C--   Open the parameter file
      OPEN( UNIT=modelDataUnit, FILE=data_file,
     &      _READONLY_ACTION STATUS='OLD', IOSTAT=errIO )
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'Unable to open parameter file: ',data_file
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R CALLED BY ',caller_sub
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R OPEN_COPY_DATA_FILE'
      ENDIF

C--   Read parameter file, make a scratch copy without comments
C     and report contents of parameter file to STDOUT
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A,A,A)') '// Parameter file "',data_file,'"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C     Read file, remove comments and apply specific syntax changes:
      DO WHILE ( .TRUE. )
       READ(modelDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter ) THEN
c        CALL NML_SET_TERMINATOR( RECORD )
         CALL NML_CHANGE_SYNTAX( RECORD, data_file, myThid )
         WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
       ENDIF
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDDO
 1001 CONTINUE
      CLOSE(modelDataUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef SINGLE_DISK_IO
      CALL FLUSH(scrUnit1)
      CLOSE(scrUnit1)
      ENDIF
# ifdef ALLOW_USE_MPI
C--   all processes must wait for process 0 to complete
C     writing scratchFile1 before opening it
      IF ( usingMPI ) THEN
        CALL MPI_BARRIER( MPI_COMM_MODEL, mpiRC )
      ENDIF
# endif
#ifdef HAVE_SYSTEM
      CALL SYSTEM('sleep 1')
#endif
      OPEN( UNIT=scrUnit1, FILE=scratchFile1,
     &      _READONLY_ACTION STATUS='OLD' )
#endif /* SINGLE_DISK_IO */

C--   Return open unit to caller
      iUnit = scrUnit1
      REWIND(iUnit)

      _END_MASTER(myThid)

      RETURN
      END
