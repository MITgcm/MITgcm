#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EESET_PARMS

C     !INTERFACE:
      SUBROUTINE EESET_PARMS( procId, doReport )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EESET\_PARMS
C     | o Routine to set model "parameters"
C     *==========================================================*
C     | This routine is called from the high-level wrapper
C     | after multi-process paralle processing has started but
C     | before multi-threaded parallelism. THe routine reads an
C     | an "execution environment" input parameter file holding
C     | information about the number of threads at run-time.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     !INPUT PARAMETERS:
C     procId   :: this process id. number (either in World or in Model)
C     doReport :: if false, skip error stop and any report to std-out/err
      INTEGER  procId
      LOGICAL  doReport

C     !FUNCTIONS:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     iUnit  :: Work variable for IO unit number
C     errIO  :: IO unit error flag
C     IL     :: Temp. for index strings
C     msgBuf :: Temp. for textual I/O
C     record :: Temp. for textual I/O
      INTEGER IL
      INTEGER errIO
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
# if !defined(USE_FORTRAN_SCRATCH_FILES) || defined(SINGLE_DISK_IO)
      CHARACTER*(MAX_LEN_FNAM) scratchFile1
#endif
#ifdef SINGLE_DISK_IO
C     mpiRC  :: Error code reporting variable used with MPI.
      INTEGER mpiRC
#endif
#ifdef USE_PDAF
      INTEGER mpi_task_id
#endif
CEOP

      NAMELIST /EEPARMS/
     & nTx, nTy, usingMPI,
     & useCubedSphereExchange,
     & useCoupler, useNEST_PARENT, useNEST_CHILD,
     & useNest2W_parent, useNest2W_child, useOASIS,
     & useSETRLSTK, useSIGREG,
     & debugMode, printMapIncludesZeros, maxLengthPrt1D

C--   For now these options are fixed as the code does not fully support
C     features for overlapping communication and computation.
      usingSyncMessages          = .TRUE.

C--   The remaining parameters here are set to default values; and then
C--   any different values are read from an input file called "eedata".
C     The defaults set here are for serial execution.
C
C     nTx and nTy are the number of threads in the X and Y directions.
C     nSx/nTx and nSy/nTy be whole numbers at present.
C
C     notUsingXPeriodicity and notUsingYPeriodicity affect the identifying
C     of neighbor processes in a multi-process mode.
C     On the whole the numerical model code should not customise itself based
C     on these numbers as they may be removed if they do not prove useful.
C
C     usingMPI is a flag which controls whether MPI message passing library
C     calls are actually made. Note that under MPI it is necessary to start
C     a program a special way - normally using a command of the form
C     % mpirun program_name
C     If usingMPI is set to TRUE but % mpirun .... was not used to launch
C     the program then an internal MPI error may be generated when the first
C     MPI call ( CALL MPI_Init ) is made.
C
C     useCoupler is a flag which controls communications with other
C     model components through a coupler interface.
C
C     useSETRLSTK is a flag which toggles calling a small C routine
C     which sets the stack size to "unlimited" using setrlimit()

      notUsingXPeriodicity       = .FALSE.
      notUsingYPeriodicity       = .FALSE.
      useCubedSphereExchange     = .FALSE.
#ifdef ALLOW_USE_MPI
      usingMPI                   = .TRUE.
#else
      usingMPI                   = .FALSE.
#endif
      useCoupler                 = .FALSE.
      useNEST_PARENT             = .FALSE.
      useNEST_CHILD              = .FALSE.
      useNest2W_parent           = .FALSE.
      useNest2W_child            = .FALSE.
      useOASIS                   = .FALSE.
      nTx                        = 1
      nTy                        = 1
      useSETRLSTK                = .FALSE.
      useSIGREG                  = .FALSE.

C--   Parameter for printing (ascii) to Std-Oupt:
C     Print debug msg (sequence of S/R calls)
      debugMode = .FALSE.
C     Text map plots of fields ignore exact zero values
      printMapIncludesZeros = .FALSE.
C     Maximum length for printing (to Std-Msg-Unit) 1-D array
      maxLengthPrt1D = 65

C     To write output to global-files and from Master MPI process only
C     NOTE: read from main parameter file "data"
      useSingleCpuIO = .FALSE.

C--   Read in data from eedata file

C     Make a scratch copy of input eedata file without comments
#ifdef SINGLE_DISK_IO
C     Stop if called from eeboot_minimal.F before myProcId is set
      IF ( .NOT.doReport )
     &     STOP 'ABNORMAL END: S/R EESET_PARMS: myProcId unset'
      WRITE(scratchFile1,'(A)') 'scratch1'
      IF( myProcId .EQ. 0 ) THEN
       OPEN( UNIT=scrUnit1, FILE=scratchFile1, STATUS='UNKNOWN' )
      ENDIF
#else /* ifndef SINGLE_DISK_IO */
# ifdef USE_FORTRAN_SCRATCH_FILES
C     this is the old default, which can cause filename conflicts on some
C     multi-node/multi-processor systems
      OPEN( UNIT=scrUnit1, STATUS='SCRATCH' )
# else
C     this definition will go into CPP_EEMACROS.h, once this method is
C     properly established
C     After opening regular files here, they are closed with STATUS='DELETE'
#ifdef USE_PDAF
      CALL GET_TASKID_PDAF(mpi_task_id)
      WRITE(scratchFile1,'(A,'//FMT_PROC_ID//',A,'//FMT_TSK_ID//')')
     &     'scratch1.', procId, '.', mpi_task_id
#else
      WRITE(scratchFile1,'(A,'//FMT_PROC_ID//')') 'scratch1.', procId
#endif
      OPEN( UNIT=scrUnit1, FILE=scratchFile1, STATUS='UNKNOWN' )
# endif
#endif /* SINGLE_DISK_IO */

#ifdef SINGLE_DISK_IO
      IF( myProcId .EQ. 0 ) THEN
#endif

C--   Open the parameter file eedata
      OPEN( UNIT=eeDataUnit, FILE='eedata', STATUS='OLD',
     &      err=1, IOSTAT=errIO )
      IF ( errIO .GE. 0 ) GOTO 2
    1 CONTINUE
      IF ( doReport ) THEN
        WRITE(msgBuf,'(2A)') 'EESET_PARMS: ',
     &       'Unable to open parameter file "eedata"'
        CALL PRINT_ERROR( msgBuf, 1 )
        CALL EEDATA_EXAMPLE
C note: At this early stage, MPI might not be yet fully set-up; for this reason
C       set error flag and return (to avoid a call to ALL_PROC_DIE before stop)
c       STOP 'ABNORMAL END: S/R EESET_PARMS'
        eeBootError = .TRUE.
      ELSE
        RETURN
      ENDIF
    2 CONTINUE

C--   Read parameter eedata file, make a scratch copy without comments
C     and report contents of parameter file to STDOUT
      IF ( doReport ) THEN
       WRITE(msgBuf,'(A)')
     & '// ======================================================='
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A)')
     & '// Execution Environment parameter file "eedata"'
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1)
       WRITE(msgBuf,'(A)')
     & '// ======================================================='
       CALL PRINT_MESSAGE(msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1)
      ENDIF

C     Read file, remove comments and apply specific syntax changes:
 1000 CONTINUE
       READ(eeDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter ) THEN
         CALL NML_SET_TERMINATOR( RECORD )
         WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
       ENDIF
       IF ( doReport ) THEN
        WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
        CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1 )
       ENDIF
       GOTO 1000
 1001 CONTINUE
      CLOSE(eeDataUnit)
      IF ( doReport ) THEN
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE(msgBuf,standardMessageUnit, SQUEEZE_RIGHT, 1)
      ENDIF

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
      OPEN( UNIT=scrUnit1, FILE=scratchFile1, STATUS='OLD' )
#endif /* SINGLE_DISK_IO */

C--   Read namelist
      iUnit = scrUnit1
      REWIND(iUnit)
      READ(UNIT=iUnit,NML=EEPARMS,IOSTAT=errIO,err=3)
      IF ( errIO .GE. 0 ) GOTO 4
    3 CONTINUE
#ifndef TARGET_PWR3
      IF ( doReport ) THEN
       WRITE(msgBuf,'(2A)') 'EESET_PARMS: ',
     &      'Error reading parameter file "eedata"'
       CALL PRINT_ERROR( msgBuf, 1 )
       CALL EEDATA_EXAMPLE
       eeBootError = .TRUE.
      ENDIF
#endif
   4  CONTINUE

C--   Execution Environment parameter file read
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

#ifdef ALLOW_USE_MPI
#ifdef ALWAYS_USE_MPI
      IF ( doReport .AND. .NOT.usingMPI ) THEN
       WRITE(msgBuf,'(2A)') 'EESET_PARMS: ',
     &                      'in eedata: usingMPI=F conflicts'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)') 'EESET_PARMS:  with #define ALWAYS_USE_MPI'
       CALL PRINT_ERROR( msgBuf, 1 )
       eeBootError = .TRUE.
      ENDIF
      usingMPI = .TRUE.
#endif
#else /* ALLOW_USE_MPI */
      IF ( doReport .AND. usingMPI ) THEN
       WRITE(msgBuf,'(2A)') 'EESET_PARMS: ',
     &                      'in eedata: usingMPI=T conflicts'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)') 'EESET_PARMS:  with #undef ALLOW_USE_MPI'
       CALL PRINT_ERROR( msgBuf, 1 )
       eeBootError = .TRUE.
      ENDIF
      usingMPI = .FALSE.
#endif /* ALLOW_USE_MPI */

Cdbg  eeDataUnit = 42
Cdbg  OPEN(UNIT=eeDataUnit,FILE='eedata',STATUS='OLD',IOSTAT=errIO)
Cdbg  IF ( errIO .LT. 0 ) GOTO 11
Cdbg  DO K=1, 10
Cdbg   READ(eedataUnit,IOSTAT=errIO)
Cdbg   IF ( errIO .LT. 0 ) GOTO 11
Cdbg  ENDDO
Cdbg  READ(eedataUnit,FMT='(30X,1X,L23)',IOSTAT=errIO) notUsingXPeriodicity
Cdbg  IF ( errIO .LT. 0 ) GOTO 11
Cdbg  READ(eedataUnit,FMT='(30X,1X,L23)',IOSTAT=errIO) notUsingYPeriodicity
Cdbg  IF ( errIO .LT. 0 ) GOTO 11
Cdbg  READ(eedataUnit,FMT='(30X,1X,L23)',IOSTAT=errIO) usingMPI
Cdbg  IF ( errIO .LT. 0 ) GOTO 11
Cdbg  READ(eedataUnit,FMT='(30X,1X,I3)',IOSTAT=errIO) nTx
Cdbg  IF ( errIO .LT. 0 ) GOTO 11
Cdbg  READ(eedataUnit,FMT='(30X,1X,I3)',IOSTAT=errIO) nTy

Cdbg  IF (errIO .LT. 0 ) eeBootError = .TRUE.
Cdbg  CLOSE(eeDataUnit,IOSTAT=errIO)
Cdbg  IF ( eeBootError .OR. errIO .LT. 0 ) THEN
C--    Report that an error occured
Cdbg   eeBootError = .TRUE.
Cdbg   WRITE(msgBuf,'(A)' )
Cdbg &  'S/R EESET_PARMS: Error reading "eedata" execution environment file'
Cdbg   CALL PRINT_ERROR( msgBuf , 1)
Cdbg  ELSE
C--    Write summary of settings that were selected
Cdbg  ENDIF

      IF ( doReport ) THEN
C--   Set parameters for EXCH communication routines
C     Note: only done once when called with doReport=T

        exchCollectStatistics = .TRUE.
C--   Turn off memsync by default (e.g. needed for threads on SUNs)
        exchNeedsMemsync = .TRUE.
        exchUsesBarrier  = .TRUE.
        IF ( usingMPI ) THEN
C--   ... except that MPI needs this until some counter problem is fixed.
          exchNeedsMemsync = .FALSE.
          exchUsesBarrier  = .FALSE.
        ENDIF

C--   End setting parameters for EXCH communication routines
      ENDIF

      RETURN
      END
