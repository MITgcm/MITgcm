C $Header: /u/gcmpack/MITgcm/eesupp/src/eeset_parms.F,v 1.30 2011/06/08 01:39:54 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EESET_PARMS

C     !INTERFACE:
      SUBROUTINE EESET_PARMS

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
#if defined (TARGET_BGL) || defined (TARGET_CRAYXT)
      CHARACTER*(MAX_LEN_FNAM) scratchFile1
      CHARACTER*(MAX_LEN_FNAM) scratchFile2
#endif
CEOP

      NAMELIST /EEPARMS/
     & nTx, nTy, usingMPI,
     & useCubedSphereExchange,
     & useCoupler, useNEST_PARENT, useNEST_CHILD, useOASIS,
     & useSETRLSTK, useSIGREG,
     & debugMode, printMapIncludesZeros, maxLengthPrt1D

C--   For now these options are fixed as the code does
C     not fully support features for overlapping communication
C     and computation.
      usingSyncMessages          = .TRUE.

C--   The remaining parameters here are set to default values.
C--   and then any different values are read from an input
C--   file called "eedata".
C     The defaults set here are for serial execution.
C
C     nTx and nTy are the number of threads in the X and Y
C     directions.
C     nSx/nTx and nSy/nTy be whole numbers at present.
C
C     notUsingXPeriodicity and notUsingYPeriodicity affect
C     the identifying of neighbor processes in a multi-process
C     mode. On the whole the numerical model code should not
C     customise itself based on these numbers as they may be
C     removed if they do not prove useful.
C
C     usingMPI is a flag which controls whether MPI message
C     passing library calls are actually made. Note that under
C     MPI it is necessary to start a program a special way -
C     normally using a command of the form
C     % mpirun program_name
C     If usingMPI is set to TRUE but % mpirun .... was not
C     used to launch the program then an internal MPI error
C     may be generated when the first MPI call ( CALL MPI_Init )
C     is made.
C
C     useCoupler is a flag which controls communications with other
C     model components through a coupler interface.
C
C     useSETRLSTK is a flag which toggles calling a small C routine
C     which sets the stack size to "unlimited" using setrlimit()

      notUsingXPeriodicity       = .FALSE.
      notUsingYPeriodicity       = .FALSE.
      useCubedSphereExchange     = .FALSE.
      usingMPI                   = .FALSE.
      useCoupler                 = .FALSE.
      useNEST_PARENT             = .FALSE.
      useNEST_CHILD              = .FALSE.
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
C     We really ought to be using our environment file reading
C     package - but we have not written it yet.

C     Make scratch copies of input data file with and without comments
#if defined (TARGET_BGL) || defined (TARGET_CRAYXT)
      WRITE(scratchFile1,'(A,I4.4)') 'scratch1.', myProcId
      WRITE(scratchFile2,'(A,I4.4)') 'scratch2.', myProcId
      OPEN(UNIT=scrUnit1, FILE=scratchFile1, STATUS='UNKNOWN')
      OPEN(UNIT=scrUnit2, FILE=scratchFile2, STATUS='UNKNOWN')
#else
      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
#endif
      OPEN(UNIT=eeDataUnit,FILE='eedata',STATUS='OLD',
     &     err=1,IOSTAT=errIO)
      IF ( errIO .GE. 0 ) GOTO 2
    1 CONTINUE
       WRITE(msgBuf,'(A)')
     &  'S/R EESET_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Unable to open execution environment'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "eedata"'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL EEDATA_EXAMPLE
       STOP 'ABNORMAL END: S/R EESET_PARMS'
    2 CONTINUE
 1000 CONTINUE
       READ(eeDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
        IF ( RECORD(1:1) .NE. commentCharacter ) THEN
         CALL NML_SET_TERMINATOR( RECORD )
         WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
        ENDIF
        WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
       GOTO 1000
 1001 CONTINUE
      CLOSE(eeDataUnit)
C--   Report contents of parameter file
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     & '// Execution Environment parameter file "eedata"'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     & '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      iUnit = scrUnit2
      REWIND(iUnit)
 2000 CONTINUE
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE(msgBuf,standardMessageUnit, SQUEEZE_RIGHT , 1)
      GOTO 2000
 2001 CONTINUE
      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      iUnit = scrUnit1
      REWIND(iUnit)
      READ(UNIT=iUnit,NML=EEPARMS,IOSTAT=errIO,err=3)
      IF ( errIO .GE. 0 ) GOTO 4
    3 CONTINUE
#ifndef TARGET_PWR3
       WRITE(msgBuf,'(A)')
     &  'S/R EESET_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading execution environment '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "eedata"'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL EEDATA_EXAMPLE
       STOP 'ABNORMAL END: S/R EESET_PARMS'
#endif
   4  CONTINUE

C--   Execution Environment parameter file read
      CLOSE(iUnit)

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
C
C
      RETURN
      END
