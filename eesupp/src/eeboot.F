C $Header: /u/gcmpack/MITgcm/eesupp/src/eeboot.F,v 1.8 2000/03/27 22:25:40 adcroft Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE EEBOOT
C     /==========================================================\
C     | SUBROUTINE EEBOOT                                        |
C     | o Set up execution "environment", particularly perform   |
C     |   steps to initialise parallel processing.               |
C     |==========================================================|
C     | Note: This routine can also be compiled with CPP         |
C     | directives set so that no multi-processing is initialised|
C     | This is OK and should work fine.                         |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
CEndOfInterface

C     == Local variables ==
C     I - Loop counter
      INTEGER I

C--   Set error reporting flags and I/O streams
C     fatalError is a flag used to indicate that the program
C                ended abnormally.
C     errorMessageUnit is the Fortran unit number used for
C                      writing error messages.
C     standardMessageUnit is the Fortran unit number used for
C                          writing textual, informational output.
C     eeBootError is a flag used to indicate an error in the
C                "execution environment" startup pahse as opposed
C                to the simulation pahse of the execution.
C     eeEndError is used to indicate an "execution environment" shutdown 
C                error.
C     thError flag used to allow a thread to report an error.
C             This is only really used during the startup process, 
C             although it could be used elsewhere.
C     allMyEdgesAreSharedMemory flag which says for a particular
C                               set of blocks the only form of communication
C                               it does is using shared memory.
C     threadIsRunning used during startup to enable check that all 
C                     the threads are running.
      fatalError          = .FALSE.
      errorMessageUnit    = 0
      standardMessageUnit = 6
      eeBootError         = .FALSE.
      eeEndError          = .FALSE.
      DO I=1, MAX_NO_THREADS
       thError(I)                   = .FALSE.
       allMyEdgesAreSharedMemory(I) = .TRUE.
       threadIsRunning(I)           = .FALSE.
       threadIsComplete(I)          = .FALSE.
       ioErrorCount(I)              = 0
      ENDDO
      scrUnit1      = 11
      scrUnit2      = 12
      eeDataUnit    = 13
      modelDataUnit = 14
C     Annoyingly there is no universal way to have the usingMPI
C     parameter work as one might expect. This is because, on some
C     systems I/O does not work until MPI_Init has been called. The
C     solution for now is that the parameter below may need to be changed
C     manually!
#ifdef ALLOW_USE_MPI
      usingMPI = .TRUE.
#endif

C--   Start minimal environment so that we can do I/O
C--   to report errors.
C     Notes
C     =====
C     o Here we start MPI and setup the I/O environment
C       thatis needed for error reporting.
C     o Under MPI I/O support is very variable until
C       MPI is started. This makes is hard to trap the case
C       where mpirun is used to start a non-MPI run or 
C       we try to start MPI when mpirun was not used.
C       after it is started.
      CALL EEBOOT_MINIMAL
      IF ( eeBootError ) GOTO 999

C--   Now we can write a startup message
      CALL EEINTRO_MSG

C--   Initialise parameters associated with execution environment.
      CALL EESET_PARMS
      IF ( eeBootError ) GOTO 999

C--   Write summary of execution environment configuration for this run
      CALL EEWRITE_EEENV

C--   Now do the rest of the multi-process startup.
C     o Here we map processes to the model grid.
C     o Print tables of process-grid mappings.
C     o Do other miscellaneous multi-processing set up steps. For 
C       example under MPI we create datatypes for communication
C       of array edges.
      CALL INI_PROCS
#ifdef LETS_MAKE_JAM
      CALL INI_JAM
#endif
      IF ( eeBootError ) GOTO 999

C--   Initialise variables to support "nThreads" of computation.
C     o Note the program is still running with a single thread of
C       execution at this stage.
      CALL INI_THREADING_ENVIRONMENT
      IF ( eeBootError ) GOTO 999

#ifdef USE_DFILE
C--   Initiialise the multi-process IO package
      CALL DFILE_INIT
#endif
 
  999 CONTINUE
      RETURN
      END
