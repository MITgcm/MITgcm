C $Header: /u/gcmpack/MITgcm/eesupp/src/eeboot.F,v 1.23 2013/11/27 01:24:05 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "PACKAGES_CONFIG.h"

CBOP
C     !ROUTINE: EEBOOT

C     !INTERFACE:
      SUBROUTINE EEBOOT

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EEBOOT
C     | o Set up execution "environment", particularly perform
C     |   steps to initialise parallel processing.
C     *==========================================================*
C     | Note: This routine can also be compiled with CPP
C     | directives set so that no multi-processing is initialised
C     | This is OK and works fine.
C     *==========================================================*

C     !CALLING SEQUENCE:
C     eeboot()
C       |
C       |-- eeboot_minimal() :: Minimal startup. Just enough to
C       |                       allow basic I/O.
C       |
C       |-- eeintro_msg()    :: Write startup greeting.
C       |
C       |-- eeset_parms()    :: Set WRAPPER parameters
C       |
C       |-- eewrite_eeenv()  :: Print WRAPPER parameter settings
C       |
C       |-- ini_procs()      :: Associate processes with grid regions.
C       |
C       |-- ini_threading_environment() :: Associate threads with grid regions.
C       |
C       |-- cpl_register()   :: Pass attributes to the coupler
C       |
C       |-- w2_eeboot()      :: Initialise WRAPPER2 execution environment

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I :: Loop counter
      INTEGER I
      LOGICAL doReport
CEOP

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
      eeBootError         = .FALSE.
      eeEndError          = .FALSE.
      DO I=1, MAX_NO_THREADS
       thError(I)                   = .FALSE.
       allMyEdgesAreSharedMemory(I) = .TRUE.
       threadIsRunning(I)           = .FALSE.
       threadIsComplete(I)          = .FALSE.
       ioErrorCount(I)              = 0
      ENDDO
      standardMessageUnit = 6
      scrUnit1            = 11
      scrUnit2            = 12
      eeDataUnit          = 13
      modelDataUnit       = 14
      errorMessageUnit    = 15
cph   errorMessageUnit = 0 is not allowed on some platforms, e.g. SP3

C--   Start minimal environment so that we can do I/O to report errors.
C     Notes
C     =====
C     o Here we start MPI and setup the I/O environment
C       that is needed for error reporting.
C     o Under MPI I/O support is very variable until
C       MPI is started. This makes is hard to trap the case
C       where mpirun is used to start a non-MPI run or
C       we try to start MPI when mpirun was not used.
C       after it is started.
C jmc: test:
C     If necessary ( COMPONENT_MODULE defined ) add a 1rst preliminary
C     call to eeset_params (in EEBOOT_MINIMAL) to set useCoupler (needed
C     very early, and the stardard call eeset_params below comes too late)
C jmc: This needs to be sorted-out in a cleaner way.
      CALL EEBOOT_MINIMAL
      IF ( eeBootError ) GOTO 999

C--   Now we can write a startup message
      CALL EEINTRO_MSG

C--   Initialise parameters associated with execution environment.
      doReport = .TRUE.
      CALL EESET_PARMS ( doReport )
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
      IF ( eeBootError ) GOTO 999

C--   Initialise variables to support "nThreads" of computation.
C     o Note the program is still running with a single thread of
C       execution at this stage.
      CALL INI_THREADING_ENVIRONMENT
      IF ( eeBootError ) GOTO 999

#ifdef ALLOW_EXCH2
C--   Initialise WRAPPER2 execution environment
      CALL W2_EEBOOT
#endif /* ALLOW_EXCH2 */

C--   Pass attributes to the coupler
#ifdef COMPONENT_MODULE
      IF ( useCoupler ) CALL CPL_REGISTER
#endif

  999 CONTINUE
      RETURN
      END
