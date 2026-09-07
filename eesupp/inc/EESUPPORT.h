!BOP
! !ROUTINE: EESUPPORT.h
! !INTERFACE:
! include "EESUPPORT.h"
!
! !DESCRIPTION:
! *==========================================================*
! | EESUPPORT.h                                              |
! *==========================================================*
! | Support data structures for the MITgcm UV                |
! | "execution environment" code. This data should be        |
! | private to the execution environment routines. Data      |
! | which needs to be accessed directly by a numerical model |
! | goes in EEPARAMS.h.                                      |
! *==========================================================*
!EOP

! ERROR_HEADER        - String which prefixes error messages
      CHARACTER(len=*) :: ERROR_HEADER
      PARAMETER ( ERROR_HEADER = ' *** ERROR ***' )
! PROCESS_HEADER      - String which prefixes processor number
      CHARACTER(len=*) :: PROCESS_HEADER
      PARAMETER ( PROCESS_HEADER = 'PID.TID' )

! MAX_NUM_COMM_MODES - Maximum number of communication modes
! COMM_NONE       - No edge communication
! COMM_MSG        - Use messages to communicate edges
! COMM_PUT        - Use put to communicate edges
! COMM_GET        - Use get to communicate edges
! Note - commName holds an identifying name for each communication
!        mode. The COMM_ parameters are used to index commName
!        so the COMM_ parameters need to be in the range
!        1 : MAX_NUM_COMM_MODES.
      INTEGER :: MAX_NUM_COMM_MODES
      PARAMETER ( MAX_NUM_COMM_MODES = 4 )
      INTEGER :: COMM_NONE
      PARAMETER ( COMM_NONE   =   1 )
      INTEGER :: COMM_MSG
      PARAMETER ( COMM_MSG    =   2 )
      INTEGER :: COMM_PUT
      PARAMETER ( COMM_PUT    =   3 )
      INTEGER :: COMM_GET
      PARAMETER ( COMM_GET    =   4 )
      COMMON /EESUPP_COMMNAME/ commName
      CHARACTER(len=10) :: commName(MAX_NUM_COMM_MODES)

! Tile identifiers
! Tiles have a number that is unique over the global domain.
! A tile that is not there has its number set to NULL_TILE
      INTEGER :: NULL_TILE
      PARAMETER ( NULL_TILE = -1 )


!--   COMMON /EESUPP_C/ Execution environment support character variables
! myProcessStr - String identifying my process number
      COMMON /EESUPP_C/ myProcessStr
      CHARACTER(len=128) :: myProcessStr

!--   COMMON /EESUPP_L/ Execution environment support logical variables
! initMPError - Flag indicating error during multi-processing
!               initialisation.
! finMPError  - Flag indicating error during multi-processing
!               termination.
! ThError     - Thread detected an error.
! usingMPI    - Flag controlling use of MPI routines. This flag
!               allows either MPI or threads to be used in a
!               shared memory environment which can be a useful
!               debugging/performance analysis tool.
! usingSyncMessages - Flag that causes blocking communication to be used
!                     if possible. When false non-blocking EXCH routines
!                     will be used if possible.
! notUsingXPeriodicity - Flag indicating no X/Y boundary wrap around
! notUsingYPeriodicity   This affects the communication routines but
!                        is generally ignored in the numerical model
!                        code.
! threadIsRunning, threadIsComplete - Flags used to check for correct behaviour
!                                     of multi-threaded code.
!                                     threadIsRunning is used to check that the
!                                     threads we need are running. This catches the
!                                     situation where a program eedata file has nTthreads
!                                     greater than the setenv PARALLEL or NCPUS variable.
!                                     threadIsComplete is used to flag that a thread has
!                                     reached the end of the model. This is used as a check to
!                                     trap problems that might occur if one thread "escapes"
!                                     the main.F master loop. This should not happen
!                                     if the multi-threading compilation tools works right.
!                                     But (see for example KAP) this is not always the case!
      COMMON /EESUPP_L/ thError, threadIsRunning, threadIsComplete,               &
     &      allMyEdgesAreSharedMemory, usingMPI, usingSyncMessages,               &
     &      notUsingXPeriodicity, notUsingYPeriodicity
      LOGICAL :: thError(MAX_NO_THREADS)
      LOGICAL :: threadIsRunning(MAX_NO_THREADS)
      LOGICAL :: threadIsComplete(MAX_NO_THREADS)
      LOGICAL :: allMyEdgesAreSharedMemory(MAX_NO_THREADS)
      LOGICAL :: usingMPI
      LOGICAL :: usingSyncMessages
      LOGICAL :: notUsingXPeriodicity
      LOGICAL :: notUsingYPeriodicity

!--   COMMON /EESUPP_I/ Parallel support integer globals
! pidW   -  Process  ID of neighbor to West
! pidE   -           ditto             East
! pidN   -           ditto             North
! pidS   -           ditto             South
!          Note: pid[XY] is not necessairily the UNIX
!                process id - it is just an identifying
!                number.
! myPid  - My own process id
! nProcs - Number of processes
! westCommunicationMode  - Mode of communication for each tile face
! eastCommunicationMode
! northCommunicationMode
! southCommunicationMode
! bi0   - Low cartesian tile index for this process
! bj0     Note - In a tile distribution with holes bi0 and bj0
!                are not useful. Neighboring tile indices must
!                be derived some other way.
! tileNo       - Tile identification number for my tile and
! tileNo[WENS]   my N,S,E,W neighbor tiles.
! tilePid[WENS] - Process identification number for
!                 my N,S,E,W neighbor tiles.
! nTx, nTy    - No. threads in X and Y. This assumes a simple
!               cartesian gridding of the threads which is not
!               required elsewhere but that makes it easier.
      COMMON /EESUPP_I/                                                           &
     &      myPid, nProcs, pidW, pidE, pidN, pidS,                                &
     &      tileCommModeW,  tileCommModeE,                                        &
     &      tileCommModeN,  tileCommModeS,                                        &
     &      tileNo, tileNoW, tileNoE, tileNoS, tileNoN,                           &
     &      tilePidW, tilePidE, tilePidS, tilePidN,                               &
     &      tileBiW, tileBiE, tileBiS, tileBiN,                                   &
     &      tileBjW, tileBjE, tileBjS, tileBjN,                                   &
     &      tileTagSendW, tileTagSendE, tileTagSendS, tileTagSendN,               &
     &      tileTagRecvW, tileTagRecvE, tileTagRecvS, tileTagRecvN
      INTEGER :: myPid
      INTEGER :: nProcs
      INTEGER :: pidW
      INTEGER :: pidE
      INTEGER :: pidN
      INTEGER :: pidS
      INTEGER :: tileCommModeW ( nSx, nSy )
      INTEGER :: tileCommModeE ( nSx, nSy )
      INTEGER :: tileCommModeN ( nSx, nSy )
      INTEGER :: tileCommModeS ( nSx, nSy )
      INTEGER :: tileNo( nSx, nSy )
      INTEGER :: tileNoW( nSx, nSy )
      INTEGER :: tileNoE( nSx, nSy )
      INTEGER :: tileNoN( nSx, nSy )
      INTEGER :: tileNoS( nSx, nSy )
      INTEGER :: tilePidW( nSx, nSy )
      INTEGER :: tilePidE( nSx, nSy )
      INTEGER :: tilePidN( nSx, nSy )
      INTEGER :: tilePidS( nSx, nSy )
      INTEGER :: tileBiW( nSx, nSy )
      INTEGER :: tileBiE( nSx, nSy )
      INTEGER :: tileBiN( nSx, nSy )
      INTEGER :: tileBiS( nSx, nSy )
      INTEGER :: tileBjW( nSx, nSy )
      INTEGER :: tileBjE( nSx, nSy )
      INTEGER :: tileBjN( nSx, nSy )
      INTEGER :: tileBjS( nSx, nSy )
      INTEGER :: tileTagSendW( nSx, nSy )
      INTEGER :: tileTagSendE( nSx, nSy )
      INTEGER :: tileTagSendN( nSx, nSy )
      INTEGER :: tileTagSendS( nSx, nSy )
      INTEGER :: tileTagRecvW( nSx, nSy )
      INTEGER :: tileTagRecvE( nSx, nSy )
      INTEGER :: tileTagRecvN( nSx, nSy )
      INTEGER :: tileTagRecvS( nSx, nSy )

#ifdef ALLOW_USE_MPI
!--   Include MPI standard Fortran header file
#include "mpif.h"
#define _mpiTRUE_  1
#define _mpiFALSE_ 0

!--   COMMON /EESUPP_MPI_I/ MPI parallel support integer globals
! mpiPidW   - MPI process id for west neighbor.
! mpiPidE   - MPI process id for east neighbor.
! mpiPidN   - MPI process id for north neighbor.
! mpiPidS   - MPI process id for south neighbor.
! mpiPidNW  - MPI process id for northwest neighbor.
! mpiPidNE  - MPI process id for northeast neighbor.
! mpiPidSW  - MPI process id for southwest neighbor.
! mpiPidSE  - MPI process id for southeast neighbor.
! mpiPidIO  - MPI process to use for IO.
! mpiNprocs - No. of MPI processes.
! mpiMyId   - MPI process id of me.
! mpiComm   - MPI communicator to use.
! mpiPx     - My MPI proc. grid X coord
! mpiPy     - My MPI proc. grid Y coord
! mpiXGlobalLo - My bottom-left (south-west) x-coordinate in
!                global domain.
! mpiYGlobalLo - My bottom-left (south-west) y-coordinate in
!                global domain.
! mpiTypeXFaceBlock_xy_r4  - Primitives for communicating edge
! mpiTypeXFaceBlock_xy_r8    of a block.
! mpiTypeYFaceBlock_xy_r4    XFace is used in east-west transfer
! mpiTypeYFaceBlock_xy_r8    YFace is used in nrth-south transfer
! mpiTypeXFaceBlock_xyz_r4   xy is used in two-dimensional arrays
! mpiTypeXFaceBlock_xyz_r8   xyz is used with three-dimensional arrays
! mpiTypeYFaceBlock_xyz_r4   r4 is used for real*4 data
! mpiTypeYFaceBlock_xyz_r8   r8 is used for real*8 data
! mpiTypeXFaceThread_xy_r4  - Composites of the above primitives
! mpiTypeXFaceThread_xy_r8    for communicating edges of all blocks
! mpiTypeYFaceThread_xy_r4    owned by a thread.
! mpiTypeYFaceThread_xy_r8
! mpiTypeXFaceThread_xyz_r4
! mpiTypeXFaceThread_xyz_r8
! mpiTypeYFaceThread_xyz_r4
! mpiTypeYFaceBlock_xyz_r8
! mpiTagE       - Tags are needed to mark requests when MPI is running
! mpiTagW         between multithreaded processes or when the same process.
! mpiTagS         is a neighbor in more than one direction. The tags ensure that
! mpiTagN         a thread will get the message it is looking for.
! mpiTagSW        The scheme adopted is to tag messages according to
! mpiTagSE        the direction they are travelling. Thus a message
! mpiTagNW        travelling east is tagged mpiTagE. However, in a
! mpiTagNE        multi-threaded environemnt several messages could
!                 be travelling east from the same process at the
!                 same time. The tag is therefore modified to
!                 be mpiTag[EWS...]*nThreads+myThid. This requires that
!                 each thread also know the thread ids of its "neighbor"
!                 threads.
      COMMON /EESUPP_MPI_I/                                                       &
     &      mpiPidW,  mpiPidE,  mpiPidS,  mpiPidN,                                &
     &      mpiPidSE, mpiPidSW, mpiPidNE, mpiPidNW,                               &
     &      mpiPidIo, mpiMyId, mpiNProcs, mpiComm,                                &
     &      mpiPx, mpiPy, mpiXGlobalLo, mpiYGlobalLo,                             &
     &      mpiTypeXFaceBlock_xy_r4, mpiTypeXFaceBlock_xy_r8,                     &
     &      mpiTypeYFaceBlock_xy_r4, mpiTypeYFaceBlock_xy_r8,                     &
     &      mpiTypeXFaceBlock_xyz_r4, mpiTypeXFaceBlock_xyz_r8,                   &
     &      mpiTypeYFaceBlock_xyz_r4, mpiTypeYFaceBlock_xyz_r8,                   &
     &      mpiTypeXFaceThread_xy_r4, mpiTypeXFaceThread_xy_r8,                   &
     &      mpiTypeYFaceThread_xy_r4, mpiTypeYFaceThread_xy_r8,                   &
     &      mpiTypeXFaceThread_xyz_r4, mpiTypeXFaceThread_xyz_r8,                 &
     &      mpiTypeYFaceThread_xyz_r4, mpiTypeYFaceThread_xyz_r8,                 &
     &      mpiTagE, mpiTagW, mpiTagN, mpiTagS,                                   &
     &      mpiTagSE, mpiTagSW, mpiTagNW, mpiTagNE

      INTEGER :: mpiPidW
      INTEGER :: mpiPidE
      INTEGER :: mpiPidS
      INTEGER :: mpiPidN
      INTEGER :: mpiPidSW
      INTEGER :: mpiPidSE
      INTEGER :: mpiPidNW
      INTEGER :: mpiPidNE
      INTEGER :: mpiPidIO
      INTEGER :: mpiMyId
      INTEGER :: mpiNProcs
      INTEGER :: mpiComm
      INTEGER :: mpiPx
      INTEGER :: mpiPy
      INTEGER :: mpiXGlobalLo
      INTEGER :: mpiYGlobalLo
      INTEGER :: mpiTypeXFaceBlock_xy_r4
      INTEGER :: mpiTypeXFaceBlock_xy_r8
      INTEGER :: mpiTypeYFaceBlock_xy_r4
      INTEGER :: mpiTypeYFaceBlock_xy_r8
      INTEGER :: mpiTypeXFaceBlock_xyz_r4
      INTEGER :: mpiTypeXFaceBlock_xyz_r8
      INTEGER :: mpiTypeYFaceBlock_xyz_r4
      INTEGER :: mpiTypeYFaceBlock_xyz_r8
      INTEGER :: mpiTypeXFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER :: mpiTypeXFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER :: mpiTypeYFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER :: mpiTypeYFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER :: mpiTypeXFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER :: mpiTypeXFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER :: mpiTypeYFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER :: mpiTypeYFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER :: mpiTagNW
      INTEGER :: mpiTagNE
      INTEGER :: mpiTagSW
      INTEGER :: mpiTagSE
      INTEGER :: mpiTagW
      INTEGER :: mpiTagE
      INTEGER :: mpiTagN
      INTEGER :: mpiTagS

!--   COMMON /MPI_FULLMAP_I/ holds integer arrays of the full list of MPI process
! mpi_myXGlobalLo :: List of all processors bottom-left X-index in global domain
! mpi_myYGlobalLo :: List of all processors bottom-left Y-index in global domain
!                    Note: needed for mpi gather/scatter routines & singleCpuIO.
      COMMON /MPI_FULLMAP_I/                                                      &
     &      mpi_myXGlobalLo, mpi_myYGlobalLo
      INTEGER :: mpi_myXGlobalLo(nPx*nPy)
      INTEGER :: mpi_myYGlobalLo(nPx*nPy)

! MPI communicator describing this model realization
      COMMON /MPI_COMMS/                                                          &
     &      MPI_COMM_MODEL
      INTEGER :: MPI_COMM_MODEL

#endif /* ALLOW_USE_MPI */
