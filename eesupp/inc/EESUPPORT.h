C $Header: /u/gcmpack/MITgcm/eesupp/inc/EESUPPORT.h,v 1.2 1998/04/23 20:59:35 cnh Exp $
C
C     /==========================================================\
C     | EESUPPORT.h                                              |
C     |==========================================================|
C     | Support data structures for the MITgcm UV "execution     |
C     | environment" code. This data should be private to the    |
C     | execution environment routines. Data which needs to be   |
C     | accessed directly by the numerical model goes in         |
C     | EEPARAMS.h.                                              |
C     \==========================================================/

C     ERROR_HEADER        - String which prefixes error messages
      CHARACTER*(*) ERROR_HEADER
      PARAMETER ( ERROR_HEADER = ' *** ERROR ***' )
C     PROCESS_HEADER      - String which prefixes processor number
      CHARACTER*(*) PROCESS_HEADER
      PARAMETER ( PROCESS_HEADER = 'PID.TID' )

C     COMM_NONE       - No edge communication
C     COMM_MPI        - Use MPI to communicate edges
C     COMM_SHMPG      - Use shm get/put to communicate edges
C     COMM_SHARED     - Use true shared memory to communicate edges
      INTEGER COMM_NONE
      PARAMETER ( COMM_NONE   =   0 )
      INTEGER COMM_MPI
      PARAMETER ( COMM_MPI    =   1 )
      INTEGER COMM_SHMPG
      PARAMETER ( COMM_SHMPG  =   2 )
      INTEGER COMM_SHARED
      PARAMETER ( COMM_SHARED =   3 )

C--   COMMON /EESUPP_C/ Execution environment support character variables
C     myProcessStr - String identifying my process number
      COMMON /EESUPP_C/ myProcessStr
      CHARACTER*128 myProcessStr

C--   COMMON /EESUPP_L/ Execution environment support logical variables
C     initMPError - Flag indicating error during multi-processing
C                   initialisation.
C     finMPError  - Flag indicating error during multi-processing
C                   termination.
C     ThError     - Thread detected an error.
C     usingMPI    - Flag controlling use of MPI routines. This flag
C                   allows either MPI or threads to be used in a
C                   shared memory environment which can be a useful
C                   debugging/performance analysis tool.
C     usingSyncMessages - Flag that causes blocking communication to be used
C                         if possible. When false non-blocking EXCH routines
C                         will be used if possible.
C     notUsingXPeriodicity - Flag indicating no X/Y boundary wrap around
C     notUsingYPeriodicity   This affects the communication routines but
C                            is generally ignored in the numerical model
C                            code.
C     threadIsRunning, threadIsComplete - Flags used to check for correct behaviour
C                                         of multi-threaded code.
C                                         threadIsRunning is used to check that the
C                                         threads we need are running. This catches the
C                                         situation where a program eedata file has nTthreads
C                                         greater than the setenv PARALLEL or NCPUS variable.
C                                         threadIsComplete is used to flag that a thread has
C                                         reached the end of the model. This is used as a check to
C                                         trap problems that might occur if one thread "escapes"
C                                         the main.F master loop. This should not happen
C                                         if the multi-threading compilation tools works right.
C                                         But (see for example KAP) this is not always the case!
      COMMON /EESUPP_L/ thError, threadIsRunning, threadIsComplete,
     & allMyEdgesAreSharedMemory, usingMPI, usingSyncMessages,
     & notUsingXPeriodicity, notUsingYPeriodicity
      LOGICAL thError(MAX_NO_THREADS)
      LOGICAL threadIsRunning(MAX_NO_THREADS)
      LOGICAL threadIsComplete(MAX_NO_THREADS)
      LOGICAL allMyEdgesAreSharedMemory(MAX_NO_THREADS)
      LOGICAL usingMPI
      LOGICAL usingSyncMessages
      LOGICAL notUsingXPeriodicity
      LOGICAL notUsingYPeriodicity
 

C--   COMMON /EESUPP_I/ Parallel support integer globals
C     pidW   - "Process" ID of neighbor to West
C     pidE   -    "       "       "        East
C     pidN   -    "       "       "        North
C     pidS   -    "       "       "        South
C     pidNE  -    "       "       "        North-East
C     pidNW  -    "       "       "        North-West
C     pidSE  -    "       "       "        South-East
C     pidSW  -    "       "       "        South-West
C              Note: pid[XY] is not necessairily the UNIX
C                    process id - it is just an identifying
C                    number.
C     commW       - Communication method at thread edge to the
C     commE         west (W), east (E), south (S), north (N).
C     commS
C     commN
C     myThrS      - Thread number of neighboring thread, used
C     myThrN        to match senders with receivers of
C     myThrW        messages.
C     myThrE
C     myThrSW
C     myThrSE
C     myThrNW
C     myThrNE
C     nTx, nTy    - No. threads in X and Y. This assumes a simple cartesian
C                   gridding of the threads which is not required elsewhere 
C                   but that makes it easier.
      COMMON /EESUPP_I/
     & pidW, pidE, pidS, pidN, pidSE, pidSW, pidNE, pidNW,
     & commW, commN, commS, commE,
     & myThrS,  myThrN,  myThrW,  myThrE,
     & myThrNE, myThrNW, myThrSE, myThrSW
      INTEGER commW(MAX_NO_THREADS)
      INTEGER commE(MAX_NO_THREADS)
      INTEGER commN(MAX_NO_THREADS)
      INTEGER commS(MAX_NO_THREADS)
      INTEGER pidW
      INTEGER pidE
      INTEGER pidS
      INTEGER pidN
      INTEGER pidSE
      INTEGER pidSW
      INTEGER pidNE
      INTEGER pidNW
      INTEGER myThrS(MAX_NO_THREADS)
      INTEGER myThrN(MAX_NO_THREADS)
      INTEGER myThrW(MAX_NO_THREADS)
      INTEGER myThrE(MAX_NO_THREADS)
      INTEGER myThrSW(MAX_NO_THREADS)
      INTEGER myThrNW(MAX_NO_THREADS)
      INTEGER myThrNE(MAX_NO_THREADS)
      INTEGER myThrSE(MAX_NO_THREADS)

#ifdef ALLOW_USE_MPI
C--   Include MPI standard Fortran header file
#include "mpif.h"
#define _mpiTRUE_  1
#define _mpiFALSE_ 0

C--   COMMON /EESUPP_MPI_I/ MPI parallel support integer globals
C     mpiPidW   - MPI process id for west neighbor.
C     mpiPidE   - MPI process id for east neighbor.
C     mpiPidN   - MPI process id for north neighbor.
C     mpiPidS   - MPI process id for south neighbor.
C     mpiPidNW  - MPI process id for northwest neighbor.
C     mpiPidNE  - MPI process id for northeast neighbor.
C     mpiPidSW  - MPI process id for southwest neighbor.
C     mpiPidSE  - MPI process id for southeast neighbor.
C     mpiPidIO  - MPI process to use for IO.
C     mpiNprocs - No. of MPI processes.
C     mpiMyId   - MPI process id of me.
C     mpiComm   - MPI communicator to use.
C     mpiPx     - My MPI proc. grid X coord
C     mpiPy     - My MPI proc. grid Y coord
C     mpiXGlobalLo - My bottom-left (south-west) x-coordinate in
C                    global domain.
C     mpiYGlobalLo - My bottom-left (south-west) y-coordinate in
C                    global domain.
C     mpiTypeXFaceBlock_xy_r4  - Primitives for communicating edge
C     mpiTypeXFaceBlock_xy_r8    of a block.
C     mpiTypeYFaceBlock_xy_r4    XFace is used in east-west transfer
C     mpiTypeYFaceBlock_xy_r8    YFace is used in nrth-south transfer
C     mpiTypeXFaceBlock_xyz_r4   xy is used in two-dimensional arrays
C     mpiTypeXFaceBlock_xyz_r8   xyz is used with three-dimensional arrays
C     mpiTypeYFaceBlock_xyz_r4   r4 is used for real*4 data
C     mpiTypeYFaceBlock_xyz_r8   r8 is used for real*8 data
C     mpiTypeXFaceThread_xy_r4  - Composites of the above primitives
C     mpiTypeXFaceThread_xy_r8    for communicating edges of all blocks
C     mpiTypeYFaceThread_xy_r4    owned by a thread.
C     mpiTypeYFaceThread_xy_r8
C     mpiTypeXFaceThread_xyz_r4
C     mpiTypeXFaceThread_xyz_r8
C     mpiTypeYFaceThread_xyz_r4
C     mpiTypeYFaceBlock_xyz_r8
C     mpiTagE       - Tags are needed to mark requests when MPI is running
C     mpiTagW         between multithreaded processes or when the same process.
C     mpiTagS         is a neighbor in more than one direction. The tags ensure that
C     mpiTagN         a thread will get the message it is looking for.
C     mpiTagSW        The scheme adopted is to tag messages according to
C     mpiTagSE        the direction they are travelling. Thus a message
C     mpiTagNW        travelling east is tagged mpiTagE. However, in a
C     mpiTagNE        multi-threaded environemnt several messages could
C                     be travelling east from the same process at the
C                     same time. The tag is therefore modified to
C                     be mpiTag[EWS...]*nThreads+myThid. This requires that
C                     each thread also know the thread ids of its "neighbor"
C                     threads.
      COMMON /EESUPP_MPI_I/
     & mpiPidW,  mpiPidE,  mpiPidS,  mpiPidN,
     & mpiPidSE, mpiPidSW, mpiPidNE, mpiPidNW,
     & mpiPidIo, mpiMyId, mpiNProcs, mpiComm,
     & mpiPx, mpiPy, mpiXGlobalLo, mpiYGlobalLo,
     & mpiTypeXFaceBlock_xy_r4, mpiTypeXFaceBlock_xy_r8,
     & mpiTypeYFaceBlock_xy_r4, mpiTypeYFaceBlock_xy_r8,
     & mpiTypeXFaceBlock_xyz_r4, mpiTypeXFaceBlock_xyz_r8,
     & mpiTypeYFaceBlock_xyz_r4, mpiTypeYFaceBlock_xyz_r8,
     & mpiTypeXFaceThread_xy_r4, mpiTypeXFaceThread_xy_r8,
     & mpiTypeYFaceThread_xy_r4, mpiTypeYFaceThread_xy_r8,
     & mpiTypeXFaceThread_xyz_r4, mpiTypeXFaceThread_xyz_r8,
     & mpiTypeYFaceThread_xyz_r4, mpiTypeYFaceThread_xyz_r8,
     & mpiTagE, mpiTagW, mpiTagN, mpiTagS,
     & mpiTagSE, mpiTagSW, mpiTagNW, mpiTagNE

      INTEGER mpiPidW
      INTEGER mpiPidE
      INTEGER mpiPidS
      INTEGER mpiPidN
      INTEGER mpiPidSW
      INTEGER mpiPidSE
      INTEGER mpiPidNW
      INTEGER mpiPidNE
      INTEGER mpiPidIO
      INTEGER mpiMyId
      INTEGER mpiNProcs
      INTEGER mpiComm
      INTEGER mpiPx
      INTEGER mpiPy
      INTEGER mpiXGlobalLo
      INTEGER mpiYGlobalLo
      INTEGER mpiTypeXFaceBlock_xy_r4
      INTEGER mpiTypeXFaceBlock_xy_r8
      INTEGER mpiTypeYFaceBlock_xy_r4
      INTEGER mpiTypeYFaceBlock_xy_r8
      INTEGER mpiTypeXFaceBlock_xyz_r4
      INTEGER mpiTypeXFaceBlock_xyz_r8
      INTEGER mpiTypeYFaceBlock_xyz_r4
      INTEGER mpiTypeYFaceBlock_xyz_r8
      INTEGER mpiTypeXFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xy_r4(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xy_r8(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER mpiTypeXFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xyz_r4(MAX_NO_THREADS)
      INTEGER mpiTypeYFaceThread_xyz_r8(MAX_NO_THREADS)
      INTEGER mpiTagNW
      INTEGER mpiTagNE
      INTEGER mpiTagSW
      INTEGER mpiTagSE
      INTEGER mpiTagW
      INTEGER mpiTagE
      INTEGER mpiTagN
      INTEGER mpiTagS
#endif /* ALLOW_USE_MPI */

