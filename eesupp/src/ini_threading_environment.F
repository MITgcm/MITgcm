C $Header: /u/gcmpack/MITgcm/eesupp/src/ini_threading_environment.F,v 1.3 1998/04/23 20:56:54 cnh Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_THREADING_ENVIRONMENT
C     /==========================================================\
C     | SUBROUTINE INI_THREADING_ENVIRONMENT                     |
C     | o Initialise multi-threaded environment.                 |
C     |==========================================================|
C     | Generally we do not start separate threads here but      |
C     | just initialise data structures indicating which of the  |
C     | nSx x nSy blocks a thread is responsible for.            |
C     | The multiple threads are spawned in the top level MAIN   |
C     | routine.                                                 |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
CEndOfInterface

C     == Local variables ==
C     bXPerThread - Blocks of size sNx per thread.
C     byPerThread - Blocks of size sNy per thread.
C     Thid        - Thread index. Temporary used in loops
C                   which set per. thread values on a 
C                   cartesian grid.
C     bxLo, bxHi  - Work vars. for thread index
C     byLo, byHi    range. bxLo is the lowest i index
C                   that a thread covers, bxHi is the
C                   highest i index. byLo is the lowest
C                   j index, byHi is the highest j index.
C     I, J        - Loop counter
C     msgBuf      - I/O buffer for reporting status information.
C     myThid      - Dummy thread id for use in printed messages
C                   ( this routine "INI_THREADING_ENVIRONMENT" is called before
C                     multi-threading has started.)
C     threadWest  - Temporaries used in calculating neighbor threads.
C     threadEast    
C     threadSouth
C     threadNorth
      INTEGER bxPerThread
      INTEGER byPerThread
      INTEGER Thid
      INTEGER bxLo, bxHi
      INTEGER byLo, byHi
      INTEGER I, J
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER myThid
      INTEGER threadWest
      INTEGER threadEast
      INTEGER threadSouth
      INTEGER threadNorth
      INTEGER threadNW  
      INTEGER threadNE  
      INTEGER threadSW    
      INTEGER threadSE    

#ifdef ALLOW_USE_MPI
C     elCount   - Length in elements of an MPI datatype
C     elStride  - Stride between elements of an MPI datatype.
C     elLen     - Length of each element of the datatype
C     arrElSize - Size in bytes of an array element
C     arrElSep  - Separation in array elements between consecutive
C                 start locations for an MPI datatype.
C     mpiRC     - MPI return code
      INTEGER elCount
      INTEGER elStride
      INTEGER elLen
      INTEGER arrElSize
      INTEGER arrElSep 
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */

C--   Set default for all threads of having no blocks to
C--   work on - except for thread 1.
      myBxLo(1) = 1
      myBxHi(1) = nSx
      myByLo(1) = 1
      myByHi(1) = nSy
      DO I = 2, MAX_NO_THREADS
       myBxLo(I) = 0
       myBxHi(I) = 1
       myByLo(I) = 0
       myByHi(I) = 1
      ENDDO
      myThid = 1

C--   If there are multiple threads allocate different range of the
C--   nSx*nSy blocks to each thread.
C     For now handle simple case of no. blocks nSx = n*nTx and
C     no. blocks nSy = m*nTy ( where m and n are integer ). This
C     is handled by simply mapping threads to blocks in sequence
C     with the x thread index moving fastest. 
C     Later code which sets the thread number of neighboring blocks 
C     needs to be consisten with the code here.
      nThreads = nTx * nTy

C--   Initialise the barrier mechanism
      CALL BARRIER_INIT

      IF   ( nThreads .NE. nTx*nTy ) THEN
       WRITE(msgBuf,'(A,A,A,I,A,I)') 
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Total number of threads is not the same as nTx*nTy.',
     &  ' nTx * nTy = ',nTx*nTy,' nThreads = ',nThreads
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      bxPerThread = nSx/nTx
      IF ( bxPerThread*nTx .NE. nSx ) THEN
       WRITE(msgBuf,'(A,A)') 
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in X (nSx) must be exact multiple of threads in X (nTx).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      byPerThread = nSy/nTy
      IF ( byPerThread*nTy .NE. nSy ) THEN
       WRITE(msgBuf,'(A,A)') 
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in Y (nSy) must be exact multiple of threads in Y (nTy).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( .NOT. eeBootError ) THEN
       byLo = 1
       DO J=1,nTy
        byHi = byLo+byPerThread-1
        bxLo = 1
        DO I=1,nTx
         Thid = (J-1)*nTx+I
         bxHi = bxLo+bxPerThread-1
         myBxLo(Thid) = bxLo
         myBxHi(Thid) = bxHi
         myByLo(Thid) = byLo
         myByHi(Thid) = byHi
         bxLo = bxHi+1
        ENDDO
        byLo = byHi+1
       ENDDO
      ENDIF

C--   Set flags saying how each thread is communicating
C     Notes:
C     ======
C     By default each block communicates with its neighbor using
C     direct reads and writes from the neighbors overlap regions. 
C     This rule will always applie for the blocks in the interior
C     of a processes domain, but for the "outside" faces of blocks on
C     the edges of the processes domain i.e. where bx=1 or nSx or
C     where by = 1 or nSy. In this section each thread checks to see
C     whether any of the blocks it is responsible for are "outside"
C     blocks and if so what communication strategy should be used.
C     to
      DO I=1, nThreads

C      1. Check for block which is on the west edge.
       commW(I) = COMM_SHARED
       IF ( notUsingXPeriodicity .AND. 
     &      myBxLo(I) .EQ. 1     .AND. 
     &           myPx .EQ. 1 ) THEN
        commW(I) = COMM_NONE
       ELSEIF ( myBxLo(I) .EQ. 1 ) THEN
#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
         IF ( mpiPidW .NE. MPI_PROC_NULL ) THEN
          commW(I) = COMM_MPI
          allMyEdgesAreSharedMemory(I) = .FALSE.
         ENDIF
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /* ALLOW_USE_MPI */
       ENDIF

C      2. Check for block which is on the east edge.
       commE(I) = COMM_SHARED
       IF ( notUsingXPeriodicity .AND. 
     &      myBxHi(I) .EQ. nSx   .AND. 
     &      myPx .EQ. nPx ) THEN
        commE(I) = COMM_NONE  
       ELSEIF ( myBxHi(I) .EQ. nSx ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
         IF ( mpiPidE .NE. MPI_PROC_NULL ) THEN
          commE(I) = COMM_MPI
          allMyEdgesAreSharedMemory(I) = .FALSE.
         ENDIF
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /* ALLOW_USE_MPI */
       ENDIF

C      3. Check for block which is southern edge
       commS(I) = COMM_SHARED
       IF ( notUsingYPeriodicity .AND. 
     &      myByLo(I) .EQ.   1   .AND. 
     &           myPy .EQ.   1 ) THEN
        commS(I) = COMM_NONE
       ELSEIF ( myByLo(I) .EQ. 1 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
         IF ( mpiPidS .NE. MPI_PROC_NULL ) THEN
          commS(I) = COMM_MPI
          allMyEdgesAreSharedMemory(I) = .FALSE.
         ENDIF
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /* ALLOW_USE_MPI */
       ENDIF

C      4. Check for block which is on northern edge
       commN(I) = COMM_SHARED
       IF ( notUsingYPeriodicity .AND. 
     &        myByHi(I) .EQ. nSy .AND. 
     &             myPy .EQ. nPy ) THEN
        commN(I) = COMM_NONE
       ELSEIF ( myByHi(I) .EQ. nSy ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
        IF ( usingMPI ) THEN
#endif
         IF ( mpiPidN .NE. MPI_PROC_NULL ) THEN
          commN(I) = COMM_MPI
          allMyEdgesAreSharedMemory(I) = .FALSE.
         ENDIF
#ifndef ALWAYS_USE_MPI
        ENDIF
#endif
#endif /* ALLOW_USE_MPI */
       ENDIF
      ENDDO

C--   Print mapping of threads to grid points.
      WRITE(msgBuf,'(A)') '// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '// Mapping of tiles to threads'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)') '// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      DO I=1,nThreads
       WRITE(msgBuf,'(A,I4,A,4(I4,A1))') 
     & '// -o- Thread',I,', tiles (',
     & myBxLo(I),':',myBxHi(I),',',myByLo(I),':',myByHi(I),')'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_BOTH , 1)
       IF ( myBxLo(I) .NE. 1 .OR.
     &      commW(I) .EQ. COMM_SHARED ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' shared memory to west.'
       ELSEIF ( commW(I) .NE. COMM_NONE ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' messages to west.'
       ELSE
        WRITE(msgBuf,'(A,A)')  '//',' no communication to west.'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
       IF ( myBxHi(I) .NE. nSx .OR.
     &      commE(I) .EQ. COMM_SHARED ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' shared memory to east.'
       ELSEIF ( commE(I) .NE. COMM_NONE ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' messages to east.'
       ELSE
        WRITE(msgBuf,'(A,A)')  '//',' no communication to east.'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
       IF ( myByLo(I) .NE. 1 .OR.
     &      commS(I) .EQ. COMM_SHARED ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' shared memory to south.'
       ELSEIF ( commS(I) .NE. COMM_NONE ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' messages to south.'
       ELSE
        WRITE(msgBuf,'(A,A)')  '//',' no communication to south.'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
       IF ( myByHi(I) .NE. nSy .OR.
     &      commN(I) .EQ. COMM_SHARED ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' shared memory to north.'
       ELSEIF ( commN(I) .NE. COMM_NONE ) THEN
        WRITE(msgBuf,'(A,A)')  '//',' messages to north.'
       ELSE
        WRITE(msgBuf,'(A,A)')  '//',' no communication to north.'
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      ENDDO
      WRITE(msgBuf,'(A)')  ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)

#ifdef ALLOW_USE_MPI
C--   Create MPI datatypes for communicating thread boundaries if needed
C     For every thread we define 8 MPI datatypes for use
C     in indicating regions of data to transfer as follows:
C     o mpiTypeXFaceThread_xy_r4
C       Handles east-west transfer for XY arrays of REAL*4
C     o mpiTypeXFaceThread_xy_r8
C       Handles east-west transfer for XY arrays of REAL*8
C     o mpiTypeYFaceThread_xy_r4
C       Handles north-south transfer for XY arrays of REAL*4
C     o mpiTypeYFaceThread_xy_r8
C       Handles north-south transfer for XY arrays of REAL*8
C     o mpiTypeXFaceThread_xyz_r4
C       Handles east-west transfer for XYZ arrays of REAL*4
C     o mpiTypeXFaceThread_xyz_r8
C       Handles east-west transfer for XYZ arrays of REAL*8
C     o mpiTypeYFaceThread_xyz_r4
C       Handles north-south transfer for XYZ arrays of REAL*4
C     o mpiTypeYFaceThread_xyz_r8
C       Handles north-south transfer for XYZarrays of REAL*8
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
       DO I =1, nThreads

C       x-face exchanges for xy real*4 data
        elCount   = myByHi(I)-myByLo(I)+1
        elLen     = 1
        arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)*nSx
        arrElSize = 4
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeXFaceBlock_xy_r4,
     O                        mpiTypeXFaceThread_xy_r4(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeXFaceThread_xy_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeXFaceThread_xy_r4(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeXFaceThread_xy_r4)',
     &         mpiRC 
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF

C       x-face exchanges for xy real*8 data
        arrElSize = 8
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeXFaceBlock_xy_r8,
     O                        mpiTypeXFaceThread_xy_r8(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeXFaceThread_xy_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeXFaceThread_xy_r8(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeXFaceThread_xy_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF

C       x-face exchanges for xyz real*4 data
        elCount   = myByHi(I)-myByLo(I)+1
        elLen     = 1
        arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)*Nz*nSx
        arrElSize = 4
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeXFaceBlock_xyz_r4,
     O                        mpiTypeXFaceThread_xyz_r4(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeXFaceThread_xyz_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeXFaceThread_xyz_r4(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeXFaceThread_xyz_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
 
C       x-face exchanges for xyz real*8 data
        arrElSize = 8
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeXFaceBlock_xyz_r8,
     O                        mpiTypeXFaceThread_xyz_r8(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeXFaceThread_xyz_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeXFaceThread_xyz_r8(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeXFaceThread_xyz_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF

C       y-face exchages for xy real*4 data
        elCount   = myBxHi(I)-myBxLo(I)+1
        elLen     = 1
        arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)
        arrElSize = 4
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeYFaceBlock_xy_r4,
     O                        mpiTypeYFaceThread_xy_r4(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeYFaceThread_xy_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeYFaceThread_xy_r4(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeYFaceThread_xy_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
       
C       y-face exchages for xy real*8 data
        arrElSize = 8
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeYFaceBlock_xy_r8,
     O                        mpiTypeYFaceThread_xy_r8(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeYFaceThread_xy_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeYFaceThread_xy_r8(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeYFaceThread_xy_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF

C       y-face exchages for xyz real*4 data
        elCount   = myBxHi(I)-myBxLo(I)+1
        elLen     = 1
        arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)*Nz
        arrElSize = 4
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeYFaceBlock_xyz_r4,
     O                        mpiTypeYFaceThread_xyz_r4(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeYFaceThread_xyz_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeYFaceThread_xyz_r4(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeYFaceThread_xyz_r4)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
       
C       y-face exchages for xy real*8 data
        arrElSize = 8
        elStride  = arrElSep*arrElSize
        CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,mpiTypeYFaceBlock_xyz_r8,
     O                        mpiTypeYFaceThread_xyz_r8(I), mpiRC )
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_HVECTOR (mpiTypeYFaceThread_xyz_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF
        CALL MPI_TYPE_COMMIT(mpiTypeYFaceThread_xyz_r8(I),mpiRC)
        IF ( mpiRC .NE. MPI_SUCCESS ) THEN
         eeBootError = .TRUE.
         WRITE(msgBuf,'(A,I)')
     &         'S/R INI_THREADS: MPI_TYPE_COMMIT (mpiTypeYFaceThread_xyz_r8)',
     &         mpiRC
         CALL PRINT_ERROR( msgBuf , myThid)
        ENDIF

       ENDDO

#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

C--   Calculate the thread numbers of the threads I might want to "message"
C     Notes:
C     1. This code needs to be consistent with the code that maps threads to
C        blocks earlier in this routine in which threads are arranged
C       13 14 15 16 /|\
C        9 10 11 12  | nTy
C        5  6  7  8  |
C        1  2  3  4 \|/
C       <---- nTx --->
C        on equally sized collections of sNx x sNy sub-blocks.
      DO I = 1, nThreads
C      Find thread to west, east, south, north using wrap around for
C      threads managing "outside" blocks.
       threadWest  = I-1
       IF ( myBxLo(I) .EQ. 1   ) threadWest  = I+nTx-1
       threadEast  = I+1
       IF ( myBxHi(I) .EQ. nSx ) threadEast  = I-nTx+1
       threadSouth = I-nTx
       IF ( myByLo(I) .EQ. 1   ) threadSouth = I+nTx*(nTy-1)
       threadNorth = I+nTx
       IF ( myByHi(I) .EQ. nSy ) threadNorth = I-nTx*(nTy-1)
C      Find thread to NW, NE, SW, SE - again with wrap around.
       threadNW    = threadWest+nTx
       IF ( myByHi(threadWest) .EQ. nSy ) threadNW = threadWest-nTx*(nTy-1)
       threadNE    = threadEast+nTx
       IF ( myByHi(threadEast) .EQ. nSy ) threadNE = threadEast-nTx*(nTy-1)
       threadSW    = threadWest-nTx
       IF ( myByHi(threadWest) .EQ. 1   ) threadSW = threadWest+nTx*(nTy-1)
       threadSE    = threadEast-nTx
       IF ( myByHi(threadEast) .EQ. 1   ) threadSE = threadEast+nTx*(nTy-1)
       myThrW(I)   = threadWest
       myThrE(I)   = threadEast
       myThrN(I)   = threadNorth
       myThrS(I)   = threadSouth
       myThrNW(I)  = threadNW
       myThrNE(I)  = threadNE
       myThrSW(I)  = threadSW 
       myThrSE(I)  = threadSE
      ENDDO

      RETURN
      END
