C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch.F,v 1.4 1998/06/15 05:13:55 cnh Exp $

#include "CPP_EEOPTIONS.h"

C--   File exch.F: Routines that perform atomic communication operations 
C                  i.e. communication operations that are completed by
C                  time the routines return. 
C                  Note
C                  ====
C                  The code in here although intricate is fairly 
C                  straightforward. There are four routines, one
C                  for each of the data patterns we wish to do overlap
C                  updates on - as listed below. Each routine has two
C                  parts. The first part does overlap updates to and from
C                  remote "processes", that is processes who do not truly
C                  share the address space of this process. This part
C                  requires a facility like MPI or CRAY shmem get and put
C                  operations. It is possible to switch this part on or
C                  off using compiler directives. In the case of a simple
C                  serial execution nothing happens in this part. This part
C                  is also switched off when communication strategies
C                  that allow the overlapping of communication and computation 
C                  are employed.
C                  The second part of each routine does the true shared memory 
C                  overlap copying i.e. copying from one part of array phi
C                  to another part. This part is always active, in the case
C                  of a single threaded messaging code, however, this part 
C                  will not do any copies as all edges will be flagged as using
C                  for example MPI or SHMPUT, SHMGET.
C      Contents
C      o exch_xy_r4   - Does overlap updates for REAL*4 2d XY fields
C      o exch_xy_r8   - Does overlap updates for REAL*8 2d XY fields
C      o exch_xyz_r4  - Does overlap updates for REAL*4 3d XYZ fields
C      o exch_xyz_r8  - Does overlap updates for REAL*8 3d XYZ fields

CStartOfInterface
      SUBROUTINE EXCH_XY_R4( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XY_R4                                    |
C     | o Handle exchanges for real*4, two-dimensional arrays.   |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a two-dimensional array with overlaps.        |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | If it is used, "asynchronous" messaging in which         |
C     | communication overlaps computation is handled elsewhere  |
C     | - see recv.F and send.F.                                 |
C     | In practice MPI implementations may not be completely    |
C     | thread safe. In principle this code is correct even for  |
C     | mixed MPI and multi-threaded execution.                  |
C     | In multi-thread execution data managed by other threads  |
C     | is only automatically visible to another thread if it    |
C     | existed before the thread was created. This means that   |
C     | all Fortran we declare arrays with overlap regions in    |
C     | COMMON blocks.                                           |
C     \==========================================================/

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
C     myThid - My thread id.
      Real*4 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     bi,bj  - Outer loop counters
C     I,J,K  - Inner loop counters
      INTEGER bi, bj
      INTEGER I, J
#ifdef    ALLOW_USE_MPI
C     tagSend - Tags used to mark and select messages
C     tagRecv
C     nReqs  - MPI request counter.
C     reqIds - Outstanding requests to wait on.
C     mpiRC  - MPI return code
C     mpiStatArr - Multi-request status reporting array.
C     toPid      - Proc. id sending to
C     fromPid    - Proc. id receving from
C     elCount    - Number of items to send or receive
C     elType     - Datat type of elements
      INTEGER tagSend
      INTEGER tagRecv
      INTEGER nReqs
      INTEGER reqIds(4)
      INTEGER mpiRC
      INTEGER mpiStatArr(MPI_STATUS_SIZE,4)
      INTEGER toPid
      INTEGER fromPid
      INTEGER elCount
      INTEGER elType
#endif /* ALLOW_USE_MPI */
C     bE, bW - Block index to east, west, north and
C     bN, bS   south.
      INTEGER bW, bE, bS, bN

C--   Do "message-passing" data exchanges
#ifdef  ALLOW_SYNC_COMMUNICATION
#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      IF ( usingSyncMessages ) THEN
#endif

C--    MPI based data exchages
#ifdef    ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif

C       Here use the approach that all the X shifts are completed
C       before the Y shifts start. Then we can expand our Y shifts
C       to include X overlap regions. This means we don't have to
C       transfer corners separately. 
C       Notes:
C       ======
C       1. If we are only using messages in Y and using "true shared-memory"
C       in X then we rely on the shared-memory code to be implemented so that
C       corner values will be set correctly. The true shared-memory code
C       is in the block below - so be careful when altering it!
C       2. In order to ensure we grab the right message we need to tag
C       messages. We tag the messages with the direction in which they
C       were sent out. Thus to receive from a processor to our west we
C       request the next message from that processor that was sent out 
C       with a tag indicating an easterly send.
C       3. We could implement a "safe" code here by having everyone
C       sync and then getting a particular thread ( say thread 1 ) to
C       do the MPI_Isend sequentially. In this mode the "communication"
C       thread would loop over nThreads and do MPI_Sendrecv for each 
C       one. This could be more efficient on some platforms as the messages
C       might be sent as one large unit rather than by separate threads.
C       It would also be "thread-safe" for MPI implementations that have
C       multi-threading problems with things like request ids.
C       4. We include overlap regions in both X and Y sends.
C       This means we can interchange the Y block with the X block.
C       Not sure whether this will ever be useful!
C       5. The generation of a request handle by MPI_Isend and
C       MPI_Irecv ouught to involve some global state within MPI.
C       If this process is not thread safe it may be necessary to
C       enable a critical section around the MPI_Isend and
C       MPI_Irecv calls.

C      We need a barrier here to synchronise threads otherwise 
C      one thread might declare it has is ready to send and receive
C      even though another thread is going to write to the first
C      threads send or receive region. This won't happen, however,
C      if we are careful about which data a thread updates - but at
C      the moment we aren't careful!
       _BARRIER

C--     East-west messaging communication
        nReqs = 0
        bE = myBxLo(myThid)
        IF ( bE .EQ. 1 .AND. commW(myThid) .EQ. COMM_MPI ) THEN
C        My west face uses MPI. Get ready to receive data from 
C        west and post that we are ready to send data to the west.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id
C        and the send direction.
         nReqs   = nReqs+1
         tagSend = mpiTagW*nThreads+myThid
         toPid   = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_r4(myThid)
C    I                  tagSend,MPI_COMM_WORLD,
         CALL MPI_Isend(
     I                  phi(1,1-OLy,bE,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagE*nThreads+myThrW(myThid)
         fromPid = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_r4(myThid)
         CALL MPI_Irecv(
     U                  phi(1-OLx,1-OLy,bE,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
        bW = myBxHi(myThid)
        IF ( bW .EQ. nSx .AND. commE(MyThid) .EQ. COMM_MPI ) THEN
C        My east face uses MPI. Get ready to receive data from 
C        east and post that we are ready to send data to the east.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id.
         nReqs = nReqs+1
         tagSend = mpiTagE*nThreads+myThid
         toPid   = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_r4(myThid)
         CALL MPI_Isend(
     I                  phi(sNx-OLx+1,1-OLy,bW,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagW*nThreads+myThrE(myThid)
         fromPid = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_r4(myThid)
         CALL MPI_Irecv(
     U                  phi(sNx+1,1-OLy,bW,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
C       Wait for this threads east-west transactions to finish before
C       posting north-south transactions. We have to do this so that
C       the north-south transactions will send out the correct overlap
C       region values into the corner sections of our neighbors.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

CcnhDebugStarts
        RETURN
CcnhDebugEnds

C--     North-south messaging communication
        nReqs = 0
        bN = myByLo(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My south face uses MPI. Get ready to receive data from
C        south and post that I am ready to send data to the south.
         nReqs = nReqs + 1
         tagSend = mpiTagS*nThreads+myThid
         toPid   = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_r4(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,1,myBxLo(myThid),bN), 
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagN*nThreads+myThrS(myThid)
         fromPid = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_r4(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,1-OLy,myBxLo(myThid),bN),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C
        bS = myByHi(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My north face uses MPI. Get ready to receive data from
C        north and post that I am ready to send data to the north.
         nReqs = nReqs + 1
         tagSend = mpiTagN*nThreads+myThid
         toPid   = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_r4(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,sNy-OLy+1,myBxLo(myThid),bS),
     I                  elCount, elType, toPid,                      
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagS*nThreads+myThrN(myThid)
         fromPid = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_r4(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,sNy+1,myBxLo(myThid),bS),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C       Wait for this threads north-south transactions to finish.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

#ifndef ALWAYS_USE_MPI 
       ENDIF 
#endif
#endif /* ALLOW_USE_MPI */

#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      ENDIF
#endif
#endif /* ALLOW_SYNC_COMMUNICATION */

C--   Do true shared-memory data exchanges
C     Note this is also doing the edge copies for a regular serial
C     code.
C--   First make sure all threads have reached here
      _BARRIER

C--   Now do copies
C     Notes:
C     ======
C     Here we copy from up to and including the overlap
C     regions for both the X shift and the Y shift. This
C     catches the "corners" of the data copied using messages
C     in the "synchronous communication" section above.
C     As coded here we also write to a remote region in
C     one direction. In some situations it may be better only
C     to write to our own overlaps - could be the case with
C     Wildfire replication. This should be chamged once everything 
C     is working OK.
C    
C--   x-axis exchanges
      bE = myBxLo(myThid)
      IF ( bE .NE. 1 .OR.
     &     commW(myThid) .EQ. COMM_SHARED ) THEN
       bW = bE-1
       IF ( bW .LT. 1 ) bW = nSx
       DO bj=myByLo(myThid),myByHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1,OLx
          phi(sNx+I,J,bW,bj)=phi(  1+I-1,J,bE,bj)
          phi(1-I  ,J,bE,bj)=phi(sNx-I+1,J,bW,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bE=myBxLo(myThid)+1,myBxHi(myThid)
       bW = bE-1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1,OLx
          phi(sNx+I,J,bW,bj)=phi(  1+I-1,J,bE,bj)
          phi(1-I  ,J,bE,bj)=phi(sNx-I+1,J,bW,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Need to ensure all threads have completed x-axis transfers before
C     we can do y-axis exchanges.
      _BARRIER

C--   y-axis exchanges
      bN = myByLo(myThid)
      IF ( bN .NE. 1 .OR.
     &     commS(myThid) .EQ. COMM_SHARED ) THEN
       bS = bN - 1
       IF ( bS .LT. 1 ) bS = nSy
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,OLy
         DO I=1-OLx,sNx+OLx
          phi(I,1-J    ,bi,bN)=phi(I,sNy-J+1,bi,bS)
          phi(I,sNy+J  ,bi,bS)=phi(I,1+J-1  ,bi,bN)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bN=myByLo(myThid)+1,myByHi(myThid)
       bS = bN - 1
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,OLy
         DO I=1-OLx,sNx+OLx
          phi(I,1-J    ,bi,bN)=phi(I,sNy-J+1,bi,bS)
          phi(I,sNy+J  ,bi,bS)=phi(I,1+J-1  ,bi,bN)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XYZ_R4( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XYZ_R4                                   |
C     | o Handle exchanges for real*4, three-dimensional arrays. |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a three-dimensional array with overlaps.      |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | If it is used, "asynchronous" messaging in which         |
C     | communication overlaps computation is handled elsewhere  |
C     | - see recv.F and send.F.                                 |
C     | In practice MPI implementations may not be completely    |
C     | thread safe. In principle this code is correct even for  |
C     | mixed MPI and multi-threaded execution.                  |
C     | In multi-thread execution data managed by other threads  |
C     | is only automatically visible to another thread if it    |
C     | existed before the thread was created. This means that   |
C     | for Fortran we declare arrays with overlap regions in    |
C     | COMMON blocks.                                           |
C     \==========================================================/

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
C     myThid - My thread id.
      Real*4 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     bi,bj  - Outer loop counters
C     I,J,K  - Inner loop counters
      INTEGER bi, bj
      INTEGER I, J, K
#ifdef    ALLOW_USE_MPI
C     tagSend - Tags used to mark and select messages
C     tagRecv
C     nReqs  - MPI request counter.
C     reqIds - Outstanding requests to wait on.
C     mpiRC  - MPI return code
C     mpiStatArr - Multi-request status reporting array.
C     toPid      - Proc. id sending to
C     fromPid    - Proc. id receving from
C     elCount    - Number of items to send or receive
C     elType     - Data type of elements
      INTEGER tagSend
      INTEGER tagRecv
      INTEGER nReqs
      INTEGER reqIds(4)
      INTEGER mpiRC
      INTEGER mpiStatArr(MPI_STATUS_SIZE,4)
      INTEGER toPid
      INTEGER fromPid
      INTEGER elCount
      INTEGER elType
#endif /* ALLOW_USE_MPI */
C     bE, bW - Block index to east, west, north and
C     bN, bS   south.
      INTEGER bW, bE, bS, bN

C--   Do "message-passing" data exchanges
#ifdef  ALLOW_SYNC_COMMUNICATION
#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      IF ( usingSyncMessages ) THEN
#endif

C--    MPI based data exchages
#ifdef    ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif

C       Here use the approach that all the X shifts are completed
C       before the Y shifts start. Then we can expand our Y shifts
C       to include X overlap regions. This means we don't have to
C       transfer corners separately. 
C       Notes:
C       ======
C       1. If we are only using messages in Y and using "true shared-memory"
C       in X then we rely on the shared-memory code to be implemented so that
C       corner values will be set correctly. The true shared-memory code
C       is in the block below - so be careful when altering it!
C       2. In order to ensure we grab the right message we need to tag
C       messages. We tag the messages with the direction in which they
C       were sent out. Thus to receive from a processor to our west we
C       request the next message from that processor that was sent out 
C       with a tag indicating an easterly send.
C       3. We could implement a "safe" code here by having everyone
C       sync and then getting a particular thread ( say thread 1 ) to
C       do the MPI_Isend sequentially. In this mode the "communication"
C       thread would loop over nThreads and do MPI_Sendrecv for each 
C       one. This could be more efficient on some platforms as the messages
C       might be sent as one large unit rather than by separate threads.
C       It would also be "thread-safe" for MPI implementations that have
C       multi-threading problems with things like request ids.
C       4. We include overlap regions in both X and Y sends.
C       This means we can interchange the Y block with the X block.
C       Not sure whether this will ever be useful!
C       5. The generation of a request handle by MPI_Isend and
C       MPI_Irecv ouught to involve some global state within MPI.
C       If this process is not thread safe it may be necessary to
C       enable a critical section around the MPI_Isend and
C       MPI_Irecv calls.

C       We need a barrier here to synchronise threads otherwise 
C       one thread might declare it has is ready to send and receive
C       even though another thread is going to write to the first
C       threads send or receive region. This won't happen, however,
C       if we are careful about which data a thread updates - but at
C       the moment we aren't careful!
        _BARRIER
 
C--     East-west messaging communication
        nReqs = 0
        bE = myBxLo(myThid)
        IF ( bE .EQ. 1 .AND. commW(myThid) .EQ. COMM_MPI ) THEN
C        My west face uses MPI. Get ready to receive data from 
C        west and post that we are ready to send data to the west.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id
C        and the send direction.
         nReqs   = nReqs+1
         tagSend = mpiTagW*nThreads+myThid
         toPid   = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xyz_r4(myThid)
         CALL MPI_Isend(
     I                  phi(1,1-OLy,1,bE,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagE*nThreads+myThrW(myThid)
         fromPid = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xyz_r4(myThid)
         CALL MPI_Irecv(
     U                  phi(1-OLx,1-OLy,1,bE,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
        bW = myBxHi(myThid)
        IF ( bW .EQ. nSx .AND. commE(MyThid) .EQ. COMM_MPI ) THEN
C        My east face uses MPI. Get ready to receive data from 
C        east and post that we are ready to send data to the east.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id.
         nReqs = nReqs+1
         tagSend = mpiTagE*nThreads+myThid
         toPid   = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xyz_r4(myThid)
         CALL MPI_Isend(
     I                  phi(sNx-OLx+1,1-OLy,1,bW,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagW*nThreads+myThrE(myThid)
         fromPid = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xyz_r4(myThid)
         CALL MPI_Irecv(
     U                  phi(sNx+1,1-OLy,1,bW,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
C       Wait for this threads east-west transactions to finish before
C       posting north-south transactions. We have to do this so that
C       the north-south transactions will send out the correct overlap
C       region values into the corner sections of our neighbors.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )
CcnhDebugStarts
        RETURN
CcnhDebugEnds

 
C--     North-south messaging communication
        nReqs = 0
        bN = myByLo(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My south face uses MPI. Get ready to receive data from
C        south and post that I am ready to send data to the south.
         nReqs = nReqs + 1
         tagSend = mpiTagS*nThreads+myThid
         toPid   = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xyz_r4(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,1,1,myBxLo(myThid),bN), 
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagN*nThreads+myThrS(myThid)
         fromPid = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xyz_r4(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,1-OLy,1,myBxLo(myThid),bN),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C
        bS = myByHi(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My north face uses MPI. Get ready to receive data from
C        north and post that I am ready to send data to the north.
         nReqs = nReqs + 1
         tagSend = mpiTagN*nThreads+myThid
         toPid   = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xyz_r4(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,sNy-OLy+1,1,myBxLo(myThid),bS),
     I                  elCount, elType, toPid,                      
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagS*nThreads+myThrN(myThid)
         fromPid = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xyz_r4(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,sNy+1,1,myBxLo(myThid),bS),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C       Wait for this threads north-south transactions to finish.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

#ifndef ALWAYS_USE_MPI 
       ENDIF 
#endif
#endif /* ALLOW_USE_MPI */

#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      ENDIF
#endif
#endif /* ALLOW_SYNC_COMMUNICATION */

C--   Do true shared-memory data exchanges
C     Note: This is also doing the overlap copies 
C           for a single threaded code.
C--   First make sure all threads have reached here
      _BARRIER

C--   Now do copies
C     Notes:
C     ======
C     Here we copy from up to and including the overlap
C     regions for both the X shift and the Y shift. This
C     catches the "corners" of the data copied using messages
C     in the "synchronous communication" section above.
C--   x-axis exchanges
      bE = myBxLo(myThid)
      IF ( bE .NE. 1 .OR.
     &     commW(myThid) .EQ. COMM_SHARED ) THEN
       bW = bE-1
       IF ( bW .LT. 1 ) bW = nSx
       DO bj=myByLo(myThid),myByHi(myThid)
        DO K=1,Nz
         DO J=1-OLy,sNy+OLy
          DO I=1,OLx
           phi(sNx+I,J,K,bW,bj)=phi(  1+I-1,J,K,bE,bj)
           phi(1-I  ,J,K,bE,bj)=phi(sNx-I+1,J,K,bW,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bE=myBxLo(myThid)+1,myBxHi(myThid)
       bW = bE-1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO K=1,Nz
         DO J=1-OLy,sNy+OLy
          DO I=1,OLx
           phi(sNx+I,J,K,bW,bj)=phi(  1+I-1,J,K,bE,bj)
           phi(1-I  ,J,K,bE,bj)=phi(sNx-I+1,J,K,bW,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Need to ensure all threads have completed x-axis transfers before
C     we can do y-axis exchanges.
      _BARRIER

C--   y-axis exchanges
      bN = myByLo(myThid)
      IF ( bN .NE. 1 .OR.
     &     commS(myThid) .EQ. COMM_SHARED ) THEN
       bS = bN - 1
       IF ( bS .LT. 1 ) bS = nSy
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nz
         DO J=1,OLy
          DO I=1-OLx,sNx+OLx
           phi(I,1-J    ,K,bi,bN)=phi(I,sNy-J+1,K,bi,bS)
           phi(I,sNy+J  ,K,bi,bS)=phi(I,1+J-1  ,K,bi,bN)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bN=myByLo(myThid)+1,myByHi(myThid)
       bS = bN - 1
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nz
         DO J=1,OLy
          DO I=1-OLx,sNx+OLx
           phi(I,1-J    ,K,bi,bN)=phi(I,sNy-J+1,K,bi,bS)
           phi(I,sNy+J  ,K,bi,bS)=phi(I,1+J-1  ,K,bi,bN)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XY_R8( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XY_R8                                    |
C     | o Handle exchanges for real*8, two-dimensional arrays.   |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a two-dimensional array with overlaps.        |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | If it is used, "asynchronous" messaging in which         |
C     | communication overlaps computation is handled elsewhere  |
C     | - see recv.F and send.F.                                 |
C     | In practice MPI implementations may not be completely    |
C     | thread safe. In principle this code is correct even for  |
C     | mixed MPI and multi-threaded execution.                  |
C     | In multi-thread execution data managed by other threads  |
C     | is only automatically visible to another thread if it    |
C     | existed before the thread was created. This means that   |
C     | all Fortran we declare arrays with overlap regions in    |
C     | COMMON blocks.                                           |
C     \==========================================================/

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
C     myThid - My thread id.
      Real*8 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     bi,bj  - Outer loop counters
C     I,J,K  - Inner loop counters
      INTEGER bi, bj
      INTEGER I, J
#ifdef    ALLOW_USE_MPI
C     tagSend - Tags used to mark and select messages
C     tagRecv
C     nReqs  - MPI request counter.
C     reqIds - Outstanding requests to wait on.
C     mpiRC  - MPI return code
C     mpiStatArr - Multi-request status reporting array.
C     toPid      - Proc. id sending to
C     fromPid    - Proc. id receving from
C     elCount    - Number of items to send or receive
C     elType     - Datat type of elements
      INTEGER tagSend
      INTEGER tagRecv
      INTEGER nReqs
      INTEGER reqIds(4)
      INTEGER mpiRC
      INTEGER mpiStatArr(MPI_STATUS_SIZE,4)
      INTEGER toPid
      INTEGER fromPid
      INTEGER elCount
      INTEGER elType
#endif /* ALLOW_USE_MPI */
C     bE, bW - Block index to east, west, north and
C     bN, bS   south.
      INTEGER bW, bE, bS, bN

C--   Do "message-passing" data exchanges
#ifdef  ALLOW_SYNC_COMMUNICATION
#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      IF ( usingSyncMessages ) THEN
#endif

C--    MPI based data exchages
#ifdef    ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif

C       Here use the approach that all the X shifts are completed
C       before the Y shifts start. Then we can expand our Y shifts
C       to include X overlap regions. This means we don't have to
C       transfer corners separately. 
C       Notes:
C       ======
C       1. If we are only using messages in Y and using "true shared-memory"
C       in X then we rely on the shared-memory code to be implemented so that
C       corner values will be set correctly. The true shared-memory code
C       is in the block below - so be careful when altering it!
C       2. In order to ensure we grab the right message we need to tag
C       messages. We tag the messages with the direction in which they
C       were sent out. Thus to receive from a processor to our west we
C       request the next message from that processor that was sent out 
C       with a tag indicating an easterly send.
C       3. We could implement a "safe" code here by having everyone
C       sync and then getting a particular thread ( say thread 1 ) to
C       do the MPI_Isend sequentially. In this mode the "communication"
C       thread would loop over nThreads and do MPI_Sendrecv for each 
C       one. This could be more efficient on some platforms as the messages
C       might be sent as one large unit rather than by separate threads.
C       It would also be "thread-safe" for MPI implementations that have
C       multi-threading problems with things like request ids.
C       4. We include overlap regions in both X and Y sends.
C       This means we can interchange the Y block with the X block.
C       Not sure whether this will ever be useful!
C       5. The generation of a request handle by MPI_Isend and
C       MPI_Irecv ouught to involve some global state within MPI.
C       If this process is not thread safe it may be necessary to
C       enable a critical section around the MPI_Isend and
C       MPI_Irecv calls.

C      We need a barrier here to synchronise threads otherwise 
C      one thread might declare it has is ready to send and receive
C      even though another thread is going to write to the first
C      threads send or receive region. This won't happen, however,
C      if we are careful about which data a thread updates - but at
C      the moment we aren't careful!
       _BARRIER

C--     East-west messaging communication
        nReqs = 0
        bE = myBxLo(myThid)
        IF ( bE .EQ. 1 .AND. commW(myThid) .EQ. COMM_MPI ) THEN
C        My west face uses MPI. Get ready to receive data from 
C        west and post that we are ready to send data to the west.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id
C        and the send direction.
         nReqs   = nReqs+1
         tagSend = mpiTagW*nThreads+myThid
         toPid   = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_R8(myThid)
C    I                  tagSend,MPI_COMM_WORLD,
         CALL MPI_Isend(
     I                  phi(1,1-OLy,bE,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagE*nThreads+myThrW(myThid)
         fromPid = mpiPidW
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_R8(myThid)
         CALL MPI_Irecv(
     U                  phi(1-OLx,1-OLy,bE,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
        bW = myBxHi(myThid)
        IF ( bW .EQ. nSx .AND. commE(MyThid) .EQ. COMM_MPI ) THEN
C        My east face uses MPI. Get ready to receive data from 
C        east and post that we are ready to send data to the east.
C        Data we receive has to come with tag of thread we know is
C        sending from our west, data we send is tagged with our thread id.
         nReqs = nReqs+1
         tagSend = mpiTagE*nThreads+myThid
         toPid   = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_R8(myThid)
         CALL MPI_Isend(
     I                  phi(sNx-OLx+1,1-OLy,bW,myByLo(myThid)),
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
         nReqs = nReqs+1
         tagRecv = mpiTagW*nThreads+myThrE(myThid)
         fromPid = mpiPidE
         elCount = 1
         elType  = mpiTypeXFaceThread_xy_R8(myThid)
         CALL MPI_Irecv(
     U                  phi(sNx+1,1-OLy,bW,myByLo(myThid)),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs),mpiRC)
        ENDIF
C       Wait for this threads east-west transactions to finish before
C       posting north-south transactions. We have to do this so that
C       the north-south transactions will send out the correct overlap
C       region values into the corner sections of our neighbors.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

CcnhDebugStarts
        RETURN
CcnhDebugEnds


C--     North-south messaging communication
        nReqs = 0
        bN = myByLo(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My south face uses MPI. Get ready to receive data from
C        south and post that I am ready to send data to the south.
         nReqs = nReqs + 1
         tagSend = mpiTagS*nThreads+myThid
         toPid   = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_R8(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,1,myBxLo(myThid),bN), 
     I                  elCount, elType, toPid,
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagN*nThreads+myThrS(myThid)
         fromPid = mpiPidS
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_R8(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,1-OLy,myBxLo(myThid),bN),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C
        bS = myByHi(myTHid)
        IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C        My north face uses MPI. Get ready to receive data from
C        north and post that I am ready to send data to the north.
         nReqs = nReqs + 1
         tagSend = mpiTagN*nThreads+myThid
         toPid   = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_R8(myThid)
         CALL MPI_Isend(
     I                  phi(1-OLx,sNy-OLy+1,myBxLo(myThid),bS),
     I                  elCount, elType, toPid,                      
     I                  tagSend,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
         nReqs = nReqs + 1
         tagRecv = mpiTagS*nThreads+myThrN(myThid)
         fromPid = mpiPidN
         elCount = 1
         elType  = mpiTypeYFaceThread_xy_R8(myThid)
         CALL MPI_Irecv(
     I                  phi(1-OLx,sNy+1,myBxLo(myThid),bS),
     I                  elCount, elType, fromPid,
     I                  tagRecv,MPI_COMM_WORLD,
     I                  reqIds(nReqs), mpiRC )
        ENDIF
C       Wait for this threads north-south transactions to finish.
        CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

#ifndef ALWAYS_USE_MPI 
       ENDIF 
#endif
#endif /* ALLOW_USE_MPI */

#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      ENDIF
#endif
#endif /* ALLOW_SYNC_COMMUNICATION */

C--   Do true shared-memory data exchanges
C     Note: This section also does the overlap copies for serial
C           code overlap copies.
C--   First make sure all threads have reached here
      _BARRIER

C--   Now do copies
C     Notes:
C     ======
C     Here we copy from up to and including the overlap
C     regions for both the X shift and the Y shift. This
C     catches the "corners" of the data copied using messages
C     in the "synchronous communication" section above.
C--   x-axis exchanges
      bE = myBxLo(myThid)
      IF ( bE .NE. 1 .OR.
     &     commW(myThid) .EQ. COMM_SHARED ) THEN
       bW = bE-1
       IF ( bW .LT. 1 ) bW = nSx
       DO bj=myByLo(myThid),myByHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1,OLx
          phi(sNx+I,J,bW,bj)=phi(  1+I-1,J,bE,bj)
          phi(1-I  ,J,bE,bj)=phi(sNx-I+1,J,bW,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bE=myBxLo(myThid)+1,myBxHi(myThid)
       bW = bE-1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO J=1-OLy,sNy+OLy
         DO I=1,OLx
          phi(sNx+I,J,bW,bj)=phi(  1+I-1,J,bE,bj)
          phi(1-I  ,J,bE,bj)=phi(sNx-I+1,J,bW,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Need to ensure all threads have completed x-axis transfers before
C     we can do y-axis exchanges.
      _BARRIER

C--   y-axis exchanges
      bN = myByLo(myThid)
      IF ( bN .NE. 1 .OR.
     &     commS(myThid) .EQ. COMM_SHARED ) THEN
       bS = bN - 1
       IF ( bS .LT. 1 ) bS = nSy
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,OLy
         DO I=1-OLx,sNx+OLx
          phi(I,1-J    ,bi,bN)=phi(I,sNy-J+1,bi,bS)
          phi(I,sNy+J  ,bi,bS)=phi(I,1+J-1  ,bi,bN)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bN=myByLo(myThid)+1,myByHi(myThid)
       bS = bN - 1
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,OLy
         DO I=1-OLx,sNx+OLx
          phi(I,1-J    ,bi,bN)=phi(I,sNy-J+1,bi,bS)
          phi(I,sNy+J  ,bi,bS)=phi(I,1+J-1  ,bi,bN)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER

      RETURN
      END

CStartOfInterface
      SUBROUTINE EXCH_XYZ_R8( 
     U                       phi, 
     I                       myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_XYZ_R8                                   |
C     | o Handle exchanges for real*8, three-dimensional arrays. |
C     |==========================================================|
C     | Do true shared-memory data transfers and "messaging"     |
C     | tranfers for blocking case of data transfers.            |
C     | Applications call this routine using                     |
C     |  CALL EXCH..( x, myThid )                                |
C     | where x is a three-dimensional array with overlaps.      |
C     | This routine does true-shared-memory copies for blocks   |
C     | within a thread. It will also do MPI meesaging between   |
C     | different processes.                                     |
C     | Note:                                                    |
C     | =====                                                    |
C     | If it is used, "asynchronous" messaging in which         |
C     | communication overlaps computation is handled elsewhere  |
C     | - see recv.F and send.F.                                 |
C     | In practice MPI implementations may not be completely    |
C     | thread safe. In principle this code is correct even for  |
C     | mixed MPI and multi-threaded execution.                  |
C     | In multi-thread execution data managed by other threads  |
C     | is only automatically visible to another thread if it    |
C     | existed before the thread was created. This means that   |
C     | for Fortran we declare arrays with overlap regions in    |
C     | COMMON blocks.                                           |
C     \==========================================================/

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     === Routine arguments ===
C     phi    - Array who's overlap regions are to be exchanged
C     myThid - My thread id.
      Real*8 phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     bi,bj  - Outer loop counters
C     I,J,K  - Inner loop counters
      INTEGER bi, bj
      INTEGER I, J, K
#ifdef    ALLOW_USE_MPI
C     tagSend - Tags used to mark and select messages
C     tagRecv
C     nReqs  - MPI request counter.
C     reqIds - Outstanding requests to wait on.
C     mpiRC  - MPI return code
C     mpiStatArr - Multi-request status reporting array.
C     toPid      - Proc. id sending to
C     fromPid    - Proc. id receving from
C     elCount    - Number of items to send or receive
C     elType     - Datat type of elements
      INTEGER tagSend
      INTEGER tagRecv
      INTEGER nReqs
      INTEGER reqIds(4)
      INTEGER mpiRC
      INTEGER mpiStatArr(MPI_STATUS_SIZE,4)
      INTEGER toPid
      INTEGER fromPid
      INTEGER elCount
      INTEGER elType
#endif /* ALLOW_USE_MPI */
C     bE, bW - Block index to east, west, north and
C     bN, bS   south.
      INTEGER bW, bE, bS, bN

C--   Do "message-passing" data exchanges
#ifdef  ALLOW_SYNC_COMMUNICATION
#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      IF ( usingSyncMessages ) THEN
#endif

C--    MPI based data exchages
#ifdef    ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
       IF ( usingMPI ) THEN
#endif

C       Here use the approach that all the X shifts are completed
C       before the Y shifts start. Then we can expand our Y shifts
C       to include X overlap regions. This means we don't have to
C       transfer corners separately. 
C       Notes:
C       ======
C       1. If we are only using messages in Y and using "true shared-memory"
C       in X then we rely on the shared-memory code to be implemented so that
C       corner values will be set correctly. The true shared-memory code
C       is in the block below - so be careful when altering it!
C       2. In order to ensure we grab the right message we need to tag
C       messages. We tag the messages with the direction in which they
C       were sent out. Thus to receive from a processor to our west we
C       request the next message from that processor that was sent out 
C       with a tag indicating an easterly send.
C       3. We could implement a "safe" code here by having everyone
C       sync and then getting a particular thread ( say thread 1 ) to
C       do the MPI_Isend sequentially. In this mode the "communication"
C       thread would loop over nThreads and do MPI_Sendrecv for each 
C       one. This could be more efficient on some platforms as the messages
C       might be sent as one large unit rather than by separate threads.
C       It would also be "thread-safe" for MPI implementations that have
C       multi-threading problems with things like request ids.
C       4. We include overlap regions in both X and Y sends.
C       This means we can interchange the Y block with the X block.
C       Not sure whether this will ever be useful!
C       5. The generation of a request handle by MPI_Isend and
C       MPI_Irecv ouught to involve some global state within MPI.
C       If this process is not thread safe it may be necessary to
C       enable a critical section around the MPI_Isend and
C       MPI_Irecv calls.

C      We need a barrier here to synchronise threads otherwise 
C      one thread might declare it has is ready to send and receive
C      even though another thread is going to write to the first
C      threads send or receive region. This won't happen, however,
C      if we are careful about which data a thread updates - but at
C      the moment we aren't careful!
       _BARRIER

C--    East-west messaging communication
       bE = myBxLo(myThid)
       bW = myBxHi(myThid)
       nReqs = 0
       IF ( bE .EQ. 1 .AND. commW(myThid) .EQ. COMM_MPI ) THEN
C       My west face uses MPI. Get ready to receive data from 
C       west and post that we are ready to send data to the west.
C       Data we receive has to come with tag of thread we know is
C       sending from our west, data we send is tagged with our thread id
C       and the send direction.
        nReqs   = nReqs+1
        tagSend = mpiTagW*nThreads+myThid
        toPid   = mpiPidW
        elCount = 1
        elType  = mpiTypeXFaceThread_xyz_R8(myThid)
        CALL MPI_Isend(
     I                 phi(1,1-OLy,1,bE,myByLo(myThid)),
     I                 elCount, elType, toPid,
     I                 tagSend,MPI_COMM_WORLD,
     I                 reqIds(nReqs),mpiRC)
       ENDIF
       IF ( bW .EQ. nSx .AND. commE(MyThid) .EQ. COMM_MPI ) THEN
        nReqs = nReqs+1
        tagRecv = mpiTagW*nThreads+myThrE(myThid)
        fromPid = mpiPidE
        elCount = 1
        elType  = mpiTypeXFaceThread_xyz_R8(myThid)
        CALL MPI_Irecv(
     U                 phi(sNx+1,1-OLy,1,bW,myByLo(myThid)),
     I                 elCount, elType, fromPid,
     I                 tagRecv,MPI_COMM_WORLD,
     I                 reqIds(nReqs),mpiRC)
       ENDIF
       CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

       nReqs = 0
       IF ( bW .EQ. nSx .AND. commE(MyThid) .EQ. COMM_MPI ) THEN
C       My east face uses MPI. Get ready to receive data from 
C       east and post that we are ready to send data to the east.
C       Data we receive has to come with tag of thread we know is
C       sending from our west, data we send is tagged with our thread id.
        nReqs = nReqs+1
        tagSend = mpiTagE*nThreads+myThid
        toPid   = mpiPidE
        elCount = 1
        elType  = mpiTypeXFaceThread_xyz_R8(myThid)
        CALL MPI_Isend(
     I                 phi(sNx-OLx+1,1-OLy,1,bW,myByLo(myThid)),
     I                 elCount, elType, toPid,
     I                 tagSend,MPI_COMM_WORLD,
     I                 reqIds(nReqs),mpiRC)
       ENDIF
       IF ( bE .EQ. 1 .AND. commW(myThid) .EQ. COMM_MPI ) THEN
        nReqs = nReqs+1
        tagRecv = mpiTagE*nThreads+myThrW(myThid)
        fromPid = mpiPidW
        elCount = 1
        elType  = mpiTypeXFaceThread_xyz_R8(myThid)
        CALL MPI_Irecv(
     U                 phi(1-OLx,1-OLy,1,bE,myByLo(myThid)),
     I                 elCount, elType, fromPid,
     I                 tagRecv,MPI_COMM_WORLD,
     I                 reqIds(nReqs),mpiRC)
       ENDIF

C      Wait for this threads east-west transactions to finish before
C      posting north-south transactions. We have to do this so that
C      the north-south transactions will send out the correct overlap
C      region values into the corner sections of our neighbors.
       CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

CcnhDebugStarts
C       RETURN
CcnhDebugEnds


C--    North-south messaging communication
       nReqs = 0
       bN = myByLo(myTHid)
       bS = myByHi(myTHid)

       IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
C       My south face uses MPI. Get ready to receive data from
C       south and post that I am ready to send data to the south.
        nReqs = nReqs + 1
        tagSend = mpiTagS*nThreads+myThid
        toPid   = mpiPidS
        elCount = 1
        elType  = mpiTypeYFaceThread_xyz_R8(myThid)
        CALL MPI_Isend(
     I                 phi(1-OLx,1,1,myBxLo(myThid),bN), 
     I                 elCount, elType, toPid,
     I                 tagSend,MPI_COMM_WORLD,
     I                 reqIds(nReqs), mpiRC )
       ENDIF
       IF ( bS .EQ. nSy .AND. commN(myThid) .EQ. COMM_MPI ) THEN
        nReqs = nReqs + 1
        tagRecv = mpiTagS*nThreads+myThrN(myThid)
        fromPid = mpiPidN
        elCount = 1
        elType  = mpiTypeYFaceThread_xyz_R8(myThid)
        CALL MPI_Irecv(
     I                 phi(1-OLx,sNy+1,1,myBxLo(myThid),bS),
     I                 elCount, elType, fromPid,
     I                 tagRecv,MPI_COMM_WORLD,
     I                 reqIds(nReqs), mpiRC )
       ENDIF
C      Wait for this threads north-south transactions to finish.
       CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )
C
       nReqs = 0
       IF ( bS .EQ. nSy .AND. commN(myThid) .EQ. COMM_MPI ) THEN
C       My north face uses MPI. Get ready to receive data from
C       north and post that I am ready to send data to the north.
        nReqs = nReqs + 1
        tagSend = mpiTagN*nThreads+myThid
        toPid   = mpiPidN
        elCount = 1
        elType  = mpiTypeYFaceThread_xyz_R8(myThid)
        CALL MPI_Isend(
     I                 phi(1-OLx,sNy-OLy+1,1,myBxLo(myThid),bS),
     I                 elCount, elType, toPid,                      
     I                 tagSend,MPI_COMM_WORLD,
     I                 reqIds(nReqs), mpiRC )
       ENDIF
       IF ( bN .EQ. 1 .AND. commS(myThid) .EQ. COMM_MPI ) THEN
        nReqs = nReqs + 1
        tagRecv = mpiTagN*nThreads+myThrS(myThid)
        fromPid = mpiPidS
        elCount = 1
        elType  = mpiTypeYFaceThread_xyz_R8(myThid)
        CALL MPI_Irecv(
     I                 phi(1-OLx,1-OLy,1,myBxLo(myThid),bN),
     I                 elCount, elType, fromPid,
     I                 tagRecv,MPI_COMM_WORLD,
     I                 reqIds(nReqs), mpiRC )
       ENDIF
C      Wait for this threads north-south transactions to finish.
       CALL MPI_Waitall( nReqs, reqIds, mpiStatArr, mpiRC )

#ifndef ALWAYS_USE_MPI 
       ENDIF 
#endif
#endif /* ALLOW_USE_MPI */

#ifndef ALWAYS_USE_SYNC_COMMUNICATION
      ENDIF
#endif
#endif /* ALLOW_SYNC_COMMUNICATION */

C--   Do true shared-memory data exchanges
C     Note: This section also does the overlap copies for serial
C           code overlap copies.
C--   First make sure all threads have reached here
      _BARRIER

C--   Now do copies
C     Notes:
C     ======
C     Here we copy from up to and including the overlap
C     regions for both the X shift and the Y shift. This
C     catches the "corners" of the data copied using messages
C     in the "synchronous communication" section above.
C--   x-axis exchanges
      bE = myBxLo(myThid)
      IF ( bE .NE. 1 .OR.
     &     commW(myThid) .EQ. COMM_SHARED ) THEN
       bW = bE-1
       IF ( bW .LT. 1 ) bW = nSx
       DO bj=myByLo(myThid),myByHi(myThid)
        DO K=1,Nz
         DO J=1-OLy,sNy+OLy
          DO I=1,OLx
           phi(sNx+I,J,K,bW,bj)=phi(  1+I-1,J,K,bE,bj)
           phi(1-I  ,J,K,bE,bj)=phi(sNx-I+1,J,K,bW,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bE=myBxLo(myThid)+1,myBxHi(myThid)
       bW = bE-1
       DO bj=myByLo(myThid),myByHi(myThid)
        DO K=1,Nz
         DO J=1-OLy,sNy+OLy
          DO I=1,OLx
           phi(sNx+I,J,K,bW,bj)=phi(  1+I-1,J,K,bE,bj)
           phi(1-I  ,J,K,bE,bj)=phi(sNx-I+1,J,K,bW,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Need to ensure all threads have completed x-axis transfers before
C     we can do y-axis exchanges.
      _BARRIER

C--   y-axis exchanges
      bN = myByLo(myThid)
      IF ( bN .NE. 1 .OR.
     &     commS(myThid) .EQ. COMM_SHARED ) THEN
       bS = bN - 1
       IF ( bS .LT. 1 ) bS = nSy
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nz
         DO J=1,OLy
          DO I=1-OLx,sNx+OLx
           phi(I,1-J    ,K,bi,bN)=phi(I,sNy-J+1,K,bi,bS)
           phi(I,sNy+J  ,K,bi,bS)=phi(I,1+J-1  ,K,bi,bN)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      DO bN=myByLo(myThid)+1,myByHi(myThid)
       bS = bN - 1
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nz
         DO J=1,OLy
          DO I=1-OLx,sNx+OLx
           phi(I,1-J    ,K,bi,bN)=phi(I,sNy-J+1,K,bi,bS)
           phi(I,sNy+J  ,K,bi,bS)=phi(I,1+J-1  ,K,bi,bN)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER

      RETURN
      END
