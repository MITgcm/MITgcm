C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch_recv_get_x.F,v 1.4 1999/05/03 21:40:00 adcroft Exp $
#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH_RL_RECV_GET_X( array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            theSimulationMode, theCornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE RECV_RL_GET_X                                 |
C     | o "Send" or "put" X edges for RL array.                  |
C     |==========================================================|
C     | Routine that invokes actual message passing send or      |
C     | direct "put" of data to update X faces of an XY[R] array.|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     == Routine arguments ==
C     array - Array with edges to exchange.
C     myOLw - West, East, North and South overlap region sizes.
C     myOLe
C     myOLn
C     myOLs
C     exchWidthX - Width of data region exchanged.
C     exchWidthY
C     theSimulationMode - Forward or reverse mode exchange ( provides
C                         support for adjoint integration of code. )
C     theCornerMode     - Flag indicating whether corner updates are
C                         needed.
C     myThid            - Thread number of this instance of S/R EXCH...
C     eBl               - Edge buffer level
      INTEGER myOLw
      INTEGER myOLe
      INTEGER myOLs
      INTEGER myOLn
      INTEGER myNz
      _RL array(1-myOLw:sNx+myOLe,
     &          1-myOLs:sNy+myOLn,
     &          myNZ, nSx, nSy)
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER theSimulationMode
      INTEGER theCornerMode
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K, iMin, iMax, iB    - Loop counters and extents
C     bi, bj
C     biW, bjW                   - West tile indices
C     biE, bjE                   - East tile indices
C     eBl                        - Current exchange buffer level
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     westCommMode               - Working variables holding type
C     eastCommMode                 of communication a particular
C                                  tile face uses.
      INTEGER I, J, K, iMin, iMax, iB, iB0
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER eBl
      INTEGER westCommMode
      INTEGER eastCommMode
      INTEGER spinCount
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize
      INTEGER mpiStatus(MPI_STATUS_SIZE,4), mpiRc
#endif


C--   Under a "put" scenario we 
C--     i. set completetion signal for buffer we put into.
C--    ii. wait for completetion signal indicating data has been put in 
C--        our buffer.
C--   Under a messaging mode we "receive" the message.
C--   Under a "get" scenario we
C--     i. Check that the data is ready.
C--    ii. Read the data.
C--   iii. Set data read flag + memory sync.


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        ebL = exchangeBufLevel(1,bi,bj)
        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)
        IF ( westCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidW(bi,bj)
         theTag  = _tileTagRecvW(bi,bj)
         theType = MPI_DOUBLE_PRECISION
         theSize = sNy*exchWidthX*myNz
         CALL MPI_Recv( westRecvBuf_RL(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
        IF ( eastCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidE(bi,bj)
         theTag  = _tileTagRecvE(bi,bj)
         theType = MPI_DOUBLE_PRECISION
         theSize = sNy*exchWidthX*myNz
         CALL MPI_Recv( eastRecvBuf_RL(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
       ENDDO
      ENDDO

C--   Wait for buffers I am going read to be ready.
      IF ( exchUsesBarrier  ) THEN
C      o On some machines ( T90 ) use system barrier rather than spinning.
       CALL BARRIER( myThid )
      ELSE
C      o Spin waiting for completetion flag. This avoids a global-lock
C        i.e. we only lock waiting for data that we need.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         spinCount = 0
         ebL = exchangeBufLevel(1,bi,bj)
         westCommMode = _tileCommModeW(bi,bj)
         eastCommMode = _tileCommModeE(bi,bj)
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          WRITE(0,*) ' eBl = ', ebl
C          STOP ' S/R EXCH_RECV_GET_X: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( westRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
          IF ( eastRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
C        Clear outstanding requests
         westRecvAck(eBl,bi,bj) = 0.
         eastRecvAck(eBl,bi,bj) = 0.

         IF ( exchNReqsX(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
          CALL MPI_Waitall( exchNReqsX(1,bi,bj), exchReqIdX(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsX(1,bi,bj) = 0
C        Update statistics
         IF ( exchCollectStatistics ) THEN
          exchRecvXExchCount(1,bi,bj) = exchRecvXExchCount(1,bi,bj)+1
          exchRecvXSpinCount(1,bi,bj) = 
     &    exchRecvXSpinCount(1,bi,bj)+spinCount
          exchRecvXSpinMax(1,bi,bj) = 
     &    MAX(exchRecvXSpinMax(1,bi,bj),spinCount)
          exchRecvXSpinMin(1,bi,bj) = 
     &    MIN(exchRecvXSpinMin(1,bi,bj),spinCount)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = sNx+1
         iMax = sNx+exchWidthX
         iB0  = 0
         IF (     eastCommMode .EQ. COMM_PUT
     &       .OR. eastCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=1,sNy
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = eastRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           DO J=1,sNy
            iB = iB0
            DO I=iMin,iMax
             iB = iB+1
             array(I,J,K,bi,bj) = array(iB,J,K,biE,bjE)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = sNx-exchWidthX+1
         iMax = sNx
         iB0  = 1-exchWidthX-1
         IF (     eastCommMode .EQ. COMM_PUT 
     &       .OR. eastCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=1,sNy
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+eastRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( eastCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           DO J=1,sNy
            iB = iB0
            DO I=iMin,iMax
             iB = iB+1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(iB,J,K,biE,bjE)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = 1-exchWidthX
         iMax = 0
         iB0  = sNx-exchWidthX
         IF (      westCommMode .EQ. COMM_PUT
     &        .OR. westCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=1,sNy
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = westRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           DO J=1,sNy
            iB = iB0
            DO I=iMin,iMax
             iB = iB+1
             array(I,J,K,bi,bj) = array(iB,J,K,biW,bjW)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = 1
         iMax = 1+exchWidthX-1
         iB0  = sNx
         IF (      westCommMode .EQ. COMM_PUT 
     &        .OR. westCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=1,sNy
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+westRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( westCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           DO J=1,sNy
            iB = iB0
            DO I=iMin,iMax
             iB = iB+1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(iB,J,K,biW,bjW)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE EXCH_RS_RECV_GET_X( array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            theSimulationMode, theCornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE RECV_GET_X                                    |
C     | o "Send" or "put" X edges for RS array.                  |
C     |==========================================================|
C     | Routine that invokes actual message passing send or      |
C     | direct "put" of data to update X faces of an XY[R] array.|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     == Routine arguments ==
C     array - Array with edges to exchange.
C     myOLw - West, East, North and South overlap region sizes.
C     myOLe
C     myOLn
C     myOLs
C     exchWidthX - Width of data region exchanged.
C     exchWidthY
C     theSimulationMode - Forward or reverse mode exchange ( provides
C                         support for adjoint integration of code. )
C     theCornerMode     - Flag indicating whether corner updates are
C                         needed.
C     myThid            - Thread number of this instance of S/R EXCH...
C     eBl               - Edge buffer level
      INTEGER myOLw
      INTEGER myOLe
      INTEGER myOLs
      INTEGER myOLn
      INTEGER myNz
      _RS array(1-myOLw:sNx+myOLe,
     &          1-myOLs:sNy+myOLn,
     &          myNZ, nSx, nSy)
      INTEGER exchWidthX
      INTEGER exchWidthY
      INTEGER theSimulationMode
      INTEGER theCornerMode
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K, iMin, iMax, iB    - Loop counters and extents
C     bi, bj
C     biW, bjW                   - West tile indices
C     biE, bjE                   - East tile indices
C     eBl                        - Current exchange buffer level
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     westCommMode               - Working variables holding type
C     eastCommMode                 of communication a particular
C                                  tile face uses.
      INTEGER I, J, K, iMin, iMax, iB, iB0
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER eBl
      INTEGER westCommMode
      INTEGER eastCommMode
      INTEGER spinCount
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize
      INTEGER mpiStatus(MPI_STATUS_SIZE,4), mpiRc
#endif


C--   Under a "put" scenario we 
C--     i. set completetion signal for buffer we put into.
C--    ii. wait for completetion signal indicating data has been put in 
C--        our buffer.
C--   Under a messaging mode we "receive" the message.
C--   Under a "get" scenario we
C--     i. Check that the data is ready.
C--    ii. Read the data.
C--   iii. Set data read flag + memory sync.


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        ebL = exchangeBufLevel(1,bi,bj)
        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)
        IF ( westCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidW(bi,bj)
         theTag  = _tileTagRecvW(bi,bj)
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         theSize = sNy*exchWidthX*myNz
         CALL MPI_Recv( westRecvBuf_RS(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
        IF ( eastCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidE(bi,bj)
         theTag  = _tileTagRecvE(bi,bj)
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         theSize = sNy*exchWidthX*myNz
         CALL MPI_Recv( eastRecvBuf_RS(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
       ENDDO
      ENDDO

C--   Wait for buffers I am going read to be ready.
      IF ( exchUsesBarrier  ) THEN
C      o On some machines ( T90 ) use system barrier rather than spinning.
       CALL BARRIER( myThid )
      ELSE
C      o Spin waiting for completetion flag. This avoids a global-lock
C        i.e. we only lock waiting for data that we need.
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         spinCount = 0
         ebL = exchangeBufLevel(1,bi,bj)
         westCommMode = _tileCommModeW(bi,bj)
         eastCommMode = _tileCommModeE(bi,bj)
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          WRITE(0,*) ' eBl = ', ebl
C          STOP ' S/R EXCH_RECV_GET_X: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( westRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
          IF ( eastRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
C        Clear outstanding requests
         westRecvAck(eBl,bi,bj) = 0.
         eastRecvAck(eBl,bi,bj) = 0.

         IF ( exchNReqsX(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
          CALL MPI_Waitall( exchNReqsX(1,bi,bj), exchReqIdX(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsX(1,bi,bj) = 0
C        Update statistics
         IF ( exchCollectStatistics ) THEN
          exchRecvXExchCount(1,bi,bj) = exchRecvXExchCount(1,bi,bj)+1
          exchRecvXSpinCount(1,bi,bj) = 
     &    exchRecvXSpinCount(1,bi,bj)+spinCount
          exchRecvXSpinMax(1,bi,bj) = 
     &    MAX(exchRecvXSpinMax(1,bi,bj),spinCount)
          exchRecvXSpinMin(1,bi,bj) = 
     &    MIN(exchRecvXSpinMin(1,bi,bj),spinCount)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = sNx+1
         iMax = sNx+exchWidthX
         iB0  = 0
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = sNx-exchWidthX+1
         iMax = sNx
         iB0  = 1-exchWidthX-1
        ENDIF
        IF (     eastCommMode .EQ. COMM_PUT 
     &      .OR. eastCommMode .EQ. COMM_MSG ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            array(I,J,K,bi,bj) = eastRecvBuf_RS(iB,eBl,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( eastCommMode .EQ. COMM_GET ) THEN
         DO K=1,myNz
          DO J=1,sNy
           iB = iB0
           DO I=iMin,iMax
            iB = iB+1
            array(I,J,K,bi,bj) = array(iB,J,K,biE,bjE)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = 1-exchWidthX
         iMax = 0
         iB0  = sNx-exchWidthX
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = 1
         iMax = 1+exchWidthX-1
         iB0  = sNx
        ENDIF
        IF (      westCommMode .EQ. COMM_PUT 
     &       .OR. westCommMode .EQ. COMM_MSG ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            array(I,J,K,bi,bj) = westRecvBuf_RS(iB,eBl,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( westCommMode .EQ. COMM_GET ) THEN
         DO K=1,myNz
          DO J=1,sNy
           iB = iB0
           DO I=iMin,iMax
            iB = iB+1
            array(I,J,K,bi,bj) = array(iB,J,K,biW,bjW)
           ENDDO
          ENDDO
         ENDDO
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END

