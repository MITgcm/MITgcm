C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch_recv_get_y.F,v 1.4 1999/05/03 21:40:00 adcroft Exp $
#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH_RL_RECV_GET_Y( array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            theSimulationMode, theCornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE RECV_GET_Y                                    |
C     | o "Send" or "put" Y edges for RL array.                  |
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
C     biS, bjS                   - South tile indices
C     biN, bjN                   - North tile indices
C     eBl                        - Current exchange buffer level
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     southCommMode              - Working variables holding type
C     northCommMode                of communication a particular
C                                  tile face uses.
C     spinCount                  - Exchange statistics counter
      INTEGER I, J, K, iMin, iMax, jMin, jMax, iB, iB0
      INTEGER bi, bj, biS, bjS, biN, bjN
      INTEGER eBl
      INTEGER southCommMode
      INTEGER northCommMode
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
        southCommMode  = _tileCommModeS(bi,bj)
        northCommMode  = _tileCommModeN(bi,bj)
        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)
        IF ( southCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidS(bi,bj)
         theTag  = _tileTagRecvS(bi,bj)
         theType = MPI_DOUBLE_PRECISION
         theSize = sNx*exchWidthY*myNz
         IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS )
     &    theSize = (sNx+2*exchWidthX)*exchWidthY*myNz
         CALL MPI_Recv( southRecvBuf_RL(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
        IF ( northCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidN(bi,bj)
         theTag  = _tileTagRecvN(bi,bj)
         theType = MPI_DOUBLE_PRECISION
         theSize = sNx*exchWidthY*myNz
         IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS )
     &    theSize = (sNx+2*exchWidthX)*exchWidthY*myNz
         CALL MPI_Recv( northRecvBuf_RL(1,eBl,bi,bj), theSize, theType,
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
         ebL = exchangeBufLevel(1,bi,bj)
         southCommMode = _tileCommModeS(bi,bj)
         northCommMode = _tileCommModeN(bi,bj)
         spinCount = 0
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          STOP ' S/R EXCH_RECV_GET_Y: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( southRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
          IF ( northRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
C        Clear requests
         southRecvAck(eBl,bi,bj) = 0.
         northRecvAck(eBl,bi,bj) = 0.
C        Update statistics
         IF ( exchCollectStatistics ) THEN
          exchRecvYExchCount(1,bi,bj) = exchRecvYExchCount(1,bi,bj)+1
          exchRecvYSpinCount(1,bi,bj) = 
     &    exchRecvYSpinCount(1,bi,bj)+spinCount
          exchRecvYSpinMax(1,bi,bj) = 
     &    MAX(exchRecvYSpinMax(1,bi,bj),spinCount)
          exchRecvYSpinMin(1,bi,bj) = 
     &    MIN(exchRecvYSpinMin(1,bi,bj),spinCount)
         ENDIF


         IF ( exchNReqsY(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
          CALL MPI_Waitall( exchNReqsY(1,bi,bj), exchReqIdY(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsY(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)
        southCommMode = _tileCommModeS(bi,bj)
        northCommMode = _tileCommModeN(bi,bj)
        IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
         iMin =   1-exchWidthX
         iMax = sNx+exchWidthX
        ELSE
         iMin = 1
         iMax = sNx
        ENDIF
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         jMin = sNy+1
         jMax = sNy+exchWidthY
         iB0  = 0
         IF (  northCommMode .EQ. COMM_PUT
     &        .OR. northCommMode .EQ. COMM_MSG  ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = northRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( northCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = array(I,iB,K,biN,bjN)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         jMin = sNy-exchWidthY+1
         jMax = sNy
         iB0  = 1-exchWidthY-1
         IF (  northCommMode .EQ. COMM_PUT 
     &        .OR. northCommMode .EQ. COMM_MSG  ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+northRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( northCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(I,iB,K,biN,bjN)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF

        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         jMin = 1-exchWidthY
         jMax = 0
         iB0  = sNy-exchWidthY
         IF (      southCommMode .EQ. COMM_PUT
     &        .OR. southCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = southRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( southCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = array(I,iB,K,biS,bjS)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         jMin = 1
         jMax = 1+exchWidthY-1
         iB0  = sNy
         IF (      southCommMode .EQ. COMM_PUT 
     &        .OR. southCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+southRecvBuf_RL(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( southCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(I,iB,K,biS,bjS)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE EXCH_RS_RECV_GET_Y( array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            theSimulationMode, theCornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE RECV_GET_Y                                    |
C     | o "Send" or "put" Y edges for RS array.                  |
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
C     biS, bjS                   - South tile indices
C     biN, bjN                   - North tile indices
C     eBl                        - Current exchange buffer level
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     southCommMode              - Working variables holding type
C     northCommMode                of communication a particular
C                                  tile face uses.
C     spinCount                  - Exchange statistics counter
      INTEGER I, J, K, iMin, iMax, jMin, jMax, iB, iB0
      INTEGER bi, bj, biS, bjS, biN, bjN
      INTEGER eBl
      INTEGER southCommMode
      INTEGER northCommMode
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
        southCommMode  = _tileCommModeS(bi,bj)
        northCommMode  = _tileCommModeN(bi,bj)
        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)
        IF ( southCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidS(bi,bj)
         theTag  = _tileTagRecvS(bi,bj)
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         theSize = sNx*exchWidthY*myNz
         IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS )
     &    theSize = (sNx+2*exchWidthX)*exchWidthY*myNz
         CALL MPI_Recv( southRecvBuf_RS(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
        ENDIF
        IF ( northCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidN(bi,bj)
         theTag  = _tileTagRecvN(bi,bj)
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         theSize = sNx*exchWidthY*myNz
         IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS )
     &    theSize = (sNx+2*exchWidthX)*exchWidthY*myNz
         CALL MPI_Recv( northRecvBuf_RS(1,eBl,bi,bj), theSize, theType,
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
         ebL = exchangeBufLevel(1,bi,bj)
         southCommMode = _tileCommModeS(bi,bj)
         northCommMode = _tileCommModeN(bi,bj)
         spinCount = 0
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          STOP ' S/R EXCH_RECV_GET_Y: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( southRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
          IF ( northRecvAck(eBl,bi,bj) .EQ. 0. ) GOTO 10
C        Clear requests
         southRecvAck(eBl,bi,bj) = 0.
         northRecvAck(eBl,bi,bj) = 0.
C        Update statistics
         IF ( exchCollectStatistics ) THEN
          exchRecvYExchCount(1,bi,bj) = exchRecvYExchCount(1,bi,bj)+1
          exchRecvYSpinCount(1,bi,bj) = 
     &    exchRecvYSpinCount(1,bi,bj)+spinCount
          exchRecvYSpinMax(1,bi,bj) = 
     &    MAX(exchRecvYSpinMax(1,bi,bj),spinCount)
          exchRecvYSpinMin(1,bi,bj) = 
     &    MIN(exchRecvYSpinMin(1,bi,bj),spinCount)
         ENDIF


         IF ( exchNReqsY(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
          CALL MPI_Waitall( exchNReqsY(1,bi,bj), exchReqIdY(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsY(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)
        southCommMode = _tileCommModeS(bi,bj)
        northCommMode = _tileCommModeN(bi,bj)
        IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
         iMin =   1-exchWidthX
         iMax = sNx+exchWidthX
        ELSE
         iMin = 1
         iMax = sNx
        ENDIF
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         jMin = sNy+1
         jMax = sNy+exchWidthY
         iB0  = 0
         IF (     northCommMode .EQ. COMM_PUT
     &       .OR. northCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = northRecvBuf_RS(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( northCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = array(I,iB,K,biN,bjN)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         jMin = sNy-exchWidthY+1
         jMax = sNy
         iB0  = 1-exchWidthY-1
         IF (     northCommMode .EQ. COMM_PUT 
     &       .OR. northCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+northRecvBuf_RS(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( northCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(I,iB,K,biN,bjN)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF

        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         jMin = 1-exchWidthY
         jMax = 0
         iB0  = sNy-exchWidthY
         IF (      southCommMode .EQ. COMM_PUT
     &        .OR. southCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = southRecvBuf_RS(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( southCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = array(I,iB,K,biS,bjS)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         jMin = 1
         jMax = 1+exchWidthY-1
         iB0  = sNy
         IF (      southCommMode .EQ. COMM_PUT 
     &        .OR. southCommMode .EQ. COMM_MSG ) THEN
          iB  = 0
          DO K=1,myNz
           DO J=jMin,jMax
            DO I=iMin,iMax
             iB = iB + 1
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+southRecvBuf_RS(iB,eBl,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ELSEIF ( southCommMode .EQ. COMM_GET ) THEN
          DO K=1,myNz
           iB = iB0
           DO J=jMin,jMax
            iB = iB+1
            DO I=iMin,iMax
             array(I,J,K,bi,bj) = 
     &       array(I,J,K,bi,bj)+array(I,iB,K,biS,bjS)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ENDIF

       ENDDO
      ENDDO

      RETURN
      END

