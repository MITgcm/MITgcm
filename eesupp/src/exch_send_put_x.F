C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch_send_put_x.F,v 1.2 1999/03/22 17:37:43 adcroft Exp $
#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH_RL_SEND_PUT_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             thesimulationMode, thecornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_RL_SEND_PUT_X                            |
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
      INTEGER I, J, K, iMin, iMax, iB
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER eBl
      INTEGER westCommMode
      INTEGER eastCommMode

#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize, mpiRc
#endif
C--   Write data to exchange buffer
C     Various actions are possible depending on the communication mode 
C     as follows:
C       Mode      Action
C     --------   ---------------------------
C     COMM_NONE  Do nothing
C
C     COMM_MSG   Message passing communication ( e.g. MPI )
C                Fill west send buffer from this tile.
C                Send data with tag identifying tile and direction.
C                Fill east send buffer from this tile.
C                Send data with tag identifying tile and direction.
C
C     COMM_PUT   "Put" communication ( UMP_, shmemput, etc... )
C                Fill east receive buffer of west-neighbor tile
C                Fill west receive buffer of east-neighbor tile
C                Sync. memory
C                Write data-ready Ack for east edge of west-neighbor
C                tile
C                Write data-ready Ack for west edge of east-neighbor
C                tile
C                Sync. memory
C      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)

C       o Send or Put west edge
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = 1
         iMax = 1+exchWidthX-1
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = 1-exchWidthX
         iMax = 0
        ENDIF
        IF ( westCommMode .EQ. COMM_MSG  ) THEN
         iB = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            westSendBuf_RL(iB,eBl,bi,bj) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidW(bi,bj)
         theTag  = _tileTagSendW(bi,bj)
         theSize = iB
         theType = MPI_DOUBLE_PRECISION
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
         CALL MPI_Isend(westSendBuf_RL(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc )
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         eastRecvAck(eBl,biW,bjW) = 1.
        ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            eastRecvBuf_RL(iB,eBl,biW,bjW) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( westCommMode .NE. COMM_NONE 
     &   .AND.   westCommMode .NE. COMM_GET ) THEN
         STOP ' S/R EXCH: Invalid commW mode.'
        ENDIF

C       o Send or Put east edge
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = sNx-exchWidthX+1
         iMax = sNx
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = sNx+1
         iMax = sNx+exchWidthX
        ENDIF
        IF ( eastCommMode .EQ. COMM_MSG  ) THEN
         iB = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            eastSendBuf_RL(iB,eBl,bi,bj) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidE(bi,bj)
         theTag  = _tileTagSendE(bi,bj)
         theSize = iB
         theType = MPI_DOUBLE_PRECISION
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
         CALL MPI_Isend(eastSendBuf_RL(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc )
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         westRecvAck(eBl,biE,bjE) = 1.
        ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            westRecvBuf_RL(iB,eBl,biE,bjE) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( eastCommMode .NE. COMM_NONE
     &   .AND.   eastCommMode .NE. COMM_GET  ) THEN
         STOP ' S/R EXCH: Invalid commE mode.'
        ENDIF

       ENDDO
      ENDDO

C--   Signal completetion ( making sure system-wide memory state is
C--                         consistent ).

C     ** NOTE ** We are relying on being able to produce strong-ordered
C     memory semantics here. In other words we assume that there is a
C     mechanism which can ensure that by the time the Ack is seen the
C     overlap region data that will be exchanged is up to date.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        ebL = exchangeBufLevel(1,bi,bj)
        biE = _tileBiE(bi,bj)
        bjE = _tileBjE(bi,bj)
        biW = _tileBiW(bi,bj)
        bjW = _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
        IF ( westCommMode .EQ. COMM_PUT ) eastRecvAck(eBl,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_PUT ) westRecvAck(eBl,biE,bjE) = 1.
        IF ( westCommMode .EQ. COMM_GET ) eastRecvAck(eBl,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_GET ) westRecvAck(eBl,biE,bjE) = 1.
       ENDDO
      ENDDO

C--   Make sure "ack" setting is seen system-wide.
C     Here strong-ordering is not an issue but we want to make
C     sure that processes that might spin on the above Ack settings
C     will see the setting.
C     ** NOTE ** On some machines we wont spin on the Ack setting
C     ( particularly the T90 ), instead we will use s system barrier.
C     On the T90 the system barrier is very fast and switches out the 
C     thread while it waits. On most machines the system barrier
C     is much too slow and if we own the machine and have one thread
C     per process preemption is not a problem.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

      RETURN 
      END

      SUBROUTINE EXCH_RS_SEND_PUT_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             thesimulationMode, thecornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_RS_SEND_PUT_X                            |
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
      INTEGER I, J, K, iMin, iMax, iB
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER eBl
      INTEGER westCommMode
      INTEGER eastCommMode

#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize, mpiRc
#endif
C--   Write data to exchange buffer
C     Various actions are possible depending on the communication mode 
C     as follows:
C       Mode      Action
C     --------   ---------------------------
C     COMM_NONE  Do nothing
C
C     COMM_MSG   Message passing communication ( e.g. MPI )
C                Fill west send buffer from this tile.
C                Send data with tag identifying tile and direction.
C                Fill east send buffer from this tile.
C                Send data with tag identifying tile and direction.
C
C     COMM_PUT   "Put" communication ( UMP_, shmemput, etc... )
C                Fill east receive buffer of west-neighbor tile
C                Fill west receive buffer of east-neighbor tile
C                Sync. memory
C                Write data-ready Ack for east edge of west-neighbor
C                tile
C                Write data-ready Ack for west edge of east-neighbor
C                tile
C                Sync. memory
C      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        ebL = exchangeBufLevel(1,bi,bj)
        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)

C       o Send or Put west edge
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = 1
         iMax = 1+exchWidthX-1
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = 1-exchWidthX
         iMax = 0
        ENDIF
        IF ( westCommMode .EQ. COMM_MSG  ) THEN
         iB = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            westSendBuf_RS(iB,eBl,bi,bj) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidW(bi,bj)
         theTag  = _tileTagSendW(bi,bj)
         theSize = iB
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
         CALL MPI_Isend(westSendBuf_RS(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc )
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         eastRecvAck(eBl,biW,bjW) = 1.
        ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            eastRecvBuf_RS(iB,eBl,biW,bjW) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( westCommMode .NE. COMM_NONE 
     &   .AND.   westCommMode .NE. COMM_GET ) THEN
         STOP ' S/R EXCH: Invalid commW mode.'
        ENDIF

C       o Send or Put east edge
        IF     ( _theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
         iMin = sNx-exchWidthX+1
         iMax = sNx
        ELSEIF ( _theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
         iMin = sNx+1
         iMax = sNx+exchWidthX
        ENDIF
        IF ( eastCommMode .EQ. COMM_MSG  ) THEN
         iB = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            eastSendBuf_RS(iB,eBl,bi,bj) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidE(bi,bj)
         theTag  = _tileTagSendE(bi,bj)
         theSize = iB
         theType = MPI_DOUBLE_PRECISION
#ifdef RS_IS_REAL4
         theType = MPI_REAL4
#endif
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
         CALL MPI_Isend(eastSendBuf_RS(1,eBl,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc )
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         westRecvAck(eBl,biE,bjE) = 1.
        ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
         iB  = 0
         DO K=1,myNz
          DO J=1,sNy
           DO I=iMin,iMax
            iB = iB + 1
            westRecvBuf_RS(iB,eBl,biE,bjE) = array(I,J,K,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSEIF ( eastCommMode .NE. COMM_NONE
     &   .AND.   eastCommMode .NE. COMM_GET  ) THEN
         STOP ' S/R EXCH: Invalid commE mode.'
        ENDIF

       ENDDO
      ENDDO

C--   Signal completetion ( making sure system-wide memory state is
C--                         consistent ).

C     ** NOTE ** We are relying on being able to produce strong-ordered
C     memory semantics here. In other words we assume that there is a
C     mechanism which can ensure that by the time the Ack is seen the
C     overlap region data that will be exchanged is up to date.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        ebL = exchangeBufLevel(1,bi,bj)
        biE = _tileBiE(bi,bj)
        bjE = _tileBjE(bi,bj)
        biW = _tileBiW(bi,bj)
        bjW = _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
        IF ( westCommMode .EQ. COMM_PUT ) eastRecvAck(eBl,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_PUT ) westRecvAck(eBl,biE,bjE) = 1.
        IF ( westCommMode .EQ. COMM_GET ) eastRecvAck(eBl,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_GET ) westRecvAck(eBl,biE,bjE) = 1.
       ENDDO
      ENDDO

C--   Make sure "ack" setting is seen system-wide.
C     Here strong-ordering is not an issue but we want to make
C     sure that processes that might spin on the above Ack settings
C     will see the setting.
C     ** NOTE ** On some machines we wont spin on the Ack setting
C     ( particularly the T90 ), instead we will use s system barrier.
C     On the T90 the system barrier is very fast and switches out the 
C     thread while it waits. On most machines the system barrier
C     is much too slow and if we own the machine and have one thread
C     per process preemption is not a problem.
      IF ( exchNeedsMemSync  ) CALL MEMSYNC

      RETURN 
      END
