C $Header: /u/gcmpack/MITgcm/pkg/flt/exch_send_put_vec.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
#include "CPP_OPTIONS.h"
#ifdef ALLOW_FLT
#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH_RL_SEND_PUT_VEC_X( arrayE, arrayW,
     I            myd1, myThid )
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
#include "FLT.h"
#include "EXCH.h"
C     == Routine arguments ==
C     arrayE - Array to be exchanged.
C     arrayW
C     myd1   - sizes.
C     myd2
C     myThid            - Thread number of this instance of S/R EXCH...
      INTEGER myd1
      INTEGER myd2
      _RL arrayE(myd1, nSx, nSy), arrayW(myd1, nSx, nSy)
      INTEGER theSimulationMode
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J                       - Loop counters and extents
C     bi, bj  
C     biW, bjW                   - West tile indices
C     biE, bjE                   - East tile indices
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     westCommMode               - Working variables holding type
C     eastCommMode                 of communication a particular
C                                  tile face uses.
      INTEGER I, J
      INTEGER bi, bj, biW, bjW, biE, bjE
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

        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)

C       o Send or Put west edge
        IF ( westCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidW(bi,bj)
         theTag  = _tileTagSendW(bi,bj)
         theSize = myd1
         theType = MPI_DOUBLE_PRECISION
c         exchVReqsX(1,bi,bj) = exchVReqsX(1,bi,bj)+1
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
         CALL MPI_Isend(arrayW(1,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc)
c     &                  exchReqVIdX(exchVReqsX(1,bi,bj),1,bi,bj), mpiRc)
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         eastRecvAck(1,biW,bjW) = 1.
        ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
          DO I=1,myd1
            arrayE(I,biW,bjW) = arrayW(I,bi,bj)
          ENDDO
        ELSEIF ( westCommMode .NE. COMM_NONE 
     &   .AND.   westCommMode .NE. COMM_GET ) THEN
         STOP ' S/R EXCH: Invalid commW mode.'
        ENDIF

C       o Send or Put east edge
        IF ( eastCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidE(bi,bj)
         theTag  = _tileTagSendE(bi,bj)
         theSize = myd1
         theType = MPI_DOUBLE_PRECISION
c         exchVReqsX(1,bi,bj) = exchVReqsX(1,bi,bj)+1
         exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
c         if (theProc .eq. 2 .or. theProc .eq. 4) then
c         if (arrayE(1,bi,bj) .ne. 0.) then
c            write(errormessageunit,*) 'qq1y: ',myprocid,
c     &      theProc,theTag,theSize,(arrayE(i,bi,bj),i=1,32)
c         endif
c         endif
         CALL MPI_Isend(arrayE(1,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), mpiRc)
c     &                  exchReqVIdX(exchVReqsX(1,bi,bj),1,bi,bj), mpiRc)
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         westRecvAck(1,biE,bjE) = 1.
        ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
          DO I=1,myd1
            arrayW(I,biE,bjE) = arrayE(I,bi,bj)
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
        biE = _tileBiE(bi,bj)
        bjE = _tileBjE(bi,bj)
        biW = _tileBiW(bi,bj)
        bjW = _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
        IF ( westCommMode .EQ. COMM_PUT ) eastRecvAck(1,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_PUT ) westRecvAck(1,biE,bjE) = 1.
        IF ( westCommMode .EQ. COMM_GET ) eastRecvAck(1,biW,bjW) = 1.
        IF ( eastCommMode .EQ. COMM_GET ) westRecvAck(1,biE,bjE) = 1.
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

      SUBROUTINE EXCH_RL_SEND_PUT_VEC_Y( arrayN, arrayS,
     I            myd1, myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_RL_SEND_PUT_Y                            |
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
#include "FLT.h"
#include "EXCH.h"
C     == Routine arguments ==
C     arrayN - Array to be exchanged.
C     arrayS
C     myd1   - sizes.
C     myd2
C     myThid            - Thread number of this instance of S/R EXCH...
      INTEGER myd1
      INTEGER myd2
      _RL arrayN(myd1, nSx, nSy), arrayS(myd1, nSx, nSy)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J                       - Loop counters and extents
C     bi, bj  
C     biN, bjN                   - North tile indices
C     biS, bjS                   - South tile indices
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     westCommMode               - Working variables holding type
C     eastCommMode                 of communication a particular
C                                  tile face uses.
      INTEGER I, J
      INTEGER bi, bj, biS, bjS, biN, bjN
      INTEGER southCommMode
      INTEGER northCommMode

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
C                Fill east receive buffer of south-neighbor tile
C                Fill west receive buffer of north-neighbor tile
C                Sync. memory
C                Write data-ready Ack for east edge of south-neighbor
C                tile
C                Write data-ready Ack for west edge of north-neighbor
C                tile
C                Sync. memory
C      
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        southCommMode  = _tileCommModeS(bi,bj)
        northCommMode  = _tileCommModeN(bi,bj)
        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)

C       o Send or Put south edge
        IF ( southCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidS(bi,bj)
         theTag  = _tileTagSendS(bi,bj)
         theSize = myd1
         theType = MPI_DOUBLE_PRECISION
c         exchVReqsY(1,bi,bj) = exchVReqsY(1,bi,bj)+1
         exchNReqsY(1,bi,bj) = exchNReqsY(1,bi,bj)+1
         CALL MPI_Isend(arrayS(1,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdY(exchNReqsY(1,bi,bj),1,bi,bj), mpiRc)
c     &                  exchReqVIdY(exchVReqsY(1,bi,bj),1,bi,bj), mpiRc)
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         northRecvAck(1,biS,bjS) = 1.
        ELSEIF ( southCommMode .EQ. COMM_PUT  ) THEN
          DO I=1,myd1
            arrayN(I,biS,bjS) = arrayS(I,bi,bj)
          ENDDO
        ELSEIF ( southCommMode .NE. COMM_NONE 
     &   .AND.   southCommMode .NE. COMM_GET ) THEN
         STOP ' S/R EXCH: Invalid commS mode.'
        ENDIF

C       o Send or Put north edge
        IF ( northCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
         theProc = tilePidN(bi,bj)
         theTag  = _tileTagSendN(bi,bj)
         theSize = myd1
         theType = MPI_DOUBLE_PRECISION
c         exchVReqsY(1,bi,bj) = exchVReqsY(1,bi,bj)+1
         exchNReqsY(1,bi,bj) = exchNReqsY(1,bi,bj)+1
         CALL MPI_Isend(arrayN(1,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  exchReqIdY(exchNReqsY(1,bi,bj),1,bi,bj), mpiRc)
c     &                  exchReqVIdY(exchVReqsY(1,bi,bj),1,bi,bj), mpiRc)
#ifndef ALWAYS_USE_MPI
         ENDIF
#endif
#endif /* ALLOW_USE_MPI */
         southRecvAck(1,biN,bjN) = 1.
        ELSEIF ( northCommMode .EQ. COMM_PUT  ) THEN
          DO I=1,myd1
            arrayS(I,biN,bjN) = arrayN(I,bi,bj)
          ENDDO
        ELSEIF ( northCommMode .NE. COMM_NONE
     &   .AND.   northCommMode .NE. COMM_GET  ) THEN
         STOP ' S/R EXCH: Invalid commN mode.'
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
        biN = _tileBiN(bi,bj)
        bjN = _tileBjN(bi,bj)
        biS = _tileBiS(bi,bj)
        bjS = _tileBjS(bi,bj)
        southCommMode = _tileCommModeE(bi,bj)
        northCommMode = _tileCommModeN(bi,bj)
        IF ( southCommMode .EQ. COMM_PUT ) northRecvAck(1,biS,bjS) = 1.
        IF ( northCommMode .EQ. COMM_PUT ) southRecvAck(1,biN,bjN) = 1.
        IF ( southCommMode .EQ. COMM_GET ) northRecvAck(1,biS,bjS) = 1.
        IF ( northCommMode .EQ. COMM_GET ) southRecvAck(1,biN,bjN) = 1.
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

#else
      SUBROUTINE EXCH_RL_SEND_PUT_VEC_X( myThid )
      INTEGER myThid
      return
      end
      SUBROUTINE EXCH_RL_SEND_PUT_VEC_Y(  myThid )
      INTEGER myThid
      return
      end
#endif
