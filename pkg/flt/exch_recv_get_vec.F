C $Header: /u/gcmpack/MITgcm/pkg/flt/exch_recv_get_vec.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
#include "CPP_OPTIONS.h"
#include "CPP_EEOPTIONS.h"

      SUBROUTINE EXCH_RL_RECV_GET_VEC_X( arrayE, arrayW,
     I            myd1, myThid )
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
#include "FLT.h"
#include "EXCH.h"

C     == Routine arguments ==
C     arrayE - Arrays to exchange be exchanged.
C     arrayW 
C     myd1   - sizes.
C     myd2
C     theSimulationMode - Forward or reverse mode exchange ( provides
C                         support for adjoint integration of code. )
C     myThid            - Thread number of this instance of S/R EXCH...
C     eBl               - Edge buffer level
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
         theSize = myd1
         CALL MPI_Recv( arrayW(1,bi,bj), theSize, theType,
     &                  theProc, theTag, MPI_COMM_MODEL,
     &                  mpiStatus, mpiRc )
c         if (theProc .eq. 0 .or. theProc .eq. 2) then
c         if (arrayW(1,bi,bj) .ne. 0.) then
c            write(errormessageunit,*) 'qq2y: ',myprocid,
c     &      theProc,theTag,theSize,(arrayW(i,bi,bj),i=1,32)
c         else
c            write(errormessageunit,*) 'qq2n: ',myprocid,
c     &      theProc,theTag,theSize,(arrayW(i,bi,bj),i=1,32)
c         endif
c         endif
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
         theSize = myd1
         CALL MPI_Recv( arrayE(1,bi,bj), theSize, theType,
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
         westCommMode = _tileCommModeW(bi,bj)
         eastCommMode = _tileCommModeE(bi,bj)
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          WRITE(0,*) ' eBl = ', ebl
C          STOP ' S/R EXCH_RECV_GET_X: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( westRecvAck(1,bi,bj) .EQ. 0. ) GOTO 10
          IF ( eastRecvAck(1,bi,bj) .EQ. 0. ) GOTO 10
C        Clear outstanding requests
         westRecvAck(1,bi,bj) = 0.
         eastRecvAck(1,bi,bj) = 0.

c         IF ( exchVReqsX(1,bi,bj) .GT. 0 ) THEN
         IF ( exchNReqsX(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
c          CALL MPI_Waitall( exchVReqsX(1,bi,bj), exchReqVIdX(1,1,bi,bj),
          CALL MPI_Waitall( exchNReqsX(1,bi,bj), exchReqIdX(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
c         exchVReqsX(1,bi,bj) = 0
         exchNReqsX(1,bi,bj) = 0
C        Update statistics
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        biE =  _tileBiE(bi,bj)
        bjE =  _tileBjE(bi,bj)
        biW =  _tileBiW(bi,bj)
        bjW =  _tileBjW(bi,bj)
        westCommMode = _tileCommModeW(bi,bj)
        eastCommMode = _tileCommModeE(bi,bj)
         IF ( eastCommMode .EQ. COMM_GET ) THEN
          DO I=1,myd1
             arrayE(I,bi,bj) = arrayW(I,biE,bjE)
           ENDDO
         ENDIF
         IF ( westCommMode .EQ. COMM_GET ) THEN
          DO I=1,myd1
             arrayW(I,bi,bj) = arrayE(I,biW,bjW)
          ENDDO
         ENDIF

       ENDDO
      ENDDO

      RETURN
      END


      SUBROUTINE EXCH_RL_RECV_GET_VEC_Y( arrayN, arrayS,
     I            myd1, myThid )
C     /==========================================================\
C     | SUBROUTINE RECV_RL_GET_Y                                 |
C     | o "Send" or "put" Y edges for RL array.                  |
C     |==========================================================|
C     | Routine that invokes actual message passing send or      |
C     | direct "put" of data to update Y faces of an XY[R] array.|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "FLT.h"
#include "EXCH.h"

C     == Routine arguments ==
C     arrayN - Arrays to exchange be exchanged.
C     arrayS 
C     myd1   - sizes.
C     myd2
C     theSimulationMode - Forward or reverse mode exchange ( provides
C                         support for adjoint integration of code. )
C     myThid            - Thread number of this instance of S/R EXCH...
      INTEGER myd1
      INTEGER myd2
      _RL arrayN(myd1, nSx, nSy), arrayS(myd1, nSx, nSy)
      INTEGER theSimulationMode
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J                       - Loop counters and extents
C     bi, bj
C     biS, bjS                   - South tile indices
C     biE, bjE                   - North tile indices
C     theProc, theTag, theType,  - Variables used in message building
C     theSize
C     southCommMode               - Working variables holding type
C     northCommMode                 of communication a particular
C                                  tile face uses.
      INTEGER I, J
      INTEGER bi, bj, biS, bjS, biN, bjN
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
         theSize = myd1
         CALL MPI_Recv( arrayS(1,bi,bj), theSize, theType,
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
         theSize = myd1
         CALL MPI_Recv( arrayN(1,bi,bj), theSize, theType,
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
         southCommMode = _tileCommModeS(bi,bj)
         northCommMode = _tileCommModeN(bi,bj)
   10    CONTINUE
          CALL FOOL_THE_COMPILER
          spinCount = spinCount+1
C         IF ( myThid .EQ. 1 .AND. spinCount .GT. _EXCH_SPIN_LIMIT ) THEN
C          WRITE(0,*) ' eBl = ', ebl
C          STOP ' S/R EXCH_RECV_GET_X: spinCount .GT. _EXCH_SPIN_LIMIT'
C         ENDIF
          IF ( southRecvAck(1,bi,bj) .EQ. 0. ) GOTO 10
          IF ( northRecvAck(1,bi,bj) .EQ. 0. ) GOTO 10
C        Clear outstanding requests
         southRecvAck(1,bi,bj) = 0.
         northRecvAck(1,bi,bj) = 0.

c         IF ( exchVReqsY(1,bi,bj) .GT. 0 ) THEN
         IF ( exchNReqsY(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
         IF ( usingMPI ) THEN
#endif
c          CALL MPI_Waitall( exchVReqsY(1,bi,bj), exchReqVIdY(1,1,bi,bj),
          CALL MPI_Waitall( exchNReqsY(1,bi,bj), exchReqIdY(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#ifndef ALWAYS_USE_MPI
        ENDIF                 
#endif
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
c         exchVReqsY(1,bi,bj) = 0
         exchNReqsY(1,bi,bj) = 0
C        Update statistics
        ENDDO
       ENDDO
      ENDIF

C--   Read from the buffers
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        biN =  _tileBiN(bi,bj)
        bjN =  _tileBjN(bi,bj)
        biS =  _tileBiS(bi,bj)
        bjS =  _tileBjS(bi,bj)
        southCommMode = _tileCommModeS(bi,bj)
        northCommMode = _tileCommModeN(bi,bj)
         IF ( southCommMode .EQ. COMM_GET ) THEN
          DO I=1,myd1
             arrayN(I,bi,bj) = arrayS(I,biN,bjN)
           ENDDO
         ENDIF
         IF ( southCommMode .EQ. COMM_GET ) THEN
          DO I=1,myd1
             arrayS(I,bi,bj) = arrayN(I,biS,bjS)
          ENDDO
         ENDIF

       ENDDO
      ENDDO

      RETURN
      END

