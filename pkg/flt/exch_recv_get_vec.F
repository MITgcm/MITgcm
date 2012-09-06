C $Header: /u/gcmpack/MITgcm/pkg/flt/exch_recv_get_vec.F,v 1.10 2012/09/06 16:13:53 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#undef EXCH_USE_SPINNING
#undef DBUG_EXCH_VEC

C--   Contents
C--   o EXCH_RECV_GET_VEC_X_RL
C--   o EXCH_RECV_GET_VEC_Y_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: EXCH_RECV_GET_VEC_X_RL

C !INTERFACE:
      SUBROUTINE EXCH_RECV_GET_VEC_X_RL(
     U                        arrayE, arrayW,
     I                        myd1, myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_RECV_GET_VEC_X_RL
C     | o "Receive" or "Get" X edges for RL array.
C     *==========================================================*
C     | Routine that invokes actual message passing receive
C     | of data to update buffer in X direction
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     !INPUT/OUTPUT PARAMETERS:
C     arrayE        :: buffer array to collect Eastern Neighbour values
C     arrayW        :: buffer array to collect Western Neighbour values
C     myd1          :: size
C     myThid        :: my Thread Id. number
      INTEGER myd1
      _RL arrayE(myd1, nSx, nSy), arrayW(myd1, nSx, nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     bi, bj        :: tile indices
C     biW, bjW      :: West tile indices
C     biE, bjE      :: East tile indices
C     theProc       :: Variables used in message building
C     theTag        :: Variables used in message building
C     theType       :: Variables used in message building
C     theSize       :: Variables used in message building
C     westCommMode  :: variables holding type of communication
C     eastCommMode  ::  a particular tile face uses.
      INTEGER bi, bj
c     INTEGER biW, bjW, biE, bjE
      INTEGER westCommMode
      INTEGER eastCommMode
      INTEGER ioUnit
#ifdef EXCH_USE_SPINNING
      INTEGER spinCount
#endif
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize
      INTEGER mpiStatus(MPI_STATUS_SIZE,4), mpiRc
#endif

C--   Under a "put" scenario we
C--     i. set completetion signal for buffer we put into.
C--    ii. wait for completetion signal indicating data has been put in
C--        our buffer.
C--   Under a messaging mode we "receive" the message.
C--   Under a "get" scenario <= not implemented, we
C--     i. Check that the data is ready.
C--    ii. Read the data.
C--   iii. Set data read flag + memory sync.

      ioUnit = errorMessageUnit

      _BEGIN_MASTER(myThid)

      DO bj=1,nSy
       DO bi=1,nSx
        westCommMode  = _tileCommModeW(bi,bj)
        eastCommMode  = _tileCommModeE(bi,bj)
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_X,0 :',myProcId,bi,bj
#endif
c       biE =  _tileBiE(bi,bj)
c       bjE =  _tileBjE(bi,bj)
c       biW =  _tileBiW(bi,bj)
c       bjW =  _tileBjW(bi,bj)
        IF ( westCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidW(bi,bj)
          theTag  = _tileTagRecvW(bi,bj)
          theType = _MPI_TYPE_RL
          theSize = myd1
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I5,I8)') 'qq2xW: ',myProcId,bi,bj,
     &          theProc,theTag,theSize
#endif
          CALL MPI_Recv( arrayW(1,bi,bj), theSize, theType,
     &                   theProc, theTag, MPI_COMM_MODEL,
     &                   mpiStatus, mpiRc )
c         if (theProc .eq. 0 .or. theProc .eq. 2) then
c         if (arrayW(1,bi,bj) .ne. 0.) then
c            write(errormessageunit,*) 'qq2y: ',myProcId,
c     &      theProc,theTag,theSize,(arrayW(i,bi,bj),i=1,32)
c         else
c            write(errormessageunit,*) 'qq2n: ',myProcId,
c     &      theProc,theTag,theSize,(arrayW(i,bi,bj),i=1,32)
c         endif
c         endif
         ENDIF
#endif /* ALLOW_USE_MPI */
        ENDIF
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_X,1 :',myProcId,bi,bj
#endif

        IF ( eastCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidE(bi,bj)
          theTag  = _tileTagRecvE(bi,bj)
          theType = _MPI_TYPE_RL
          theSize = myd1
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I5,I8)') 'qq2xE: ',myProcId,bi,bj,
     &          theProc,theTag,theSize
#endif
          CALL MPI_Recv( arrayE(1,bi,bj), theSize, theType,
     &                   theProc, theTag, MPI_COMM_MODEL,
     &                   mpiStatus, mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
        ENDIF
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_X,2 :',myProcId,bi,bj
#endif
       ENDDO
      ENDDO
#ifdef DBUG_EXCH_VEC
      write(ioUnit,'(A,5I6,I12)') 'RECV_X:',myProcId
#endif

      IF ( .NOT.exchUsesBarrier  ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         IF ( exchNReqsX(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
          IF ( usingMPI )
     &    CALL MPI_Waitall( exchNReqsX(1,bi,bj), exchReqIdX(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsX(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF

      _END_MASTER(myThid)

C--   need to sync threads after master has received data ;
C     (done after mpi waitall in case waitall is really needed)
      _BARRIER

C--   Threads wait for buffers I am going to read to be ready.
C     note: added BARRIER in exch_send_put S/R and here above (message
C     mode) so that we no longer needs this (undef EXCH_USE_SPINNING)
#ifdef EXCH_USE_SPINNING
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
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6,I12)') 'spin:', myProcId,bi,bj,
     &          westRecvAck(1,bi,bj), eastRecvAck(1,bi,bj), spinCount
#endif
   10    CONTINUE
          CALL FOOL_THE_COMPILER( spinCount )
          spinCount = spinCount+1
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6,I12)') 'spin:', myProcId,bi,bj,
     &          westRecvAck(1,bi,bj), eastRecvAck(1,bi,bj), spinCount
          IF ( myThid.EQ.1 .AND. spinCount.GT. _EXCH_SPIN_LIMIT ) THEN
           STOP ' S/R EXCH_RECV_GET_X: spinCount > _EXCH_SPIN_LIMIT'
          ENDIF
#endif
          IF ( westRecvAck(1,bi,bj) .EQ. 0 ) GOTO 10
          IF ( eastRecvAck(1,bi,bj) .EQ. 0 ) GOTO 10
C        Clear outstanding requests
         westRecvAck(1,bi,bj) = 0
         eastRecvAck(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF
#endif /* EXCH_USE_SPINNING */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: EXCH_RECV_GET_VEC_Y_RL

C !INTERFACE:
      SUBROUTINE EXCH_RECV_GET_VEC_Y_RL(
     U                        arrayN, arrayS,
     I                        myd1, myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_RECV_GET_VEC_Y_RL
C     | o "Receive" or "Get" Y edges for RL array.
C     *==========================================================*
C     | Routine that invokes actual message passing receive
C     | of data to update buffer in Y direction
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     !INPUT/OUTPUT PARAMETERS:
C     arrayN        :: buffer array to collect Northern Neighbour values
C     arrayS        :: buffer array to collect Southern Neighbour values
C     myd1          :: size
C     myThid        :: my Thread Id. number
      INTEGER myd1
      _RL arrayN(myd1, nSx, nSy), arrayS(myd1, nSx, nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     bi, bj        :: tile indices
C     biS, bjS      :: South tile indices
C     biN, bjN      :: North tile indices
C     theProc       :: Variables used in message building
C     theTag        :: Variables used in message building
C     theType       :: Variables used in message building
C     theSize       :: Variables used in message building
C     southCommMode :: variables holding type of communication
C     northCommMode ::  a particular tile face uses.
      INTEGER bi, bj
c     INTEGER biS, bjS, biN, bjN
      INTEGER southCommMode
      INTEGER northCommMode
      INTEGER ioUnit
#ifdef EXCH_USE_SPINNING
      INTEGER spinCount
#endif
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize
      INTEGER mpiStatus(MPI_STATUS_SIZE,4), mpiRc
#endif

C--   Under a "put" scenario we
C--     i. set completetion signal for buffer we put into.
C--    ii. wait for completetion signal indicating data has been put in
C--        our buffer.
C--   Under a messaging mode we "receive" the message.
C--   Under a "get" scenario <= not implemented, we
C--     i. Check that the data is ready.
C--    ii. Read the data.
C--   iii. Set data read flag + memory sync.

      ioUnit = errorMessageUnit

      _BEGIN_MASTER(myThid)

      DO bj=1,nSy
       DO bi=1,nSx
        southCommMode  = _tileCommModeS(bi,bj)
        northCommMode  = _tileCommModeN(bi,bj)
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_Y,0 :',myProcId,bi,bj
#endif
c       biN =  _tileBiN(bi,bj)
c       bjN =  _tileBjN(bi,bj)
c       biS =  _tileBiS(bi,bj)
c       bjS =  _tileBjS(bi,bj)
        IF ( southCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidS(bi,bj)
          theTag  = _tileTagRecvS(bi,bj)
          theType = _MPI_TYPE_RL
          theSize = myd1
          CALL MPI_Recv( arrayS(1,bi,bj), theSize, theType,
     &                   theProc, theTag, MPI_COMM_MODEL,
     &                   mpiStatus, mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
        ENDIF
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_Y,1 :',myProcId,bi,bj
#endif

        IF ( northCommMode .EQ. COMM_MSG ) THEN
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidN(bi,bj)
          theTag  = _tileTagRecvN(bi,bj)
          theType = _MPI_TYPE_RL
          theSize = myd1
          CALL MPI_Recv( arrayN(1,bi,bj), theSize, theType,
     &                   theProc, theTag, MPI_COMM_MODEL,
     &                   mpiStatus, mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
        ENDIF
#ifdef DBUG_EXCH_VEC
        write(ioUnit,'(A,5I6)') 'RECV_Y,2 :',myProcId,bi,bj
#endif
       ENDDO
      ENDDO
#ifdef DBUG_EXCH_VEC
      write(ioUnit,'(A,5I6,I12)') 'RECV_Y:',myProcId
#endif

C--   Processes wait for buffers I am going to read to be ready.
      IF ( .NOT.exchUsesBarrier  ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         IF ( exchNReqsY(1,bi,bj) .GT. 0 ) THEN
#ifdef ALLOW_USE_MPI
          IF ( usingMPI )
     &    CALL MPI_Waitall( exchNReqsY(1,bi,bj), exchReqIdY(1,1,bi,bj),
     &                      mpiStatus, mpiRC )
#endif /* ALLOW_USE_MPI */
         ENDIF
C        Clear outstanding requests counter
         exchNReqsY(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF

      _END_MASTER(myThid)

C--   need to sync threads after master has received data ;
C     (done after mpi waitall in case waitall is really needed)
      _BARRIER

C--   Threads wait for buffers I am going to read to be ready.
C     note: added BARRIER in exch_send_put S/R and here above (message
C     mode) so that we no longer needs this (undef EXCH_USE_SPINNING)
#ifdef EXCH_USE_SPINNING
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
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6,I12)') 'spin:', myProcId,bi,bj,
     &          southRecvAck(1,bi,bj), northRecvAck(1,bi,bj), spinCount
#endif
   10    CONTINUE
          CALL FOOL_THE_COMPILER( spinCount )
          spinCount = spinCount+1
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6,I12)') 'spin:', myProcId,bi,bj,
     &          southRecvAck(1,bi,bj), northRecvAck(1,bi,bj), spinCount
          IF ( myThid.EQ.1 .AND. spinCount.GT. _EXCH_SPIN_LIMIT ) THEN
           STOP ' S/R EXCH_RECV_GET_X: spinCount > _EXCH_SPIN_LIMIT'
          ENDIF
#endif
          IF ( southRecvAck(1,bi,bj) .EQ. 0 ) GOTO 10
          IF ( northRecvAck(1,bi,bj) .EQ. 0 ) GOTO 10
C        Clear outstanding requests
         southRecvAck(1,bi,bj) = 0
         northRecvAck(1,bi,bj) = 0
        ENDDO
       ENDDO
      ENDIF
#endif /* EXCH_USE_SPINNING */

      RETURN
      END
