C $Header: /u/gcmpack/MITgcm/pkg/flt/exch_send_put_vec.F,v 1.9 2012/09/06 16:13:53 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#undef DBUG_EXCH_VEC

C--   Contents
C--   o EXCH_SEND_PUT_VEC_X_RL
C--   o EXCH_SEND_PUT_VEC_Y_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: EXCH_SEND_PUT_VEC_X_RL

C !INTERFACE:
      SUBROUTINE EXCH_SEND_PUT_VEC_X_RL(
     I                        arrayE, arrayW,
     O                        bufRecE, bufRecW,
     I                        myd1, myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_SEND_PUT_VEC_X_RL
C     | o "Send" or "put" X edges for RL array.
C     *==========================================================*
C     | Routine that invokes actual message passing send or
C     |   direct "put" of data to update buffer in X direction
C     | Note: Since only master-thread send/put, assumes input
C     |       & output arrays are shared (i.e. incommon block)
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     !INPUT/OUTPUT PARAMETERS:
C     arrayE        :: Input buffer array to send to Eastern Neighbour
C     arrayW        :: Input buffer array to send to Western Neighbour
C     bufRecE       :: buffer array to collect Eastern Neighbour values
C     bufRecW       :: buffer array to collect Western Neighbour values
C     myd1          :: size
C     myThid        :: my Thread Id. number
      INTEGER myd1
      _RL  arrayE(myd1,nSx,nSy),  arrayW(myd1,nSx,nSy)
      _RL bufRecE(myd1,nSx,nSy), bufRecW(myd1,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     I             :: Loop counters
C     bi, bj        :: tile indices
C     biW, bjW      :: West tile indices
C     biE, bjE      :: East tile indices
C     theProc       :: Variables used in message building
C     theTag        :: Variables used in message building
C     theType       :: Variables used in message building
C     theSize       :: Variables used in message building
C     westCommMode  :: variables holding type of communication
C     eastCommMode  ::  a particular tile face uses.
      INTEGER I
      INTEGER bi, bj, biW, bjW, biE, bjE
      INTEGER westCommMode
      INTEGER eastCommMode
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize, mpiRc
#endif
#ifdef DBUG_EXCH_VEC
      INTEGER ioUnit
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
C                Write data-ready Ack for east edge of west-neighbor tile
C                Write data-ready Ack for west edge of east-neighbor tile
C                Sync. memory

C     Prevent anyone to access shared buffer while an other thread modifies it
      _BARRIER

      _BEGIN_MASTER(myThid)

#ifdef DBUG_EXCH_VEC
      ioUnit = errorMessageUnit
      WRITE(ioUnit,'(A,2L5)')
     &    'SEND_PUT_X: exchNeedsMemsync,exchUsesBarrier=',
     &     exchNeedsMemsync,exchUsesBarrier
#endif

      DO bj=1,nSy
       DO bi=1,nSx

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
         IF ( usingMPI ) THEN
          theProc = tilePidW(bi,bj)
          theTag  = _tileTagSendW(bi,bj)
          theSize = myd1
          theType = _MPI_TYPE_RL
          exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,7I5,I8)') 'qq1xW: ',myProcId,bi,bj,
     &          theProc,theTag, exchNReqsX(1,bi,bj),
     &          exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), theSize
#endif
          CALL MPI_Isend( arrayW(1,bi,bj), theSize, theType,
     &                    theProc, theTag, MPI_COMM_MODEL,
     &                    exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj),
     &                    mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
         eastRecvAck(1,biW,bjW) = 1
        ELSEIF ( westCommMode .EQ. COMM_PUT  ) THEN
c         write(0,*) 'SEND_PUT_VEC_X: copy E:',biW,bjW,' <- W:',bi,bj
          DO I=1,myd1
            bufRecE(I,biW,bjW) = arrayW(I,bi,bj)
          ENDDO
        ELSEIF ( westCommMode .NE. COMM_NONE ) THEN
         STOP ' S/R EXCH: Invalid commW mode.'
        ENDIF

C       o Send or Put east edge
        IF ( eastCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidE(bi,bj)
          theTag  = _tileTagSendE(bi,bj)
          theSize = myd1
          theType = _MPI_TYPE_RL
          exchNReqsX(1,bi,bj) = exchNReqsX(1,bi,bj)+1
#ifdef DBUG_EXCH_VEC
c         if (theProc .eq. 2 .or. theProc .eq. 4) then
c         if (arrayE(1,bi,bj) .ne. 0.) then
          write(ioUnit,'(A,7I5,I8)') 'qq1xE: ',myProcId,bi,bj,
     &       theProc,theTag, exchNReqsX(1,bi,bj),
     &       exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj), theSize
c         endif
c         endif
#endif
          CALL MPI_Isend( arrayE(1,bi,bj), theSize, theType,
     &                    theProc, theTag, MPI_COMM_MODEL,
     &                    exchReqIdX(exchNReqsX(1,bi,bj),1,bi,bj),
     &                    mpiRc)
         ENDIF
#endif /* ALLOW_USE_MPI */
         westRecvAck(1,biE,bjE) = 1
        ELSEIF ( eastCommMode .EQ. COMM_PUT  ) THEN
c         write(0,*) 'SEND_PUT_VEC_X: copy W:',biE,bjE,' <- E:',bi,bj
          DO I=1,myd1
            bufRecW(I,biE,bjE) = arrayE(I,bi,bj)
          ENDDO
        ELSEIF ( eastCommMode .NE. COMM_NONE ) THEN
         STOP ' S/R EXCH: Invalid commE mode.'
        ENDIF

       ENDDO
      ENDDO

      _END_MASTER(myThid)

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
        IF ( westCommMode .EQ. COMM_PUT ) eastRecvAck(1,biW,bjW) = 1
        IF ( eastCommMode .EQ. COMM_PUT ) westRecvAck(1,biE,bjE) = 1
        IF ( westCommMode .EQ. COMM_GET ) eastRecvAck(1,biW,bjW) = 1
        IF ( eastCommMode .EQ. COMM_GET ) westRecvAck(1,biE,bjE) = 1
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

C     Wait until all threads finish filling buffer <-- jmc: really needed ?
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: EXCH_SEND_PUT_VEC_Y_RL

C !INTERFACE:
      SUBROUTINE EXCH_SEND_PUT_VEC_Y_RL(
     I                        arrayN, arrayS,
     O                        bufRecN, bufRecS,
     I                        myd1, myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_SEND_PUT_VEC_Y_RL
C     | o "Send" or "put" Y edges for RL array.
C     *==========================================================*
C     | Routine that invokes actual message passing send or
C     |   direct "put" of data to update buffer in X direction
C     | Note: Since only master-thread send/put, assumes input
C     |       & output arrays are shared (i.e. incommon block)
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
C     arrayN        :: Input buffer array to send to Northern Neighbour
C     arrayS        :: Input buffer array to send to Southern Neighbour
C     bufRecN       :: buffer array to collect Northern Neighbour values
C     bufRecS       :: buffer array to collect Southern Neighbour values
C     myd1          :: size
C     myThid        :: my Thread Id. number
      INTEGER myd1
      _RL  arrayN(myd1,nSx,nSy),  arrayS(myd1,nSx,nSy)
      _RL bufRecN(myd1,nSx,nSy), bufRecS(myd1,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     I             :: Loop index
C     bi, bj        :: tile indices
C     biS, bjS      :: South tile indices
C     biN, bjN      :: North tile indices
C     theProc       :: Variables used in message building
C     theTag        :: Variables used in message building
C     theType       :: Variables used in message building
C     theSize       :: Variables used in message building
C     southCommMode :: variables holding type of communication
C     northCommMode ::  a particular tile face uses.
      INTEGER I
      INTEGER bi, bj, biS, bjS, biN, bjN
      INTEGER southCommMode
      INTEGER northCommMode
#ifdef ALLOW_USE_MPI
      INTEGER theProc, theTag, theType, theSize, mpiRc
#endif
#ifdef DBUG_EXCH_VEC
      INTEGER ioUnit
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
C                Write data-ready Ack for east edge of south-neighbor tile
C                Write data-ready Ack for west edge of north-neighbor tile
C                Sync. memory

C     Prevent anyone to access shared buffer while an other thread modifies it
      _BARRIER

      _BEGIN_MASTER(myThid)

#ifdef DBUG_EXCH_VEC
      ioUnit = errorMessageUnit
#endif

      DO bj=1,nSy
       DO bi=1,nSx

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
         IF ( usingMPI ) THEN
          theProc = tilePidS(bi,bj)
          theTag  = _tileTagSendS(bi,bj)
          theSize = myd1
          theType = _MPI_TYPE_RL
          exchNReqsY(1,bi,bj) = exchNReqsY(1,bi,bj)+1
          CALL MPI_Isend( arrayS(1,bi,bj), theSize, theType,
     &                    theProc, theTag, MPI_COMM_MODEL,
     &                    exchReqIdY(exchNReqsY(1,bi,bj),1,bi,bj),
     &                    mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
         northRecvAck(1,biS,bjS) = 1
        ELSEIF ( southCommMode .EQ. COMM_PUT  ) THEN
c         write(0,*) 'SEND_PUT_VEC_Y: copy N:',biS,bjS,' <- S:',bi,bj
          DO I=1,myd1
            bufRecN(I,biS,bjS) = arrayS(I,bi,bj)
          ENDDO
        ELSEIF ( southCommMode .NE. COMM_NONE ) THEN
         STOP ' S/R EXCH: Invalid commS mode.'
        ENDIF

C       o Send or Put north edge
        IF ( northCommMode .EQ. COMM_MSG  ) THEN
C        Send the data
#ifdef ALLOW_USE_MPI
         IF ( usingMPI ) THEN
          theProc = tilePidN(bi,bj)
          theTag  = _tileTagSendN(bi,bj)
          theSize = myd1
          theType = _MPI_TYPE_RL
          exchNReqsY(1,bi,bj) = exchNReqsY(1,bi,bj)+1
          CALL MPI_Isend( arrayN(1,bi,bj), theSize, theType,
     &                    theProc, theTag, MPI_COMM_MODEL,
     &                    exchReqIdY(exchNReqsY(1,bi,bj),1,bi,bj),
     &                    mpiRc )
         ENDIF
#endif /* ALLOW_USE_MPI */
         southRecvAck(1,biN,bjN) = 1
        ELSEIF ( northCommMode .EQ. COMM_PUT  ) THEN
c         write(0,*) 'SEND_PUT_VEC_Y: copy S:',biN,bjN,' <- N:',bi,bj
          DO I=1,myd1
            bufRecS(I,biN,bjN) = arrayN(I,bi,bj)
          ENDDO
        ELSEIF ( northCommMode .NE. COMM_NONE ) THEN
         STOP ' S/R EXCH: Invalid commN mode.'
        ENDIF

       ENDDO
      ENDDO

      _END_MASTER(myThid)

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
        southCommMode = _tileCommModeS(bi,bj)
        northCommMode = _tileCommModeN(bi,bj)
        IF ( southCommMode .EQ. COMM_PUT ) northRecvAck(1,biS,bjS) = 1
        IF ( northCommMode .EQ. COMM_PUT ) southRecvAck(1,biN,bjN) = 1
        IF ( southCommMode .EQ. COMM_GET ) northRecvAck(1,biS,bjS) = 1
        IF ( northCommMode .EQ. COMM_GET ) southRecvAck(1,biN,bjN) = 1
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

C     Wait until all threads finish filling buffer <-- jmc: really needed ?
      _BARRIER

      RETURN
      END
