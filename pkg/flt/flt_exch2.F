C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_exch2.F,v 1.3 2012/09/06 16:14:28 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"
#undef DBUG_EXCH_VEC

      SUBROUTINE FLT_EXCH2 (
     I                       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_EXCH2
C     ==================================================================
C     o Exchange particles between tiles.
C     started: Arne Biastoch
C     changed: Antti Westerlund antti.westerlund@helsinki.fi 2004.06.10
C     adapted to exch2: Oliver Jahn 2010.09
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "FLT_SIZE.h"
#include "FLT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_PARAMS.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif

C     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_EXCH2

C     == local variables ==
      INTEGER bi, bj, ic
      INTEGER ip, jp, jl, npNew
      INTEGER icountE, icountW, icountN, icountS
      INTEGER deleteList(max_npart_exch*2)
      INTEGER imax, imax2, m
      INTEGER N, nT, ipass, myFace
      INTEGER e2_msgHandles( 2, W2_maxNeighbours, nSx, nSy )
      _RL ilo, ihi, jlo, jhi, iNew, jNew
      PARAMETER(imax=9)
      PARAMETER(imax2=imax*max_npart_exch)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     buffer for sending/receiving variables (4 levels <-> N,S,E,W)
      COMMON/FLTBUF/fltbuf_send,fltbuf_recv
      _RL fltbuf_send(imax2,nSx,nSy,4)
      _RL fltbuf_recv(imax2,nSx,nSy,4)
      LOGICAL wSide, eSide, sSide, nSide
      _RL     flt_stopped

C     == end of interface ==

C have to do 2 passes to get into tiles diagonally across
      DO ipass=1,2

C     Prevent anyone to access shared buffer while an other thread modifies it
C--   not needed here since send buffer is different fron recv buffer
C     (which is not the case for usual 3-D field exch in EXCH2)
c       CALL BAR2( myThid )

C--   Choose floats that have to exchanged with eastern and western tiles
C     and pack to arrays

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           nT = W2_myTileList(bi,bj)
           myFace = exch2_myFace(nT)

C initialize buffers
           DO N=1,4
            DO m=1,imax2
             fltbuf_send(m,bi,bj,N) = 0.
             fltbuf_recv(m,bi,bj,N) = 0.
            ENDDO
           ENDDO

           icountE=0
           icountW=0
           jl = 0

           ilo = 0.5 _d 0
           ihi = 0.5 _d 0 + DFLOAT(sNx)
           wSide = exch2_isWedge(nT).EQ.1
     &       .AND. facet_link(W2_WEST,myFace).EQ.0.
           eSide = exch2_isEedge(nT).EQ.1
     &       .AND. facet_link(W2_EAST,myFace).EQ.0.
           flt_stopped = -2.
           flt_stopped = MIN( baseTime, flt_stopped )

           DO ip=1,npart_tile(bi,bj)

             IF ( eSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. ipart(ip,bi,bj).GE.ihi ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( ipart(ip,bi,bj).GE.ihi ) THEN
              icountE=icountE+1
              IF ( icountE.LE.max_npart_exch ) THEN

               ic = (icountE-1)*imax
               iNew = ipart(ip,bi,bj) - DFLOAT(sNx)
               fltbuf_send(ic+1,bi,bj,W2_EAST) =   npart(ip,bi,bj)
               fltbuf_send(ic+2,bi,bj,W2_EAST) =  tstart(ip,bi,bj)
               fltbuf_send(ic+3,bi,bj,W2_EAST) =   iNew
               fltbuf_send(ic+4,bi,bj,W2_EAST) =   jpart(ip,bi,bj)
               fltbuf_send(ic+5,bi,bj,W2_EAST) =   kpart(ip,bi,bj)
               fltbuf_send(ic+6,bi,bj,W2_EAST) =  kfloat(ip,bi,bj)
               fltbuf_send(ic+7,bi,bj,W2_EAST) =     iup(ip,bi,bj)
               fltbuf_send(ic+8,bi,bj,W2_EAST) =    itop(ip,bi,bj)
               fltbuf_send(ic+9,bi,bj,W2_EAST) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

              ENDIF
             ENDIF

             IF ( wSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. ipart(ip,bi,bj).LT.ilo ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( ipart(ip,bi,bj).LT.ilo ) THEN
              icountW=icountW+1
              IF ( icountW.LE.max_npart_exch ) THEN

               ic = (icountW-1)*imax
               iNew = ipart(ip,bi,bj) + DFLOAT(sNx)
               fltbuf_send(ic+1,bi,bj,W2_WEST) =   npart(ip,bi,bj)
               fltbuf_send(ic+2,bi,bj,W2_WEST) =  tstart(ip,bi,bj)
               fltbuf_send(ic+3,bi,bj,W2_WEST) =   iNew
               fltbuf_send(ic+4,bi,bj,W2_WEST) =   jpart(ip,bi,bj)
               fltbuf_send(ic+5,bi,bj,W2_WEST) =   kpart(ip,bi,bj)
               fltbuf_send(ic+6,bi,bj,W2_WEST) =  kfloat(ip,bi,bj)
               fltbuf_send(ic+7,bi,bj,W2_WEST) =     iup(ip,bi,bj)
               fltbuf_send(ic+8,bi,bj,W2_WEST) =    itop(ip,bi,bj)
               fltbuf_send(ic+9,bi,bj,W2_WEST) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

              ENDIF
             ENDIF

           ENDDO
           IF ( icountE.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCH2:',
     &         ' bi,bj=', bi, bj,
     &         ' icountE=', icountE,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountW.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCH2:',
     &         ' bi,bj=', bi, bj,
     &         ' icountW=', icountW,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountE.GT.max_npart_exch
     &     .OR. icountW.GT.max_npart_exch ) THEN
             STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF
           IF ( (icountE+icountW).GT.0 ) THEN
C Remove from this tile-list, floats which have been sent to an other tile
             npNew = npart_tile(bi,bj) - (icountE+icountW)
             jl = 0
             DO jp = npNew+1,npart_tile(bi,bj)
              IF ( npart(jp,bi,bj).NE.0. _d 0 ) THEN
                jl = jl + 1
                ip = deleteList(jl)
C        copy: ip <-- jp
                npart (ip,bi,bj) =  npart (jp,bi,bj)
                tstart(ip,bi,bj) =  tstart(jp,bi,bj)
                ipart (ip,bi,bj) =  ipart (jp,bi,bj)
                jpart (ip,bi,bj) =  jpart (jp,bi,bj)
                kpart (ip,bi,bj) =  kpart (jp,bi,bj)
                kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
                iup   (ip,bi,bj) =  iup   (jp,bi,bj)
                itop  (ip,bi,bj) =  itop  (jp,bi,bj)
                tend  (ip,bi,bj) =  tend  (jp,bi,bj)
              ENDIF
             ENDDO
             npart_tile(bi,bj) = npNew
           ENDIF

           icountN=0
           icountS=0
           jl = 0

           jlo = 0.5 _d 0
           jhi = 0.5 _d 0 + DFLOAT(sNy)
           sSide = exch2_isSedge(nT).EQ.1
     &       .AND. facet_link(W2_SOUTH,myFace).EQ.0.
           nSide = exch2_isNedge(nT).EQ.1
     &       .AND. facet_link(W2_NORTH,myFace).EQ.0.
           flt_stopped = -2.
           flt_stopped = MIN( baseTime, flt_stopped )

           DO ip=1,npart_tile(bi,bj)

            IF ( npart(ip,bi,bj).NE.0 ) THEN

             IF ( nSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. jpart(ip,bi,bj).GE.jhi ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( jpart(ip,bi,bj).GE.jhi ) THEN
              icountN=icountN+1
              IF ( icountN.LE.max_npart_exch ) THEN

               ic = (icountN-1)*imax
               jNew = jpart(ip,bi,bj) - DFLOAT(sNy)
               fltbuf_send(ic+1,bi,bj,W2_NORTH) =   npart(ip,bi,bj)
               fltbuf_send(ic+2,bi,bj,W2_NORTH) =  tstart(ip,bi,bj)
               fltbuf_send(ic+3,bi,bj,W2_NORTH) =   ipart(ip,bi,bj)
               fltbuf_send(ic+4,bi,bj,W2_NORTH) =   jNew
               fltbuf_send(ic+5,bi,bj,W2_NORTH) =   kpart(ip,bi,bj)
               fltbuf_send(ic+6,bi,bj,W2_NORTH) =  kfloat(ip,bi,bj)
               fltbuf_send(ic+7,bi,bj,W2_NORTH) =     iup(ip,bi,bj)
               fltbuf_send(ic+8,bi,bj,W2_NORTH) =    itop(ip,bi,bj)
               fltbuf_send(ic+9,bi,bj,W2_NORTH) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCH2,N:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yHi=', jpart(ip,bi,bj), jhi
c              CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF
             ENDIF

             IF ( sSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. jpart(ip,bi,bj).LT.jlo ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( jpart(ip,bi,bj).LT.jlo ) THEN
              icountS=icountS+1
              IF ( icountS.LE.max_npart_exch ) THEN

               ic = (icountS-1)*imax
               jNew = jpart(ip,bi,bj) + DFLOAT(sNy)
               fltbuf_send(ic+1,bi,bj,W2_SOUTH) =   npart(ip,bi,bj)
               fltbuf_send(ic+2,bi,bj,W2_SOUTH) =  tstart(ip,bi,bj)
               fltbuf_send(ic+3,bi,bj,W2_SOUTH) =   ipart(ip,bi,bj)
               fltbuf_send(ic+4,bi,bj,W2_SOUTH) =   jNew
               fltbuf_send(ic+5,bi,bj,W2_SOUTH) =   kpart(ip,bi,bj)
               fltbuf_send(ic+6,bi,bj,W2_SOUTH) =  kfloat(ip,bi,bj)
               fltbuf_send(ic+7,bi,bj,W2_SOUTH) =     iup(ip,bi,bj)
               fltbuf_send(ic+8,bi,bj,W2_SOUTH) =    itop(ip,bi,bj)
               fltbuf_send(ic+9,bi,bj,W2_SOUTH) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCH2,S:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yLo=', jpart(ip,bi,bj), jlo
c              CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF
             ENDIF

            ENDIF

           ENDDO
           IF ( icountN.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCH2:',
     &         ' bi,bj=', bi, bj,
     &         ' icountN=', icountN,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountS.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCH2:',
     &         ' bi,bj=', bi, bj,
     &         ' icountS=', icountS,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountN.GT.max_npart_exch
     &     .OR. icountS.GT.max_npart_exch ) THEN
             STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF
           IF ( (icountN+icountS).GT.0 ) THEN
C Remove from this tile-list, floats which have been sent to an other tile
             npNew = npart_tile(bi,bj) - (icountN+icountS)
             jl = 0
             DO jp = npNew+1,npart_tile(bi,bj)
              IF ( npart(jp,bi,bj).NE.0. _d 0 ) THEN
                jl = jl + 1
                ip = deleteList(jl)
C        copy: ip <-- jp
                npart (ip,bi,bj) =  npart (jp,bi,bj)
                tstart(ip,bi,bj) =  tstart(jp,bi,bj)
                ipart (ip,bi,bj) =  ipart (jp,bi,bj)
                jpart (ip,bi,bj) =  jpart (jp,bi,bj)
                kpart (ip,bi,bj) =  kpart (jp,bi,bj)
                kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
                iup   (ip,bi,bj) =  iup   (jp,bi,bj)
                itop  (ip,bi,bj) =  itop  (jp,bi,bj)
                tend  (ip,bi,bj) =  tend  (jp,bi,bj)
              ENDIF
             ENDDO
             npart_tile(bi,bj) = npNew
           ENDIF

         ENDDO
        ENDDO

C     Prevent anyone to access shared buffer while an other thread modifies it
        _BARRIER

C--   Send or Put east and west edges.

#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 0x', myIter
#endif
        CALL EXCH2_SEND_PUT_VEC_RL(
     I                               fltbuf_send,
     O                               fltbuf_recv,
     O                               e2_msgHandles(1,1,1,1),
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH2: 1x', myIter
#endif

#ifdef ALLOW_USE_MPI
        IF ( usingMPI ) THEN
C--   Receive east/west arrays
         CALL EXCH2_RECV_GET_VEC_RL(
     U                               fltbuf_recv,
     I                               e2_msgHandles(1,1,1,1),
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
         WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH2: 2x', myIter
#endif
        ENDIF
#endif /* ALLOW_USE_MPI */

C--   need to sync threads after master has received data ;
C     (done after mpi waitall in case waitall is really needed)
        _BARRIER

C--   Unpack arrays on new tiles

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recv(ic+1,bi,bj,W2_EAST).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recv(ic+1,bi,bj,W2_EAST)
               tstart(jp,bi,bj) = fltbuf_recv(ic+2,bi,bj,W2_EAST)
               ipart( jp,bi,bj) = fltbuf_recv(ic+3,bi,bj,W2_EAST)
               jpart( jp,bi,bj) = fltbuf_recv(ic+4,bi,bj,W2_EAST)
               kpart( jp,bi,bj) = fltbuf_recv(ic+5,bi,bj,W2_EAST)
               kfloat(jp,bi,bj) = fltbuf_recv(ic+6,bi,bj,W2_EAST)
               iup(   jp,bi,bj) = fltbuf_recv(ic+7,bi,bj,W2_EAST)
               itop(  jp,bi,bj) = fltbuf_recv(ic+8,bi,bj,W2_EAST)
               tend(  jp,bi,bj) = fltbuf_recv(ic+9,bi,bj,W2_EAST)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCH2:+E',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recv(ic+1,bi,bj,W2_WEST).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recv(ic+1,bi,bj,W2_WEST)
               tstart(jp,bi,bj) = fltbuf_recv(ic+2,bi,bj,W2_WEST)
               ipart( jp,bi,bj) = fltbuf_recv(ic+3,bi,bj,W2_WEST)
               jpart( jp,bi,bj) = fltbuf_recv(ic+4,bi,bj,W2_WEST)
               kpart( jp,bi,bj) = fltbuf_recv(ic+5,bi,bj,W2_WEST)
               kfloat(jp,bi,bj) = fltbuf_recv(ic+6,bi,bj,W2_WEST)
               iup(   jp,bi,bj) = fltbuf_recv(ic+7,bi,bj,W2_WEST)
               itop(  jp,bi,bj) = fltbuf_recv(ic+8,bi,bj,W2_WEST)
               tend(  jp,bi,bj) = fltbuf_recv(ic+9,bi,bj,W2_WEST)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCH2:+W',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recv(ic+1,bi,bj,W2_NORTH).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recv(ic+1,bi,bj,W2_NORTH)
               tstart(jp,bi,bj) = fltbuf_recv(ic+2,bi,bj,W2_NORTH)
               ipart( jp,bi,bj) = fltbuf_recv(ic+3,bi,bj,W2_NORTH)
               jpart( jp,bi,bj) = fltbuf_recv(ic+4,bi,bj,W2_NORTH)
               kpart( jp,bi,bj) = fltbuf_recv(ic+5,bi,bj,W2_NORTH)
               kfloat(jp,bi,bj) = fltbuf_recv(ic+6,bi,bj,W2_NORTH)
               iup(   jp,bi,bj) = fltbuf_recv(ic+7,bi,bj,W2_NORTH)
               itop(  jp,bi,bj) = fltbuf_recv(ic+8,bi,bj,W2_NORTH)
               tend(  jp,bi,bj) = fltbuf_recv(ic+9,bi,bj,W2_NORTH)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCH2:+N',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recv(ic+1,bi,bj,W2_SOUTH).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recv(ic+1,bi,bj,W2_SOUTH)
               tstart(jp,bi,bj) = fltbuf_recv(ic+2,bi,bj,W2_SOUTH)
               ipart( jp,bi,bj) = fltbuf_recv(ic+3,bi,bj,W2_SOUTH)
               jpart( jp,bi,bj) = fltbuf_recv(ic+4,bi,bj,W2_SOUTH)
               kpart( jp,bi,bj) = fltbuf_recv(ic+5,bi,bj,W2_SOUTH)
               kfloat(jp,bi,bj) = fltbuf_recv(ic+6,bi,bj,W2_SOUTH)
               iup(   jp,bi,bj) = fltbuf_recv(ic+7,bi,bj,W2_SOUTH)
               itop(  jp,bi,bj) = fltbuf_recv(ic+8,bi,bj,W2_SOUTH)
               tend(  jp,bi,bj) = fltbuf_recv(ic+9,bi,bj,W2_SOUTH)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCH2:+S',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCH2'
           ENDIF

         ENDDO
        ENDDO

C ipass
      ENDDO

#endif /* ALLOW_EXCH2 */

      RETURN
      END
