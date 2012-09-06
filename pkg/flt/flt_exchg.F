C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_exchg.F,v 1.17 2012/09/06 16:13:53 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"
#undef DBUG_EXCH_VEC

      SUBROUTINE FLT_EXCHG (
     I                       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_EXCHG
C     ==================================================================
C     o Exchange particles between tiles.
C     started: Arne Biastoch
C     changed: Antti Westerlund antti.westerlund@helsinki.fi 2004.06.10
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

C     == shared variables ==
C-    buffer for sending/receiving variables (E/W are also used for S/N)
C     (needs to be in common block for multi-threaded)
      INTEGER imax, imax2
      PARAMETER(imax=9)
      PARAMETER(imax2=imax*max_npart_exch)
      _RL fltbuf_sendE(imax2,nSx,nSy)
      _RL fltbuf_sendW(imax2,nSx,nSy)
      _RL fltbuf_recvE(imax2,nSx,nSy)
      _RL fltbuf_recvW(imax2,nSx,nSy)
      COMMON / FLT_EXCHG_BUFF /
     &   fltbuf_sendE, fltbuf_sendW, fltbuf_recvE, fltbuf_recvW

C     == local variables ==
      INTEGER bi, bj, ic
      INTEGER ip, jp, jl, m, npNew
      INTEGER icountE, icountW, icountN, icountS
      INTEGER deleteList(max_npart_exch*2)
      _RL ilo, ihi, jlo, jhi, iNew, jNew
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef FLT_WITHOUT_X_PERIODICITY
      LOGICAL wSide, eSide
#endif /* FLT_WITHOUT_X_PERIODICITY */
#ifdef FLT_WITHOUT_Y_PERIODICITY
      LOGICAL sSide, nSide
#endif /* FLT_WITHOUT_Y_PERIODICITY */
      _RL     flt_stopped

C     == end of interface ==

C--   set the "end-time" of a stopped float
      flt_stopped = -2.
      flt_stopped = MIN( baseTime, flt_stopped )

Caw Check if there are eastern/western tiles
c     IF ( Nx.NE.sNx ) THEN
C--   for periodic domain, condition above is wrong ; needs a better test
      IF ( .TRUE. ) THEN

C--   Choose floats that have to exchanged with eastern and western tiles
C     and pack to arrays

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C initialize buffers
           DO m=1,imax2
             fltbuf_sendE(m,bi,bj) = 0.
             fltbuf_sendW(m,bi,bj) = 0.
             fltbuf_recvE(m,bi,bj) = 0.
             fltbuf_recvW(m,bi,bj) = 0.
           ENDDO

           icountE=0
           icountW=0
           jl = 0

           ilo = 0.5 _d 0
           ihi = 0.5 _d 0 + DFLOAT(sNx)
#ifdef FLT_WITHOUT_X_PERIODICITY
           wSide = myXGlobalLo+bi .LE.2
           eSide = myXGlobalLo+bi*sNx.GT.Nx
#endif /* FLT_WITHOUT_X_PERIODICITY */

           DO ip=1,npart_tile(bi,bj)

#ifdef FLT_WITHOUT_X_PERIODICITY
             IF ( eSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. ipart(ip,bi,bj).GE.ihi ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( ipart(ip,bi,bj).GE.ihi ) THEN
#else /* FLT_WITHOUT_X_PERIODICITY */
             IF ( ipart(ip,bi,bj).GE.ihi ) THEN
#endif /* FLT_WITHOUT_X_PERIODICITY */
              icountE=icountE+1
              IF ( icountE.LE.max_npart_exch ) THEN

               ic = (icountE-1)*imax
               iNew = ipart(ip,bi,bj) - DFLOAT(sNx)
               fltbuf_sendE(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendE(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendE(ic+3,bi,bj) =   iNew
               fltbuf_sendE(ic+4,bi,bj) =   jpart(ip,bi,bj)
               fltbuf_sendE(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendE(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendE(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendE(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendE(ic+9,bi,bj) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

              ENDIF
             ENDIF

#ifdef FLT_WITHOUT_X_PERIODICITY
             IF ( wSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. ipart(ip,bi,bj).LT.ilo ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( ipart(ip,bi,bj).LT.ilo ) THEN
#else /* FLT_WITHOUT_X_PERIODICITY */
             IF ( ipart(ip,bi,bj).LT.ilo ) THEN
#endif /* FLT_WITHOUT_X_PERIODICITY */
              icountW=icountW+1
              IF ( icountW.LE.max_npart_exch ) THEN

               ic = (icountW-1)*imax
               iNew = ipart(ip,bi,bj) + DFLOAT(sNx)
               fltbuf_sendW(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendW(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendW(ic+3,bi,bj) =   iNew
               fltbuf_sendW(ic+4,bi,bj) =   jpart(ip,bi,bj)
               fltbuf_sendW(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendW(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendW(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendW(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendW(ic+9,bi,bj) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

              ENDIF
             ENDIF

           ENDDO
           IF ( icountE.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCHG:',
     &         ' bi,bj=', bi, bj,
     &         ' icountE=', icountE,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountW.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCHG:',
     &         ' bi,bj=', bi, bj,
     &         ' icountW=', icountW,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountE.GT.max_npart_exch
     &     .OR. icountW.GT.max_npart_exch ) THEN
             STOP 'ABNORMAL END: S/R FLT_EXCHG'
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

         ENDDO
        ENDDO

C--   Send or Put east and west edges.

#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 0x', myIter
#endif
        CALL EXCH_SEND_PUT_VEC_X_RL(
     I                               fltbuf_sendE, fltbuf_sendW,
     O                               fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 1x', myIter
#endif
C--   Receive east/west arrays
        CALL EXCH_RECV_GET_VEC_X_RL(
     U                               fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 2x', myIter
#endif

C--   Unpack arrays on new tiles

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recvE(ic+1,bi,bj).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recvE(ic+1,bi,bj)
               tstart(jp,bi,bj) = fltbuf_recvE(ic+2,bi,bj)
               ipart( jp,bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               jpart( jp,bi,bj) = fltbuf_recvE(ic+4,bi,bj)
               kpart( jp,bi,bj) = fltbuf_recvE(ic+5,bi,bj)
               kfloat(jp,bi,bj) = fltbuf_recvE(ic+6,bi,bj)
               iup(   jp,bi,bj) = fltbuf_recvE(ic+7,bi,bj)
               itop(  jp,bi,bj) = fltbuf_recvE(ic+8,bi,bj)
               tend(  jp,bi,bj) = fltbuf_recvE(ic+9,bi,bj)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCHG:+E',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCHG'
           ENDIF

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recvW(ic+1,bi,bj).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recvW(ic+1,bi,bj)
               tstart(jp,bi,bj) = fltbuf_recvW(ic+2,bi,bj)
               ipart( jp,bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               jpart( jp,bi,bj) = fltbuf_recvW(ic+4,bi,bj)
               kpart( jp,bi,bj) = fltbuf_recvW(ic+5,bi,bj)
               kfloat(jp,bi,bj) = fltbuf_recvW(ic+6,bi,bj)
               iup(   jp,bi,bj) = fltbuf_recvW(ic+7,bi,bj)
               itop(  jp,bi,bj) = fltbuf_recvW(ic+8,bi,bj)
               tend(  jp,bi,bj) = fltbuf_recvW(ic+9,bi,bj)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCHG:+W',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCHG'
           ENDIF

         ENDDO
        ENDDO

Caw end tile check
      ENDIF

C--   Choose floats that have to exchanged with northern and southern tiles
C     and pack to arrays

Caw Check if there are northern/southern tiles
c     IF ( Ny.NE.sNy ) THEN
C--   for periodic domain, condition above is wrong ; needs a better test
      IF ( .TRUE. ) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C initialize buffers

           DO m=1,imax2
             fltbuf_sendE(m,bi,bj) = 0.
             fltbuf_sendW(m,bi,bj) = 0.
             fltbuf_recvE(m,bi,bj) = 0.
             fltbuf_recvW(m,bi,bj) = 0.
           ENDDO

           icountN=0
           icountS=0
           jl = 0

           jlo = 0.5 _d 0
           jhi = 0.5 _d 0 + DFLOAT(sNy)
#ifdef FLT_WITHOUT_Y_PERIODICITY
           sSide = myYGlobalLo+bj .LE.2
           nSide = myYGlobalLo+bj*sNy.GT.Ny
#endif /* FLT_WITHOUT_Y_PERIODICITY */

           DO ip=1,npart_tile(bi,bj)

#ifdef FLT_WITHOUT_Y_PERIODICITY
             IF ( nSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. jpart(ip,bi,bj).GE.jhi ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( jpart(ip,bi,bj).GE.jhi ) THEN
#else /* FLT_WITHOUT_Y_PERIODICITY */
             IF ( jpart(ip,bi,bj).GE.jhi ) THEN
#endif /* FLT_WITHOUT_Y_PERIODICITY */
              icountN=icountN+1
              IF ( icountN.LE.max_npart_exch ) THEN

               ic = (icountN-1)*imax
               jNew = jpart(ip,bi,bj) - DFLOAT(sNy)
               fltbuf_sendE(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendE(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendE(ic+3,bi,bj) =   ipart(ip,bi,bj)
               fltbuf_sendE(ic+4,bi,bj) =   jNew
               fltbuf_sendE(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendE(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendE(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendE(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendE(ic+9,bi,bj) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCHG,N:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yHi=', jpart(ip,bi,bj), jhi
c              CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF
             ENDIF

#ifdef FLT_WITHOUT_Y_PERIODICITY
             IF ( sSide .AND.
     &           (myTime.LE.tend(ip,bi,bj) .OR. tend(ip,bi,bj).EQ.-1.)
     &           .AND. jpart(ip,bi,bj).LT.jlo ) THEN
C          stop the float:
              tend(ip,bi,bj) = flt_stopped
             ELSEIF ( jpart(ip,bi,bj).LT.jlo ) THEN
#else /* FLT_WITHOUT_Y_PERIODICITY */
             IF ( jpart(ip,bi,bj).LT.jlo ) THEN
#endif /* FLT_WITHOUT_Y_PERIODICITY */
              icountS=icountS+1
              IF ( icountS.LE.max_npart_exch ) THEN

               ic = (icountS-1)*imax
               jNew = jpart(ip,bi,bj) + DFLOAT(sNy)
               fltbuf_sendW(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendW(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendW(ic+3,bi,bj) =   ipart(ip,bi,bj)
               fltbuf_sendW(ic+4,bi,bj) =   jNew
               fltbuf_sendW(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendW(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendW(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendW(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendW(ic+9,bi,bj) =    tend(ip,bi,bj)

C tag this float to be removed:
               jl = jl + 1
               deleteList(jl) = ip
               npart(ip,bi,bj) = 0.

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCHG,S:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yLo=', jpart(ip,bi,bj), jlo
c              CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF
             ENDIF

           ENDDO
           IF ( icountN.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCHG:',
     &         ' bi,bj=', bi, bj,
     &         ' icountN=', icountN,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountS.GT.max_npart_exch ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') ' FLT_EXCHG:',
     &         ' bi,bj=', bi, bj,
     &         ' icountS=', icountS,
     &         ' > max_npart_exch=', max_npart_exch
             CALL PRINT_ERROR( msgBuf, myThid )
           ENDIF
           IF ( icountN.GT.max_npart_exch
     &     .OR. icountS.GT.max_npart_exch ) THEN
             STOP 'ABNORMAL END: S/R FLT_EXCHG'
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

C     Send or Put north and south arrays.
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 0y', myIter
#endif
        CALL EXCH_SEND_PUT_VEC_Y_RL(
     I                               fltbuf_sendE, fltbuf_sendW,
     O                               fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 1y', myIter
#endif
C     Receive north and south arrays
        CALL EXCH_RECV_GET_VEC_Y_RL(
     U                               fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )
#ifdef DBUG_EXCH_VEC
        WRITE(errorMessageUnit,'(A,I8)') 'FLT_EXCH: 2y', myIter
c       STOP 'FLT_EXCHG: Normal End'
#endif

C--   Unpack arrays on new tiles

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recvE(ic+1,bi,bj).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recvE(ic+1,bi,bj)
               tstart(jp,bi,bj) = fltbuf_recvE(ic+2,bi,bj)
               ipart( jp,bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               jpart( jp,bi,bj) = fltbuf_recvE(ic+4,bi,bj)
               kpart( jp,bi,bj) = fltbuf_recvE(ic+5,bi,bj)
               kfloat(jp,bi,bj) = fltbuf_recvE(ic+6,bi,bj)
               iup(   jp,bi,bj) = fltbuf_recvE(ic+7,bi,bj)
               itop(  jp,bi,bj) = fltbuf_recvE(ic+8,bi,bj)
               tend(  jp,bi,bj) = fltbuf_recvE(ic+9,bi,bj)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCHG:+N',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCHG'
           ENDIF

           DO ip=1,max_npart_exch

            ic=(ip-1)*imax
            IF ( fltbuf_recvW(ic+1,bi,bj).NE.0. ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN
               jp = npart_tile(bi,bj)
               npart( jp,bi,bj) = fltbuf_recvW(ic+1,bi,bj)
               tstart(jp,bi,bj) = fltbuf_recvW(ic+2,bi,bj)
               ipart( jp,bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               jpart( jp,bi,bj) = fltbuf_recvW(ic+4,bi,bj)
               kpart( jp,bi,bj) = fltbuf_recvW(ic+5,bi,bj)
               kfloat(jp,bi,bj) = fltbuf_recvW(ic+6,bi,bj)
               iup(   jp,bi,bj) = fltbuf_recvW(ic+7,bi,bj)
               itop(  jp,bi,bj) = fltbuf_recvW(ic+8,bi,bj)
               tend(  jp,bi,bj) = fltbuf_recvW(ic+9,bi,bj)
             ENDIF
            ENDIF

           ENDDO
           IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
             WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_EXCHG:+S',
     &         ' bi,bj=', bi, bj,
     &         ' npart_tile=', npart_tile(bi,bj),
     &         ' > max_npart_tile=', max_npart_tile
              CALL PRINT_ERROR( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R FLT_EXCHG'
           ENDIF

         ENDDO
        ENDDO

Caw end tile check
      ENDIF

      RETURN
      END
