C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_exchg.F,v 1.5 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_EXCHG (
     I                       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE flt_exchg
C     ==================================================================
C
C     o Exchange particles between tiles.
C
C     started: Arne Biastoch
C
C     changed: Antti Westerlund antti.westerlund@helsinki.fi 2004.06.10
C
C     ==================================================================
C     SUBROUTINE flt_exchg
C     ==================================================================

C     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
#include "FLT.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==

      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==

      INTEGER bi, bj, ic
      INTEGER ip, jp
      INTEGER icountE, icountW, icountN, icountS
      INTEGER imax, imax2, m, iG, jG
      _RL xlo, xhi, ylo, yhi
      PARAMETER(imax=9)
      PARAMETER(imax2=imax*max_npart_exch)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     buffer for sending/receiving variables (E/W are also used for S/N)
      _RL fltbuf_sendE(imax2,nSx,nSy)
      _RL fltbuf_sendW(imax2,nSx,nSy)
      _RL fltbuf_recvE(imax2,nSx,nSy)
      _RL fltbuf_recvW(imax2,nSx,nSy)

C     == end of interface ==

Caw Check if there are eastern/western tiles
      IF ( Nx.NE.sNx ) THEN

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

           iG = myXGlobalLo + (bi-1)*sNx
           xlo = xc(1,  1,  bi,bj) - delX(iG)
           xhi = xc(sNx,1,bi,bj)   + delX(iG+sNx-1)

           DO ip=1,npart_tile(bi,bj)

             IF ( xpart(ip,bi,bj).GE.xhi ) THEN
              icountE=icountE+1
              IF ( icountE.LE.max_npart_exch ) THEN

               ic=(icountE-1)*imax
               fltbuf_sendE(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendE(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendE(ic+3,bi,bj) =   xpart(ip,bi,bj)
               fltbuf_sendE(ic+4,bi,bj) =   ypart(ip,bi,bj)
               fltbuf_sendE(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendE(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendE(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendE(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendE(ic+9,bi,bj) =    tend(ip,bi,bj)

C-- jmc: we have a problem here when the last float of the list
C        is also outside [xlo,xhi] : This needs a fix.
               jp = npart_tile(bi,bj)
               npart (ip,bi,bj) =  npart (jp,bi,bj)
               tstart(ip,bi,bj) =  tstart(jp,bi,bj)
               xpart (ip,bi,bj) =  xpart (jp,bi,bj)
               ypart (ip,bi,bj) =  ypart (jp,bi,bj)
               kpart (ip,bi,bj) =  kpart (jp,bi,bj)
               kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
               iup   (ip,bi,bj) =  iup   (jp,bi,bj)
               itop  (ip,bi,bj) =  itop  (jp,bi,bj)
               tend  (ip,bi,bj) =  tend  (jp,bi,bj)

               npart_tile(bi,bj) = jp - 1

              ENDIF
             ENDIF

             IF ( xpart(ip,bi,bj).LE.xlo ) THEN
              icountW=icountW+1
              IF ( icountW.LE.max_npart_exch ) THEN

               ic=(icountW-1)*imax
               fltbuf_sendW(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendW(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendW(ic+3,bi,bj) =   xpart(ip,bi,bj)
               fltbuf_sendW(ic+4,bi,bj) =   ypart(ip,bi,bj)
               fltbuf_sendW(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendW(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendW(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendW(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendW(ic+9,bi,bj) =    tend(ip,bi,bj)

               jp = npart_tile(bi,bj)
               npart (ip,bi,bj) =  npart (jp,bi,bj)
               tstart(ip,bi,bj) =  tstart(jp,bi,bj)
               xpart (ip,bi,bj) =  xpart (jp,bi,bj)
               ypart (ip,bi,bj) =  ypart (jp,bi,bj)
               kpart (ip,bi,bj) =  kpart (jp,bi,bj)
               kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
               iup   (ip,bi,bj) =  iup   (jp,bi,bj)
               itop  (ip,bi,bj) =  itop  (jp,bi,bj)
               tend  (ip,bi,bj) =  tend  (jp,bi,bj)

               npart_tile(bi,bj) = jp - 1

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

         ENDDO
        ENDDO

C--   "Put" east and west edges.
        CALL EXCH_RL_SEND_PUT_VEC_X( fltbuf_sendE, fltbuf_sendW,
     I                               imax2, myThid )
C--   Receive east/west arrays
        CALL EXCH_RL_RECV_GET_VEC_X( fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )

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
               xpart( jp,bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               ypart( jp,bi,bj) = fltbuf_recvE(ic+4,bi,bj)
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
               xpart( jp,bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               ypart( jp,bi,bj) = fltbuf_recvW(ic+4,bi,bj)
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
      IF ( Ny.NE.sNy ) THEN

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

           jG = myYGlobalLo + (bj-1)*sNy
           ylo = yc(1,  1,  bi,bj) - delY(jG)
           yhi = yc(1,sNy,bi,bj)   + delY(jG+sNy-1)

           DO ip=1,npart_tile(bi,bj)

             IF (ypart(ip,bi,bj) .GE. yhi) THEN
              icountN=icountN+1
              IF ( icountN.LE.max_npart_exch ) THEN

               ic=(icountN-1)*imax
               fltbuf_sendE(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendE(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendE(ic+3,bi,bj) =   xpart(ip,bi,bj)
               fltbuf_sendE(ic+4,bi,bj) =   ypart(ip,bi,bj)
               fltbuf_sendE(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendE(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendE(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendE(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendE(ic+9,bi,bj) =    tend(ip,bi,bj)

               jp = npart_tile(bi,bj)
               npart (ip,bi,bj) =  npart (jp,bi,bj)
               tstart(ip,bi,bj) =  tstart(jp,bi,bj)
               xpart (ip,bi,bj) =  xpart (jp,bi,bj)
               ypart (ip,bi,bj) =  ypart (jp,bi,bj)
               kpart (ip,bi,bj) =  kpart (jp,bi,bj)
               kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
               iup   (ip,bi,bj) =  iup   (jp,bi,bj)
               itop  (ip,bi,bj) =  itop  (jp,bi,bj)
               tend  (ip,bi,bj) =  tend  (jp,bi,bj)

               npart_tile(bi,bj) = jp - 1

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCHG,N:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yHi=', ypart(ip,bi,bj), yhi
c              CALL PRINT_ERROR( msgBuf, myThid )
              ENDIF
             ENDIF

             IF (ypart(ip,bi,bj) .LE. ylo) THEN
              icountS=icountS+1
              IF ( icountS.LE.max_npart_exch ) THEN

               ic=(icountS-1)*imax
               fltbuf_sendW(ic+1,bi,bj) =   npart(ip,bi,bj)
               fltbuf_sendW(ic+2,bi,bj) =  tstart(ip,bi,bj)
               fltbuf_sendW(ic+3,bi,bj) =   xpart(ip,bi,bj)
               fltbuf_sendW(ic+4,bi,bj) =   ypart(ip,bi,bj)
               fltbuf_sendW(ic+5,bi,bj) =   kpart(ip,bi,bj)
               fltbuf_sendW(ic+6,bi,bj) =  kfloat(ip,bi,bj)
               fltbuf_sendW(ic+7,bi,bj) =     iup(ip,bi,bj)
               fltbuf_sendW(ic+8,bi,bj) =    itop(ip,bi,bj)
               fltbuf_sendW(ic+9,bi,bj) =    tend(ip,bi,bj)

               jp = npart_tile(bi,bj)
               npart (ip,bi,bj) =  npart (jp,bi,bj)
               tstart(ip,bi,bj) =  tstart(jp,bi,bj)
               xpart (ip,bi,bj) =  xpart (jp,bi,bj)
               ypart (ip,bi,bj) =  ypart (jp,bi,bj)
               kpart (ip,bi,bj) =  kpart (jp,bi,bj)
               kfloat(ip,bi,bj) =  kfloat(jp,bi,bj)
               iup   (ip,bi,bj) =  iup   (jp,bi,bj)
               itop  (ip,bi,bj) =  itop  (jp,bi,bj)
               tend  (ip,bi,bj) =  tend  (jp,bi,bj)

               npart_tile(bi,bj) = jp - 1

c             ELSE
c              WRITE(msgBuf,'(2A,2I4,I6,A,2F17.6))') ' FLT_EXCHG,S:',
c    &         ' bi,bj,ip=', bi, bj, ip,
c    &         ' yp,yLo=', ypart(ip,bi,bj), ylo
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
             write(0,*) myYGlobalLo,jG,delY(jG),yc(1,1,bi,bj),ylo
           ENDIF
           IF ( icountN.GT.max_npart_exch
     &     .OR. icountS.GT.max_npart_exch ) THEN
             STOP 'ABNORMAL END: S/R FLT_EXCHG'
           ENDIF

         ENDDO
        ENDDO

C     "Put" north and south arrays.
        CALL EXCH_RL_SEND_PUT_VEC_Y( fltbuf_sendE, fltbuf_sendW,
     I                               imax2, myThid )

C     Receive north and south arrays
        CALL EXCH_RL_RECV_GET_VEC_Y( fltbuf_recvE, fltbuf_recvW,
     I                               imax2, myThid )

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
               xpart( jp,bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               ypart( jp,bi,bj) = fltbuf_recvE(ic+4,bi,bj)
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
               xpart( jp,bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               ypart( jp,bi,bj) = fltbuf_recvW(ic+4,bi,bj)
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
