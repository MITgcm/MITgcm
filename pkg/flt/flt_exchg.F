C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_exchg.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine flt_exchg (
     I                       myCurrentIter, 
     I                       myCurrentTime, 
     I                       myThid
     &                      )

c     ==================================================================
c     SUBROUTINE flt_exchg
c     ==================================================================
c
c     o 
c
c     ==================================================================
c     SUBROUTINE flt_exchg
c     ==================================================================

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
#include "FLT.h"
#include "GRID.h"
#include "PARAMS.h"

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj, ic
      character*(max_len_mbuf) msgbuf

c     == local variables ==

      integer ip
      integer icountE, icountW, icountN, icountS
      _RL xx, yy
      INTEGER imax, imax2, m, iG, jG
      _RL xlo, xhi, ylo, yhi
      parameter(imax=9)
      parameter(imax2=imax*max_npart_exch)

c     buffer for sending/receiving variables (E/W are also used for S/N)
      _RL fltbuf_sendE(imax2,nSx,nSy)
      _RL fltbuf_sendW(imax2,nSx,nSy)
      _RL fltbuf_recvE(imax2,nSx,nSy)
      _RL fltbuf_recvW(imax2,nSx,nSy)

      _RL npart_dist
c     == end of interface ==

C--   Choose floats that have to exchanged with eastern and western tiles
C     and pack to arrays


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
      
c initialize buffers

          do m=1,imax2
             fltbuf_sendE(m,bi,bj) = 0.
             fltbuf_sendW(m,bi,bj) = 0.
             fltbuf_recvE(m,bi,bj) = 0.
             fltbuf_recvW(m,bi,bj) = 0.
          enddo

          icountE=0
          icountW=0

          iG = myXGlobalLo + (bi-1)*sNx
          xlo = xc(1,  1,  bi,bj) - delX(iG)
          xhi = xc(sNx,1,bi,bj)   + delX(iG+sNx-1)
c
          do ip=1,npart_tile(bi,bj)
c
             if (xpart(ip,bi,bj) .ge. xhi) then
                icountE=icountE+1
                if (icountE .gt. max_npart_exch) 
     &          stop ' max_npart_exch too low. stop in flt_exchg'

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

                  npart(ip,bi,bj) =   npart(npart_tile(bi,bj),bi,bj)
                 tstart(ip,bi,bj) =  tstart(npart_tile(bi,bj),bi,bj)
                xpart(ip,bi,bj)   =   xpart(npart_tile(bi,bj),bi,bj)
                ypart(ip,bi,bj)   =   ypart(npart_tile(bi,bj),bi,bj)
                kpart(ip,bi,bj)   =   kpart(npart_tile(bi,bj),bi,bj)
                kfloat(ip,bi,bj)  =  kfloat(npart_tile(bi,bj),bi,bj)
                iup(ip,bi,bj)     =     iup(npart_tile(bi,bj),bi,bj)
                itop(ip,bi,bj)    =    itop(npart_tile(bi,bj),bi,bj)
                tend(ip,bi,bj)    =    tend(npart_tile(bi,bj),bi,bj)

                npart_tile(bi,bj) = npart_tile(bi,bj) - 1

             endif

             if (xpart(ip,bi,bj) .le. xlo) then
                icountW=icountW+1
                if (icountW .gt. max_npart_exch) 
     &          stop ' max_npart_exch too low. stop in flt_exchg'

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

                  npart(ip,bi,bj) =   npart(npart_tile(bi,bj),bi,bj)
                 tstart(ip,bi,bj) =  tstart(npart_tile(bi,bj),bi,bj)
                xpart(ip,bi,bj)   =   xpart(npart_tile(bi,bj),bi,bj)
                ypart(ip,bi,bj)   =   ypart(npart_tile(bi,bj),bi,bj)
                kpart(ip,bi,bj)   =   kpart(npart_tile(bi,bj),bi,bj)
                kfloat(ip,bi,bj)  =  kfloat(npart_tile(bi,bj),bi,bj)
                iup(ip,bi,bj)     =     iup(npart_tile(bi,bj),bi,bj)
                itop(ip,bi,bj)    =    itop(npart_tile(bi,bj),bi,bj)
                tend(ip,bi,bj)    =    tend(npart_tile(bi,bj),bi,bj)

                npart_tile(bi,bj) = npart_tile(bi,bj) - 1

             endif

          enddo


       ENDDO
      ENDDO

C--   "Put" east and west edges.
       CALL EXCH_RL_SEND_PUT_VEC_X( fltbuf_sendE, fltbuf_sendW,
     I             imax2, myThid )
C--   Receive east/west arrays before exchanging north/south arrays
       CALL EXCH_RL_RECV_GET_VEC_X( fltbuf_recvE, fltbuf_recvW,
     I             imax2, myThid )

C--   Unpack arrays on new tiles

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
      
          do ip=1,max_npart_exch
c
             ic=(ip-1)*imax
             if (fltbuf_recvE(ic+1,bi,bj) .eq. 0.) goto 100
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             if (npart_tile(bi,bj) .gt. max_npart_tile) 
     &       stop ' max_npart_tile too low. stop in flt_exchg'

               npart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+1,bi,bj)
              tstart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+2,bi,bj)
               xpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               ypart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+4,bi,bj)
               kpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+5,bi,bj)
              kfloat(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+6,bi,bj)
                 iup(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+7,bi,bj)
                itop(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+8,bi,bj)
                tend(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+9,bi,bj)

          enddo
 100      continue

          do ip=1,max_npart_exch
c
             ic=(ip-1)*imax
             if (fltbuf_recvW(ic+1,bi,bj) .eq. 0.) goto 200
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             if (npart_tile(bi,bj) .gt. max_npart_tile) 
     &       stop ' max_npart_tile too low. stop in flt_exchg'

               npart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+1,bi,bj)
              tstart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+2,bi,bj)
               xpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               ypart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+4,bi,bj)
               kpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+5,bi,bj)
              kfloat(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+6,bi,bj)
                 iup(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+7,bi,bj)
                itop(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+8,bi,bj)
                itop(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+8,bi,bj)
                tend(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+9,bi,bj)

          enddo
 200      continue

       ENDDO
      ENDDO

C--   Choose floats that have to exchanged with eastern and western tiles
C     and pack to arrays


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
      
c initialize buffers

          do m=1,imax2
              fltbuf_sendE(m,bi,bj) = 0.
              fltbuf_sendW(m,bi,bj) = 0.
              fltbuf_recvE(m,bi,bj) = 0.
              fltbuf_recvW(m,bi,bj) = 0.
          enddo

          icountN=0
          icountS=0

          jG = myYGlobalLo + (bj-1)*sNy
          ylo = yc(1,  1,  bi,bj) - delY(jG)
          yhi = yc(1,sNy,bi,bj)   + delY(jG+sNy-1)

          do ip=1,npart_tile(bi,bj)

             if (ypart(ip,bi,bj) .ge. yhi) then
                icountN=icountN+1
                if (icountN .gt. max_npart_exch) 
     &          stop ' max_npart_exch too low. stop in flt_exchg'

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

                  npart(ip,bi,bj) =   npart(npart_tile(bi,bj),bi,bj)
                 tstart(ip,bi,bj) =  tstart(npart_tile(bi,bj),bi,bj)
                xpart(ip,bi,bj)   =   xpart(npart_tile(bi,bj),bi,bj)
                ypart(ip,bi,bj)   =   ypart(npart_tile(bi,bj),bi,bj)
                kpart(ip,bi,bj)   =   kpart(npart_tile(bi,bj),bi,bj)
                kfloat(ip,bi,bj)  =  kfloat(npart_tile(bi,bj),bi,bj)
                iup(ip,bi,bj)     =     iup(npart_tile(bi,bj),bi,bj)
                itop(ip,bi,bj)    =    itop(npart_tile(bi,bj),bi,bj)
                tend(ip,bi,bj)    =    tend(npart_tile(bi,bj),bi,bj)

                npart_tile(bi,bj) = npart_tile(bi,bj) - 1
             endif

             if (ypart(ip,bi,bj) .le. ylo) then
                icountS=icountS+1
                if (icountS .gt. max_npart_exch) 
     &          stop ' max_npart_exch too low. stop in flt_exchg'

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

                  npart(ip,bi,bj) =   npart(npart_tile(bi,bj),bi,bj)
                 tstart(ip,bi,bj) =  tstart(npart_tile(bi,bj),bi,bj)
                xpart(ip,bi,bj)   =   xpart(npart_tile(bi,bj),bi,bj)
                ypart(ip,bi,bj)   =   ypart(npart_tile(bi,bj),bi,bj)
                kpart(ip,bi,bj)   =   kpart(npart_tile(bi,bj),bi,bj)
                kfloat(ip,bi,bj)  =  kfloat(npart_tile(bi,bj),bi,bj)
                iup(ip,bi,bj)     =     iup(npart_tile(bi,bj),bi,bj)
                itop(ip,bi,bj)    =    itop(npart_tile(bi,bj),bi,bj)
                tend(ip,bi,bj)    =    tend(npart_tile(bi,bj),bi,bj)

                npart_tile(bi,bj) = npart_tile(bi,bj) - 1
             endif

          enddo

       ENDDO
      ENDDO

C     "Put" north and south arrays.
       CALL EXCH_RL_SEND_PUT_VEC_Y( fltbuf_sendE, fltbuf_sendW,
     I             imax2, myThid )

C     Receive north and south arrays
       CALL EXCH_RL_RECV_GET_VEC_Y( fltbuf_recvE, fltbuf_recvW,
     I             imax2, myThid )

C--   Unpack arrays on new tiles

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
      
          do ip=1,max_npart_exch
c
             ic=(ip-1)*imax
             if (fltbuf_recvE(ic+1,bi,bj) .eq. 0.) goto 300
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             if (npart_tile(bi,bj) .gt. max_npart_tile) 
     &       stop ' max_npart_tile too low. stop in flt_exchg'

               npart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+1,bi,bj)
              tstart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+2,bi,bj)
               xpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+3,bi,bj)
               ypart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+4,bi,bj)
               kpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+5,bi,bj)
              kfloat(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+6,bi,bj)
                 iup(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+7,bi,bj)
                itop(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+8,bi,bj)
                tend(npart_tile(bi,bj),bi,bj) = fltbuf_recvE(ic+9,bi,bj)

          enddo
 300      continue

          do ip=1,max_npart_exch
c
             ic=(ip-1)*imax
             if (fltbuf_recvW(ic+1,bi,bj) .eq. 0.) goto 400
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             if (npart_tile(bi,bj) .gt. max_npart_tile) 
     &       stop ' max_npart_tile too low. stop in flt_exchg'

               npart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+1,bi,bj)
              tstart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+2,bi,bj)
               xpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+3,bi,bj)
               ypart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+4,bi,bj)
               kpart(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+5,bi,bj)
              kfloat(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+6,bi,bj)
                 iup(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+7,bi,bj)
                itop(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+8,bi,bj)
                tend(npart_tile(bi,bj),bi,bj) = fltbuf_recvW(ic+9,bi,bj)

          enddo
 400      continue

       ENDDO
      ENDDO

      return
      end
