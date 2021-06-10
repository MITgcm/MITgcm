#include "CTRL_OPTIONS.h"

      subroutine ctrl_init_wet( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_init_wet
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"
#include "CTRL_OBCS.h"

#ifdef ALLOW_OBCS_CONTROL
# include "OBCS_GRID.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ntmp2(4)
      integer nwetc3d
      integer nwettmp
#ifdef ALLOW_SHELFICE
      integer ntmpshi
#endif
#ifdef ALLOW_OBCS_CONTROL
      integer iobcs
      integer ntmpob(nobcs)
      character*(80) ymaskobcs
#endif
#ifdef ALLOW_EFLUXP0_CONTROL
      _RL dummy
#endif /* ALLOW_EFLUXP0_CONTROL */
      _RS dummyRS

      character*(max_len_mbuf) msgbuf

c--   Set loop ranges.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Determine the number of wet points in each tile:
c--   maskc, masks, and maskw.

c--   Initialise the counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            nwetctile(bi,bj,k) = 0
            nwetstile(bi,bj,k) = 0
            nwetwtile(bi,bj,k) = 0
            nwetvtile(bi,bj,k) = 0
#ifdef ALLOW_SHELFICE
            nwetitile(bi,bj,k) = 0
#endif
          enddo
        enddo
      enddo

#ifdef ALLOW_OBCS_CONTROL
c--   Initialise obcs counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do iobcs = 1,nobcs
#ifdef ALLOW_OBCSN_CONTROL
              nwetobcsn(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSS_CONTROL
              nwetobcss(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSW_CONTROL
              nwetobcsw(bi,bj,k,iobcs) = 0
#endif
#ifdef ALLOW_OBCSE_CONTROL
              nwetobcse(bi,bj,k,iobcs) = 0
#endif
            enddo
          enddo
        enddo
      enddo
#endif

c--   Count wet points on each tile.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
c--             Center mask.
                if (maskC(i,j,k,bi,bj) .ne. 0.) then
                  nwetctile(bi,bj,k) = nwetctile(bi,bj,k) + 1
                endif
c--             South mask.
                if (maskS(i,j,k,bi,bj) .eq. 1.) then
                  nwetstile(bi,bj,k) = nwetstile(bi,bj,k) + 1
                endif
c--             West mask.
                if (maskW(i,j,k,bi,bj) .eq. 1.) then
                  nwetwtile(bi,bj,k) = nwetwtile(bi,bj,k) + 1
                endif
#if (defined (ALLOW_EFLUXP0_CONTROL))
c--             Vertical mask.
                if (hFacV(i,j,k,bi,bj) .ne. 0.) then
                  nwetvtile(bi,bj,k) = nwetvtile(bi,bj,k) + 1
                endif
#endif
#ifdef ALLOW_SHELFICE
c--             Ice shelf mask.
                if (maskSHI(i,j,k,bi,bj) .eq. 1.) then
                 nwetitile(bi,bj,k) = nwetitile(bi,bj,k) + 1
                endif
#endif /* ALLOW_SHELFICE */
              enddo
            enddo
          enddo
        enddo
      enddo

#ifdef ALLOW_OBCSN_CONTROL
c--   Count wet points at Northern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsn'
      call ctrl_mask_set_xz( 0, OB_indexNone, OB_Jn,
     &                       nwetobcsn, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
c--   Count wet points at Southern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcss'
      call ctrl_mask_set_xz( 1, OB_indexNone, OB_Js,
     &                       nwetobcss, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
c--   Count wet points at Western boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsw'
      call ctrl_mask_set_yz( 1, OB_indexNone, OB_Iw,
     &                       nwetobcsw, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
c--   Count wet points at Eastern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcse'
      call ctrl_mask_set_yz( 0, OB_indexNone, OB_Ie,
     &                       nwetobcse, ymaskobcs, mythid )
#endif

      _BEGIN_MASTER( mythid )
c--   Determine the total number of control variables.
      nvartype   = 0
      nvarlength = 0
      do i = 1,maxcvars
c
       if ( ncvarindex(i) .ne. -1 ) then
        nvartype = nvartype + 1
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do k = 1,ncvarnrmax(i)
           if ( ncvargrd(i) .eq. 'c' ) then
            nvarlength = nvarlength + ncvarrecs(i)*nwetctile(bi,bj,k)
           else if ( ncvargrd(i) .eq. 's' ) then
            nvarlength = nvarlength + ncvarrecs(i)*nwetstile(bi,bj,k)
           else if ( ncvargrd(i) .eq. 'w' ) then
            nvarlength = nvarlength + ncvarrecs(i)*nwetwtile(bi,bj,k)
           else if ( ncvargrd(i) .eq. 'v' ) then
            nvarlength = nvarlength + ncvarrecs(i)*nwetvtile(bi,bj,k)
#ifdef ALLOW_SHELFICE
c--             Ice shelf mask.
           else if ( ncvargrd(i) .eq. 'i') then
            nvarlength = nvarlength + ncvarrecs(i)*nwetitile(bi,bj,k)
#endif /* ALLOW_SHELFICE */
           else if ( ncvargrd(i) .eq. 'm' ) then
#ifdef ALLOW_OBCS_CONTROL
            do iobcs = 1, nobcs
cgg   This overcounts the number of o.b. control points by a factor of "nobcs".
cgg   As an ad-hoc solution I have divided by nobcs everywhere.
#ifdef ALLOW_OBCSN_CONTROL
             if ( i .eq. 11 ) nvarlength = nvarlength
     &            + nwetobcsn(bi,bj,k,iobcs)*(ncvarrecs(i)/nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
             if ( i .eq. 12 ) nvarlength = nvarlength
     &            + nwetobcss(bi,bj,k,iobcs)*(ncvarrecs(i)/nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
             if ( i .eq. 13 ) nvarlength = nvarlength
     &            + nwetobcsw(bi,bj,k,iobcs)*(ncvarrecs(i)/nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
             if ( i .eq. 14 ) nvarlength = nvarlength
     &            + nwetobcse(bi,bj,k,iobcs)*(ncvarrecs(i)/nobcs)
#endif
            enddo
#endif
           else
            print*,'ctrl_init_wet: invalid grid location'
            print*,'     control variable = ',ncvarindex(i)
            print*,'     grid location    = ',ncvargrd(i)
            stop   ' ... stopped in ctrl_init_wet'
           endif
          enddo
         enddo
        enddo
       endif
      enddo

cph(
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 1:    nvarlength = ', nvarlength
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 2: surface wet C = ', nwetctile(1,1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 3: surface wet W = ', nwetwtile(1,1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 4: surface wet S = ', nwetstile(1,1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 4a:surface wet V = ', nwetvtile(1,1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      nwetc3d = 0
      do k = 1, Nr
         nwetc3d = nwetc3d + nwetctile(1,1,k)
      end do
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 5: 3D wet points = ', nwetc3d
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      do i = 1, maxcvars
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &     'ctrl-wet 6: no recs for i = ', i, ncvarrecs(i)
        call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)
      end do

      nwettmp =
     &     2*nwetc3d +
     &     ncvarrecs(3)*nwetctile(1,1,1) +
     &     ncvarrecs(4)*nwetctile(1,1,1) +
     &     ncvarrecs(5)*nwetwtile(1,1,1) +
     &     ncvarrecs(6)*nwetstile(1,1,1)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 7: flux  ', nwettmp
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      nwettmp =
     &     2*nwetc3d +
     &     ncvarrecs(7)*nwetctile(1,1,1) +
     &     ncvarrecs(8)*nwetctile(1,1,1) +
     &     ncvarrecs(9)*nwetwtile(1,1,1) +
     &     ncvarrecs(10)*nwetstile(1,1,1)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 8: atmos ', nwettmp
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_OBCSN_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 9: surface wet obcsn = '
     &     , nwetobcsn(1,1,1,1), nwetobcsn(1,1,1,2)
     &     , nwetobcsn(1,1,1,3), nwetobcsn(1,1,1,4)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 10: surface wet obcss = '
     &     , nwetobcss(1,1,1,1), nwetobcss(1,1,1,2)
     &     , nwetobcss(1,1,1,3), nwetobcss(1,1,1,4)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 11: surface wet obcsw = '
     &     , nwetobcsw(1,1,1,1), nwetobcsw(1,1,1,2)
     &     , nwetobcsw(1,1,1,3), nwetobcsw(1,1,1,4)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 12: surface wet obcse = '
     &     , nwetobcse(1,1,1,1), nwetobcse(1,1,1,2)
     &     , nwetobcse(1,1,1,3), nwetobcse(1,1,1,4)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
#endif
cph)

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

      CALL GLOBAL_SUM_INT( nvarlength,  myThid )

      write(msgbuf,'(a,2x,I3,2x,I10)')
     &     'ctrl-wet 13: global nvarlength for Nr =', nr, nvarlength
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

c
c     Summation of wet point counters
c
      do k = 1, nr

         ntmp2(1)=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp2(1)=ntmp2(1)+nWetcTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp2(1),  myThid )
         nWetcGlobal(k)=ntmp2(1)

         ntmp2(2)=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp2(2)=ntmp2(2)+nWetsTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp2(2),  myThid )
         nWetsGlobal(k)=ntmp2(2)

         ntmp2(3)=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp2(3)=ntmp2(3)+nWetwTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp2(3),  myThid )
         nWetwGlobal(k)=ntmp2(3)

         ntmp2(4)=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp2(4)=ntmp2(4)+nWetvTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp2(4),  myThid )
         nWetvGlobal(k)=ntmp2(4)

         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &        'ctrl-wet 14: global nWet C/S/W/V k=', k, ntmp2
         call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)

      enddo

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

      do k = 1, nr

#ifdef ALLOW_OBCSN_CONTROL
         do iobcs = 1, nobcs
            ntmpob(iobcs)=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmpob(iobcs)=ntmpob(iobcs)+nwetobcsn(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmpob(iobcs),  myThid )
            nwetobcsnglo(k,iobcs)=ntmpob(iobcs)
         enddo
         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &       'ctrl-wet 15a: global obcsN T,S,U,V k=', k, ntmpob
         call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSS_CONTROL
         do iobcs = 1, nobcs
            ntmpob(iobcs)=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmpob(iobcs)=ntmpob(iobcs)+nwetobcss(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmpob(iobcs),  myThid )
            nwetobcssglo(k,iobcs)=ntmpob(iobcs)
         enddo
         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &       'ctrl-wet 15b: global obcsS T,S,U,V k=', k, ntmpob
         call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSW_CONTROL
         do iobcs = 1, nobcs
            ntmpob(iobcs)=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmpob(iobcs)=ntmpob(iobcs)+nwetobcsw(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmpob(iobcs),  myThid )
            nwetobcswglo(k,iobcs)=ntmpob(iobcs)
         enddo
         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &       'ctrl-wet 15c: global obcsW T,S,U,V k=', k, ntmpob
         call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSE_CONTROL
         do iobcs = 1, nobcs
            ntmpob(iobcs)=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmpob(iobcs)=ntmpob(iobcs)+nwetobcse(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmpob(iobcs),  myThid )
            nwetobcseglo(k,iobcs)=ntmpob(iobcs)
         enddo
         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &       'ctrl-wet 15d: global obcsE T,S,U,V k=', k, ntmpob
         call print_message( msgbuf, standardmessageunit,
     &       SQUEEZE_RIGHT , mythid)
#endif

      enddo

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_OBCSN_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcsnglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16a: global SUM(K) obcsN T,S,U,V ', ntmpob
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcssglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16b: global SUM(K) obcsS T,S,U,V ', ntmpob
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcswglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16c: global SUM(K) obcsW T,S,U,V ', ntmpob
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcseglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16d: global SUM(K) obcsE T,S,U,V ', ntmpob
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)
#endif

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_SHELFICE
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 17a:surface wet I = ', nwetitile(1,1,1)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      do k = 1, nr
       ntmpshi=0
       do bj=1,nSy
        do bi=1,nSx
         ntmpshi=ntmpshi+nWetiTile(bi,bj,k)
        enddo
       enddo
       CALL GLOBAL_SUM_INT( ntmpshi,  myThid )
       if (k.eq.1) then
        nWetiGlobal(k)=ntmpshi
       else
        nWetiGlobal(k)=0
       endif
       write(msgbuf,'(a,2x,I3,2x,I10)')
     &      'ctrl-wet 17b: global nWet I k=', k, ntmpshi
       call print_message( msgbuf, standardmessageunit,
     &      SQUEEZE_RIGHT , mythid)
      enddo

      ntmpshi=0
      do k = 1, nr
       ntmpshi=ntmpshi+nWetiGlobal(k)
      enddo
      write(msgbuf,'(a,2x,I10)')
     &    'ctrl-wet 17c: global SUM(K) shelfice ', ntmpshi
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &    SQUEEZE_RIGHT , mythid)
#endif

      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init_wet: no. of control variables: ', nvartype
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init_wet: control vector length:    ', nvarlength
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      _END_MASTER( mythid )

#ifdef ALLOW_AUTODIFF
c     write masks and weights to files to be read by a master process
c
c#ifdef REAL4_IS_SLOW
C     leave this commented out (in case of problems with ACTIVE_WRITE_GEN_RS)
c     call active_write_xyz( 'maskCtrlC', maskC, 1, 0, mythid, dummy)
c     call active_write_xyz( 'maskCtrlW', maskW, 1, 0, mythid, dummy)
c     call active_write_xyz( 'maskCtrlS', maskS, 1, 0, mythid, dummy)
c#else
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlC', maskC, 'XY', Nr,
     I                          1, .FALSE., 0, mythid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlW', maskW, 'XY', Nr,
     I                          1, .FALSE., 0, mythid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlS', maskS, 'XY', Nr,
     I                          1, .FALSE., 0, mythid, dummyRS )
c#endif
#ifdef ALLOW_EFLUXP0_CONTROL
      call active_write_xyz( 'maskhFacV', hFacV, 1, 0, mythid, dummy)
#endif
#ifdef ALLOW_SHELFICE
c#ifdef REAL4_IS_SLOW
c     call active_write_xyz( 'maskCtrlI', maskSHI, 1, 0, mythid, dummy)
c#else
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlI', maskSHI, 'XY', Nr,
     I                          1, .FALSE., 0, mythid, dummyRS )
c#endif
#endif

#endif /* ALLOW_AUTODIFF */

      RETURN
      END
