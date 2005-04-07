C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_init_wet.F,v 1.5 2005/04/07 23:38:43 heimbach Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"

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

#ifdef ALLOW_OBCS_CONTROL
# include "OBCS.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ntmp
      integer iobcs
      integer nwetc3d
      integer nwettmp

      _RL     dummy

      character*(80) ymaskobcs
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
                if (hFacC(i,j,k,bi,bj) .ne. 0.) then
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
              enddo
            enddo
          enddo
        enddo
      enddo

#ifdef ALLOW_OBCSN_CONTROL
c--   Count wet points at Northern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsn'
      call ctrl_mask_set_xz( 0, OB_Jn, nwetobcsn, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
c--   Count wet points at Southern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcss'
      call ctrl_mask_set_xz( 1, OB_Js, nwetobcss, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
c--   Count wet points at Western boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsw'
      call ctrl_mask_set_yz( 1, OB_Iw, nwetobcsw, ymaskobcs, mythid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
c--   Count wet points at Eastern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcse'
      call ctrl_mask_set_yz( 0, OB_Ie, nwetobcse, ymaskobcs, mythid )
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
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetctile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 's' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetstile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'w' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetwtile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'v' ) then
                        nvarlength = nvarlength + 
     &                       ncvarrecs(i)*nwetvtile(bi,bj,k)
                     else if ( ncvargrd(i) .eq. 'm' ) then
#ifdef ALLOW_OBCS_CONTROL
                        do iobcs = 1, nobcs
cgg   This overcounts the number of o.b. control points by a factor of "nobcs".
cgg   As an ad-hoc solution I've divided by nobcs everywhere.
                           if ( i .eq. 11 ) then
#ifdef ALLOW_OBCSN_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcsn(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 12 ) then
#ifdef ALLOW_OBCSS_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcss(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 13 ) then
#ifdef ALLOW_OBCSW_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcsw(bi,bj,k,iobcs)
#endif
                           else if ( i .eq. 14 ) then
#ifdef ALLOW_OBCSE_CONTROL
                              nvarlength = nvarlength + 
     &                             (ncvarrecs(i)/nobcs)
     &                             *nwetobcse(bi,bj,k,iobcs)
#endif
                           end if
                        enddo
#endif
                     else
                        print*,'ctrl_init: invalid grid location'
                        print*,'     control variable = ',ncvarindex(i)
                        print*,'     grid location    = ',ncvargrd(i)
                        stop   ' ... stopped in ctrl_init'
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
      
      CALL GLOBAL_SUM_INT( nvarlength,  myThid )

      write(msgbuf,'(a,2x,I3,2x,I10)')
     &     'ctrl-wet 13: global nvarlength vor k=', k, nvarlength
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

c
c     Summation of wet point counters 
c
      do k = 1, nr

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetcTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetcGlobal(k)=ntmp
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &        'ctrl-wet 14a: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetsTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetsGlobal(k)=ntmp
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &        'ctrl-wet 14b: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetwTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetwGlobal(k)=ntmp
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &        'ctrl-wet 14c: global nWet... k=', k, ntmp

         ntmp=0
         do bj=1,nSy
            do bi=1,nSx
               ntmp=ntmp+nWetvTile(bi,bj,k)
            enddo
         enddo
         CALL GLOBAL_SUM_INT( ntmp,  myThid )
         nWetvGlobal(k)=ntmp
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &        'ctrl-wet 14d: global nWet... k=', k, ntmp

#ifdef ALLOW_OBCSN_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcsn(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcsnglo(k,iobcs)=ntmp
            write(msgbuf,'(a,2(2x,I3),2x,I10)')
     &        'ctrl-wet 15a: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSS_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcss(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcssglo(k,iobcs)=ntmp
            write(msgbuf,'(a,2(2x,I3),2x,I10)')
     &        'ctrl-wet 15b: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSW_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcsw(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcswglo(k,iobcs)=ntmp
            write(msgbuf,'(a,2(2x,I3),2x,I10)')
     &        'ctrl-wet 15c: global nWet... k=', k, iobcs, ntmp
         enddo
#endif
#ifdef ALLOW_OBCSE_CONTROL
         do iobcs = 1, nobcs
            ntmp=0
            do bj=1,nSy
               do bi=1,nSx
                  ntmp=ntmp+nwetobcse(bi,bj,k,iobcs)
               enddo
            enddo
            CALL GLOBAL_SUM_INT( ntmp,  myThid )
            nwetobcseglo(k,iobcs)=ntmp
            write(msgbuf,'(a,2(2x,I3),2x,I10)')
     &        'ctrl-wet 15d: global nWet... k=', k, iobcs, ntmp
         enddo
#endif

      enddo

      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init: no. of control variables: ', nvartype
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init: control vector length:    ', nvarlength
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT , mythid)

      _END_MASTER( mythid )

c     Set unit weight to 1
c
      do bj=1,nSy
         do bi=1,nSx
            do k=1, nr
               wunit(k,bi,bj) = 1. _d 0
            enddo
         enddo
      enddo

c     write masks and weights to files to be read by a master process
c
      call active_write_xyz( 'maskCtrlC', maskC, 1, 0, mythid, dummy)
      call active_write_xyz( 'maskCtrlW', maskW, 1, 0, mythid, dummy)
      call active_write_xyz( 'maskCtrlS', maskS, 1, 0, mythid, dummy)
#if (defined (ALLOW_EFLUXP0_CONTROL))
      call active_write_xyz( 'maskhFacV', hFacV, 1, 0, mythid, dummy)
#endif

      return
      end
