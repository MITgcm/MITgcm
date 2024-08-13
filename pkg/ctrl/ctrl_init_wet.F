#include "CTRL_OPTIONS.h"

      subroutine ctrl_init_wet( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_init_wet
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_OBCS.h"

#ifdef ALLOW_OBCS_CONTROL
# include "OBCS_GRID.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

c     == routine arguments ==
      integer myThid

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ivar
      integer ntmp2(4)
      integer nwetc3d
#ifdef ALLOW_SHELFICE
      integer ntmpshi
#endif
#ifdef ALLOW_OBCS_CONTROL
      integer iobcs
      integer ntmpob(nobcs)
      CHARACTER*(MAX_LEN_FNAM) ymaskobcs
#endif
c#ifdef REAL4_IS_SLOW
c     _RL dummy
c#endif
      _RS dummyRS
      CHARACTER*(MAX_LEN_MBUF) msgbuf

c--   Set loop ranges.
      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

c--   Determine the number of wet points in each tile:
c--   maskc, masks, and maskw.

c--   Initialise the counters.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,Nr
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
          do k = 1,Nr
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
          do k = 1,Nr
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
     &                       nwetobcsn, ymaskobcs, myThid )
#endif

#ifdef ALLOW_OBCSS_CONTROL
c--   Count wet points at Southern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcss'
      call ctrl_mask_set_xz( 1, OB_indexNone, OB_Js,
     &                       nwetobcss, ymaskobcs, myThid )
#endif

#ifdef ALLOW_OBCSW_CONTROL
c--   Count wet points at Western boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcsw'
      call ctrl_mask_set_yz( 1, OB_indexNone, OB_Iw,
     &                       nwetobcsw, ymaskobcs, myThid )
#endif

#ifdef ALLOW_OBCSE_CONTROL
c--   Count wet points at Eastern boundary.
c--   mask conventions are adopted from obcs_apply_ts, obcs_apply_uv
      ymaskobcs = 'maskobcse'
      call ctrl_mask_set_yz( 0, OB_indexNone, OB_Ie,
     &                       nwetobcse, ymaskobcs, myThid )
#endif

      _BEGIN_MASTER( myThid )
c--   Determine the total number of control variables.
      nvartype   = 0
      nvarlength = 0
      do ivar = 1, maxcvars
c
       if ( ncvarindex(ivar) .ge. 0 ) then
        nvartype = nvartype + 1
        do bj = jtlo,jthi
         do bi = itlo,ithi
          do k = 1,ncvarnrmax(ivar)
           if ( ncvargrd(ivar) .eq. 'c' ) then
            nvarlength = nvarlength + ncvarrecs(ivar)*nwetctile(bi,bj,k)
           else if ( ncvargrd(ivar) .eq. 's' ) then
            nvarlength = nvarlength + ncvarrecs(ivar)*nwetstile(bi,bj,k)
           else if ( ncvargrd(ivar) .eq. 'w' ) then
            nvarlength = nvarlength + ncvarrecs(ivar)*nwetwtile(bi,bj,k)
c          else if ( ncvargrd(ivar) .eq. 'v' ) then
c           nvarlength = nvarlength + ncvarrecs(ivar)*nwetvtile(bi,bj,k)
#ifdef ALLOW_SHELFICE
c--             Ice shelf mask.
           else if ( ncvargrd(ivar) .eq. 'i') then
            nvarlength = nvarlength + ncvarrecs(ivar)*nwetitile(bi,bj,k)
#endif /* ALLOW_SHELFICE */
           else if ( ncvargrd(ivar) .eq. 'm' ) then
#ifdef ALLOW_OBCS_CONTROL
            do iobcs = 1, nobcs
cgg   This overcounts the number of o.b. control points by a factor of "nobcs".
cgg   As an ad-hoc solution I have divided by nobcs everywhere.
#ifdef ALLOW_OBCSN_CONTROL
             if ( ncvarindex(ivar) .eq. 1 ) nvarlength = nvarlength
     &            + nwetobcsn(bi,bj,k,iobcs)*(ncvarrecs(ivar)/nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
             if ( ncvarindex(ivar) .eq. 2 ) nvarlength = nvarlength
     &            + nwetobcss(bi,bj,k,iobcs)*(ncvarrecs(ivar)/nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
             if ( ncvarindex(ivar) .eq. 3 ) nvarlength = nvarlength
     &            + nwetobcse(bi,bj,k,iobcs)*(ncvarrecs(ivar)/nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
             if ( ncvarindex(ivar) .eq. 4 ) nvarlength = nvarlength
     &            + nwetobcsw(bi,bj,k,iobcs)*(ncvarrecs(ivar)/nobcs)
#endif
            enddo
#endif
           else
            print*,'ctrl_init_wet: invalid grid location'
            print*,'     control variable = ',ncvarindex(ivar)
            print*,'     grid location    = ',ncvargrd(ivar)
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
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 2: surface wet C = ', nwetctile(1,1,1)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 3: surface wet W = ', nwetwtile(1,1,1)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 4: surface wet S = ', nwetstile(1,1,1)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
c     write(msgbuf,'(a,2x,I10)')
c    &     'ctrl-wet 4a:surface wet V = ', nwetvtile(1,1,1)
c     call print_message( msgbuf, standardMessageUnit,
c    &     SQUEEZE_RIGHT, myThid )

      nwetc3d = 0
      do k = 1, Nr
         nwetc3d = nwetc3d + nwetctile(1,1,k)
      end do
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 5: 3D wet points = ', nwetc3d
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      do ivar = 1, maxcvars
         write(msgbuf,'(a,2x,I3,2x,I10)')
     &     'ctrl-wet 6: no recs for ivar = ', ivar, ncvarrecs(ivar)
        call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
      end do

#ifdef ALLOW_OBCSN_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 9: surface wet obcsn = '
     &     , nwetobcsn(1,1,1,1), nwetobcsn(1,1,1,2)
     &     , nwetobcsn(1,1,1,3), nwetobcsn(1,1,1,4)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSS_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 10: surface wet obcss = '
     &     , nwetobcss(1,1,1,1), nwetobcss(1,1,1,2)
     &     , nwetobcss(1,1,1,3), nwetobcss(1,1,1,4)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSW_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 11: surface wet obcsw = '
     &     , nwetobcsw(1,1,1,1), nwetobcsw(1,1,1,2)
     &     , nwetobcsw(1,1,1,3), nwetobcsw(1,1,1,4)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSE_CONTROL
      write(msgbuf,'(a,2x,4I10)')
     &     'ctrl-wet 12: surface wet obcse = '
     &     , nwetobcse(1,1,1,1), nwetobcse(1,1,1,2)
     &     , nwetobcse(1,1,1,3), nwetobcse(1,1,1,4)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
#endif
cph)

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      CALL GLOBAL_SUM_INT( nvarlength,  myThid )

      write(msgbuf,'(a,2x,I3,2x,I10)')
     &     'ctrl-wet 13: global nvarlength for Nr =', Nr, nvarlength
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

c
c     Summation of wet point counters
c
      do k = 1, Nr

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
c        do bj=1,nSy
c           do bi=1,nSx
c              ntmp2(4)=ntmp2(4)+nWetvTile(bi,bj,k)
c           enddo
c        enddo
c        CALL GLOBAL_SUM_INT( ntmp2(4),  myThid )
         nWetvGlobal(k)=ntmp2(4)

         write(msgbuf,'(a,2x,I3,4(2x,I10))')
     &        'ctrl-wet 14: global nWet C/S/W k=', k, (ntmp2(i),i=1,3)
c    &        'ctrl-wet 14: global nWet C/S/W/V k=', k, ntmp2
         call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )

      enddo

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      do k = 1, Nr

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
         call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
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
         call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
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
         call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
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
         call print_message( msgbuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
#endif

      enddo

#ifdef ALLOW_OBCS_CONTROL
      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_OBCSN_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, Nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcsnglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16a: global SUM(K) obcsN T,S,U,V ', ntmpob
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSS_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, Nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcssglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16b: global SUM(K) obcsS T,S,U,V ', ntmpob
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSW_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, Nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcswglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16c: global SUM(K) obcsW T,S,U,V ', ntmpob
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCSE_CONTROL
      do iobcs = 1, nobcs
        ntmpob(iobcs)=0
        do k = 1, Nr
          ntmpob(iobcs)=ntmpob(iobcs)+nwetobcseglo(k,iobcs)
        enddo
      enddo
      write(msgbuf,'(a,4(2x,I10))')
     &    'ctrl-wet 16d: global SUM(K) obcsE T,S,U,V ', ntmpob
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif
#ifdef ALLOW_OBCS_CONTROL
      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_SHELFICE
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl-wet 17a:surface wet I = ', nwetitile(1,1,1)
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      do k = 1, Nr
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
       call print_message( msgbuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
      enddo

      ntmpshi=0
      do k = 1, Nr
       ntmpshi=ntmpshi+nWetiGlobal(k)
      enddo
      write(msgbuf,'(a,2x,I10)')
     &    'ctrl-wet 17c: global SUM(K) shelfice ', ntmpshi
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )

      write(msgbuf,'(a)')
     &    'ctrl-wet -------------------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &    SQUEEZE_RIGHT, myThid )
#endif

      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init_wet: no. of control variables: ', nvartype
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,2x,I10)')
     &     'ctrl_init_wet: control vector length:    ', nvarlength
      call print_message( msgbuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      _END_MASTER( myThid )

#ifdef ALLOW_AUTODIFF
c     write masks and weights to files to be read by a master process
c
c#ifdef REAL4_IS_SLOW
C     leave this commented out (in case of problems with ACTIVE_WRITE_GEN_RS)
c     call active_write_xyz( 'maskCtrlC', maskC, 1, 0, myThid, dummy)
c     call active_write_xyz( 'maskCtrlW', maskW, 1, 0, myThid, dummy)
c     call active_write_xyz( 'maskCtrlS', maskS, 1, 0, myThid, dummy)
c#else
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlC', maskC, 'XY', Nr,
     I                          1, .FALSE., 0, myThid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlW', maskW, 'XY', Nr,
     I                          1, .FALSE., 0, myThid, dummyRS )
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlS', maskS, 'XY', Nr,
     I                          1, .FALSE., 0, myThid, dummyRS )
c#endif
#ifdef ALLOW_SHELFICE
c#ifdef REAL4_IS_SLOW
c     call active_write_xyz( 'maskCtrlI', maskSHI, 1, 0, myThid, dummy)
c#else
      CALL ACTIVE_WRITE_GEN_RS( 'maskCtrlI', maskSHI, 'XY', Nr,
     I                          1, .FALSE., 0, myThid, dummyRS )
c#endif
#endif

#endif /* ALLOW_AUTODIFF */

      RETURN
      END
