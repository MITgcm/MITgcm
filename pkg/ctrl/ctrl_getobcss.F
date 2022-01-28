#include "CTRL_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_getobcss(
     I                          myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_getobcss
c     ==================================================================
c
c     o Get southern obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c     new flags: gebbie@mit.edu, 25 Jan 2003.
c
c     ==================================================================
c     SUBROUTINE ctrl_getobcss
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_OBCSS_CONTROL
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "CTRL_OBCS.h"
#include "optim.h"
#endif /* ALLOW_OBCSS_CONTROL */

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

#ifdef ALLOW_OBCSS_CONTROL
c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer imin,imax
      integer ilobcss
      integer iobcs
      _RL     obcssfac
      logical obcssfirst
      logical obcsschanged
      integer obcsscount0
      integer obcsscount1
      integer jp1
cgg      _RL maskxz   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
      logical doglobalread
      logical ladinit
      character*(80) fnameobcss
#ifdef ALLOW_OBCS_CONTROL_MODES
      integer nk,nz
      _RL     tmpz (Nr,nSx,nSy)
      _RL     stmp
#endif

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      imin = 1-OLx
      imax = sNx+OLx
      jp1  = 1

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
       ilobcss=ilnblnk( xx_obcss_file )
       write(fnameobcss(1:80),'(2a,i10.10)')
     &      xx_obcss_file(1:ilobcss), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcssstartdate, xx_obcssperiod,
     O                   obcssfac, obcssfirst, obcsschanged,
     O                   obcsscount0,obcsscount1,
     I                   myTime, myIter, myThid )

      do iobcs = 1,nobcs

       if ( obcssfirst ) then
        call active_read_xz( fnameobcss, tmpfldxz,
     &                       (obcsscount0-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcss_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do i = imin,imax
            j = OB_Js(i,bi,bj)
            IF ( j.EQ.OB_indexNone ) j = 1
cih    Determine number of open vertical layers.
            nz = 0
            do k = 1,Nr
             if (iobcs .eq. 3) then
              nz = nz + maskS(i,j+jp1,k,bi,bj)
             else
              nz = nz + maskW(i,j,k,bi,bj)
             endif
            end do
cih    Compute absolute velocities from the barotropic-baroclinic modes.
            do k = 1,Nr
             if (k.le.nz) then
              stmp = 0.
              do nk = 1,nz
               stmp = stmp +
     &         modesv(k,nk,nz)*tmpfldxz(i,nk,bi,bj)
              end do
              tmpz(k,bi,bj) = stmp
             else
              tmpz(k,bi,bj) = 0.
             end if
            end do
            do k = 1,Nr
             if (iobcs .eq. 3) then
              tmpfldxz(i,k,bi,bj) = tmpz(k,bi,bj)
     &         *recip_hFacS(i,j+jp1,k,bi,bj)
             else
              tmpfldxz(i,k,bi,bj) = tmpz(k,bi,bj)
     &                           *recip_hFacW(i,j,k,bi,bj)
             endif
            end do
           enddo
          endif
#endif
          do k = 1,Nr
           do i = imin,imax
            xx_obcss1(i,k,bi,bj,iobcs)  = tmpfldxz (i,k,bi,bj)
cgg   &                                        *   maskxz (i,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

       if ( (obcssfirst) .or. (obcsschanged)) then

        do bj = jtlo,jthi
         do bi = itlo,ithi
          do k = 1,Nr
           do i = imin,imax
            xx_obcss0(i,k,bi,bj,iobcs) = xx_obcss1(i,k,bi,bj,iobcs)
            tmpfldxz (i,k,bi,bj)       = 0. _d 0
           enddo
          enddo
         enddo
        enddo

        call active_read_xz( fnameobcss, tmpfldxz,
     &                       (obcsscount1-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcss_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do i = imin,imax
            j = OB_Js(i,bi,bj)
            IF ( j.EQ.OB_indexNone ) j = 1
cih    Determine number of open vertical layers.
            nz = 0
            do k = 1,Nr
             if (iobcs .eq. 3) then
              nz = nz + maskS(i,j+jp1,k,bi,bj)
             else
              nz = nz + maskW(i,j,k,bi,bj)
             endif
            end do
cih    Compute absolute velocities from the barotropic-baroclinic modes.
            do k = 1,Nr
             if (k.le.nz) then
              stmp = 0.
              do nk = 1,nz
               stmp = stmp +
     &         modesv(k,nk,nz)*tmpfldxz(i,nk,bi,bj)
              end do
              tmpz(k,bi,bj) = stmp
             else
              tmpz(k,bi,bj) = 0.
             end if
            end do
            do k = 1,Nr
             if (iobcs .eq. 3) then
              tmpfldxz(i,k,bi,bj) = tmpz(k,bi,bj)
     &         *recip_hFacS(i,j+jp1,k,bi,bj)
             else
              tmpfldxz(i,k,bi,bj) = tmpz(k,bi,bj)
     &                           *recip_hFacW(i,j,k,bi,bj)
             endif
            end do
           enddo
          endif
#endif
          do k = 1,Nr
           do i = imin,imax
            xx_obcss1 (i,k,bi,bj,iobcs) = tmpfldxz (i,k,bi,bj)
cgg   &                                    *   maskxz (i,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

c--   Add control to model variable.
       do bj = jtlo,jthi
        do bi = itlo,ithi
c--   Calculate mask for tracer cells (0 => land, 1 => water).
         do k = 1,Nr
          do i = 1,sNx
           j = OB_Js(i,bi,bj)
           IF ( j.EQ.OB_indexNone ) j = 1
           if (iobcs .EQ. 1) then
            OBSt(i,k,bi,bj) = OBSt (i,k,bi,bj)
     &           + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
            OBSt(i,k,bi,bj) = OBSt(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           else if (iobcs .EQ. 2) then
            OBSs(i,k,bi,bj) = OBSs (i,k,bi,bj)
     &           + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
            OBSs(i,k,bi,bj) = OBSs(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           else if (iobcs .EQ. 4) then
            OBSu(i,k,bi,bj) = OBSu (i,k,bi,bj)
     &           + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
            OBSu(i,k,bi,bj) = OBSu(i,k,bi,bj)
     &           *maskW(i,j,k,bi,bj)
           else if (iobcs .EQ. 3) then
            OBSv(i,k,bi,bj) = OBSv (i,k,bi,bj)
     &           + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
            OBSv(i,k,bi,bj) = OBSv(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo

C--   End over iobcs loop
      enddo

#endif /* ALLOW_OBCSS_CONTROL */

      return
      end
