C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_getobcse.F,v 1.16 2014/10/09 00:49:26 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_getobcse(
     I                             mytime,
     I                             myiter,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE ctrl_getobcse
c     ==================================================================
c
c     o Get eastern obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c
c     ==================================================================
c     SUBROUTINE ctrl_getobcse
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_OBCSE_CONTROL
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
#include "optim.h"
#include "CTRL_OBCS.h"
#endif /* ALLOW_OBCSE_CONTROL */

c     == routine arguments ==
      _RL     mytime
      integer myiter
      integer mythid

#ifdef ALLOW_OBCSE_CONTROL
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ilobcse
      integer iobcs

      _RL     dummy
      _RL     obcsefac
      logical obcsefirst
      logical obcsechanged
      integer obcsecount0
      integer obcsecount1
      integer ip1

cgg      _RL maskyz   (1-oly:sny+oly,nr,nsx,nsy)
      _RL tmpfldyz (1-oly:sny+oly,nr,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnameobcse

#ifdef ALLOW_OBCS_CONTROL_MODES
      integer nk,nz
      _RL     tmpz (nr,nsx,nsy)
      _RL     stmp
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk


c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx
      ip1  = 0

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
       ilobcse=ilnblnk( xx_obcse_file )
       write(fnameobcse(1:80),'(2a,i10.10)')
     &      xx_obcse_file(1:ilobcse), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcsestartdate, xx_obcseperiod,
     O                   obcsefac, obcsefirst, obcsechanged,
     O                   obcsecount0,obcsecount1,
     I                   mytime, myiter, mythid )

      do iobcs = 1,nobcs

       if ( obcsefirst ) then
        call active_read_yz( fnameobcse, tmpfldyz,
     &                       (obcsecount0-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcse_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do j = jmin,jmax
            i = OB_Ie(j,bi,bj)
            IF ( i.EQ.OB_indexNone ) i = 1
cih    Determine number of open vertical layers.
            nz = 0
            do k = 1,Nr
              if (iobcs .eq. 3) then
                nz = nz + maskW(i+ip1,j,k,bi,bj)
              else
                nz = nz + maskS(i,j,k,bi,bj)
              endif
            end do
cih    Compute absolute velocities from the barotropic-baroclinic modes.
            do k = 1,Nr
             if (k.le.nz) then
              stmp = 0.
              do nk = 1,nz
               stmp = stmp +
     &         modesv(k,nk,nz)*tmpfldyz(j,nk,bi,bj)
              end do
               tmpz(k,bi,bj) = stmp
             else
              tmpz(k,bi,bj) = 0.
             end if
            enddo
            do k = 1,Nr
              if (iobcs .eq. 3) then
                tmpfldyz(j,k,bi,bj) = tmpz(k,bi,bj)
     &            *recip_hFacW(i+ip1,j,k,bi,bj)
              else
                tmpfldyz(j,k,bi,bj) = tmpz(k,bi,bj)
     &            *recip_hFacS(i,j,k,bi,bj)
              endif
            end do
           enddo
          endif
#endif
          do k = 1,nr
           do j = jmin,jmax
            xx_obcse1(j,k,bi,bj,iobcs)  = tmpfldyz (j,k,bi,bj)
cgg   &                                        *   maskyz (j,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

       if ( (obcsefirst) .or. (obcsechanged)) then

        do bj = jtlo,jthi
         do bi = itlo,ithi
          do j = jmin,jmax
           do k = 1,nr
            xx_obcse0(j,k,bi,bj,iobcs) = xx_obcse1(j,k,bi,bj,iobcs)
            tmpfldyz (j,k,bi,bj)       = 0. _d 0
           enddo
          enddo
         enddo
        enddo

        call active_read_yz( fnameobcse, tmpfldyz,
     &                       (obcsecount1-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcse_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do j = jmin,jmax
            i = OB_Ie(j,bi,bj)
            IF ( i.EQ.OB_indexNone ) i = 1
cih    Determine number of open vertical layers.
            nz = 0
            do k = 1,Nr
              if (iobcs .eq. 3) then
                nz = nz + maskW(i+ip1,j,k,bi,bj)
              else
                nz = nz + maskS(i,j,k,bi,bj)
              endif
            end do
cih    Compute absolute velocities from the barotropic-baroclinic modes.
            do k = 1,Nr
             if (k.le.nz) then
              stmp = 0.
              do nk = 1,nz
               stmp = stmp +
     &         modesv(k,nk,nz)*tmpfldyz(j,nk,bi,bj)
              end do
               tmpz(k,bi,bj) = stmp
             else
              tmpz(k,bi,bj) = 0.
             endif
            enddo
            do k = 1,Nr
              if (iobcs .eq. 3) then
                tmpfldyz(j,k,bi,bj) = tmpz(k,bi,bj)
     &            *recip_hFacW(i+ip1,j,k,bi,bj)
              else
                tmpfldyz(j,k,bi,bj) = tmpz(k,bi,bj)
     &            *recip_hFacS(i,j,k,bi,bj)
              endif
            end do
           enddo
          endif
#endif
          do k = 1,nr
           do j = jmin,jmax
            xx_obcse1 (j,k,bi,bj,iobcs) = tmpfldyz (j,k,bi,bj)
cgg   &                                        *   maskyz (j,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

c--   Add control to model variable.
       do bj = jtlo,jthi
        do bi = itlo,ithi
c--   Calculate mask for tracer cells (0 => land, 1 => water).
         do k = 1,nr
          do j = 1,sny
           i = OB_Ie(j,bi,bj)
           IF ( i.EQ.OB_indexNone ) i = 1
           if (iobcs .EQ. 1) then
            OBEt(j,k,bi,bj) = OBEt(j,k,bi,bj)
     &           + obcsefac            *xx_obcse0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsefac)*xx_obcse1(j,k,bi,bj,iobcs)
            OBEt(j,k,bi,bj) = OBEt(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 2) then
            OBEs(j,k,bi,bj) = OBEs(j,k,bi,bj)
     &           + obcsefac            *xx_obcse0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsefac)*xx_obcse1(j,k,bi,bj,iobcs)
            OBEs(j,k,bi,bj) = OBEs(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 3) then
            OBEu(j,k,bi,bj) = OBEu(j,k,bi,bj)
     &           + obcsefac            *xx_obcse0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsefac)*xx_obcse1(j,k,bi,bj,iobcs)
            OBEu(j,k,bi,bj) = OBEu(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 4) then
            OBEv(j,k,bi,bj) = OBEv(j,k,bi,bj)
     &           + obcsefac            *xx_obcse0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsefac)*xx_obcse1(j,k,bi,bj,iobcs)
            OBEv(j,k,bi,bj) = OBEv(j,k,bi,bj)
     &           *maskS(i,j,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo

C--   End over iobcs loop
      enddo

#endif /* ALLOW_OBCSE_CONTROL */

      return
      end
