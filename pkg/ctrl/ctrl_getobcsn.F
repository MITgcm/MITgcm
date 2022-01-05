#include "CTRL_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_getobcsn(
     I                          myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_getobcsn
c     ==================================================================
c
c     o Get northern obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c     modified: gebbie@mit.edu, 18-Mar-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_getobcsn
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_OBCSN_CONTROL
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
#endif /* ALLOW_OBCSN_CONTROL */

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

#ifdef ALLOW_OBCSN_CONTROL
c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer imin,imax
      integer ilobcsn
      integer iobcs
      integer jp1
      _RL     obcsnfac
      logical obcsnfirst
      logical obcsnchanged
      integer obcsncount0
      integer obcsncount1
cgg      _RL maskxz   (1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL tmpfldxz (1-OLx:sNx+OLx,Nr,nSx,nSy)
      logical doglobalread
      logical ladinit
      character*(80) fnameobcsn
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
cgg      imin = 1-olx
cgg      imax = snx+olx
      imin = 1
      imax = sNx
      jp1  = 0

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
        ilobcsn=ilnblnk( xx_obcsn_file )
        write(fnameobcsn(1:80),'(2a,i10.10)')
     &       xx_obcsn_file(1:ilobcsn), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcsnstartdate, xx_obcsnperiod,
     O                   obcsnfac, obcsnfirst, obcsnchanged,
     O                   obcsncount0,obcsncount1,
     I                   myTime, myIter, myThid )

      do iobcs = 1,nobcs

       if ( obcsnfirst ) then
        call active_read_xz( fnameobcsn, tmpfldxz,
     &                       (obcsncount0-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcsn_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do i = imin,imax
            j = OB_Jn(i,bi,bj)
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
            xx_obcsn1(i,k,bi,bj,iobcs)  = tmpfldxz (i,k,bi,bj)
cgg   &                                    *   maskxz (i,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

       if ( (obcsnfirst) .or. (obcsnchanged)) then

        do bj = jtlo,jthi
         do bi = itlo,ithi
          do k = 1,Nr
           do i = imin,imax
            xx_obcsn0(i,k,bi,bj,iobcs) = xx_obcsn1(i,k,bi,bj,iobcs)
            tmpfldxz (i,k,bi,bj)       = 0. _d 0
           enddo
          enddo
         enddo
        enddo

        call active_read_xz( fnameobcsn, tmpfldxz,
     &                       (obcsncount1-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcsn_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do i = imin,imax
            j = OB_Jn(i,bi,bj)
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
            xx_obcsn1 (i,k,bi,bj,iobcs) = tmpfldxz (i,k,bi,bj)
cgg   &                                        *   maskxz (i,k,bi,bj)
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
           j = OB_Jn(i,bi,bj)
           IF ( j.EQ.OB_indexNone ) j = 1
           if (iobcs .EQ. 1) then
            OBNt(i,k,bi,bj) = OBNt (i,k,bi,bj)
     &           + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
            OBNt(i,k,bi,bj) = OBNt(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           else if (iobcs .EQ. 2) then
            OBNs(i,k,bi,bj) = OBNs (i,k,bi,bj)
     &           + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
            OBNs(i,k,bi,bj) = OBNs(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           else if (iobcs .EQ. 4) then
            OBNu(i,k,bi,bj) = OBNu (i,k,bi,bj)
     &           + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
            OBNu(i,k,bi,bj) = OBNu(i,k,bi,bj)
     &           *maskW(i,j,k,bi,bj)
           else if (iobcs .EQ. 3) then
            OBNv(i,k,bi,bj) = OBNv (i,k,bi,bj)
     &           + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
            OBNv(i,k,bi,bj) = OBNv(i,k,bi,bj)
     &           *maskS(i,j+jp1,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo

C--   End over iobcs loop
      enddo

#endif /* ALLOW_OBCSN_CONTROL */

      return
      end
