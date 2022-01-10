#include "CTRL_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_getobcsw(
     I                          myTime, myIter, myThid )

c     ==================================================================
c     SUBROUTINE ctrl_getobcsw
c     ==================================================================
c
c     o Get western obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c     modified: gebbie@mit.edu, 18-Mar-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_getobcsw
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_OBCSW_CONTROL
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
#endif /* ALLOW_OBCSW_CONTROL */

c     == routine arguments ==
      _RL     myTime
      integer myIter
      integer myThid

#ifdef ALLOW_OBCSW_CONTROL
c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer ilobcsw
      integer iobcs
      integer ip1
      _RL     obcswfac
      logical obcswfirst
      logical obcswchanged
      integer obcswcount0
      integer obcswcount1
cgg      _RL maskyz   (1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpfldyz (1-OLy:sNy+OLy,Nr,nSx,nSy)
      logical doglobalread
      logical ladinit
      character*(80) fnameobcsw
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
      jmin = 1-OLy
      jmax = sNy+OLy
      ip1  = 1

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
       ilobcsw=ilnblnk( xx_obcsw_file )
       write(fnameobcsw(1:80),'(2a,i10.10)')
     &      xx_obcsw_file(1:ilobcsw), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcswstartdate, xx_obcswperiod,
     O                   obcswfac, obcswfirst, obcswchanged,
     O                   obcswcount0,obcswcount1,
     I                   myTime, myIter, myThid )

      do iobcs = 1,nobcs

       if ( obcswfirst ) then
        call active_read_yz( fnameobcsw, tmpfldyz,
     &                       (obcswcount0-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcsw_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do j = jmin,jmax
            i = OB_Iw(j,bi,bj)
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
          do k = 1,Nr
           do j = jmin,jmax
            xx_obcsw1(j,k,bi,bj,iobcs)  = tmpfldyz (j,k,bi,bj)
cgg   &                                        *   maskyz (j,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

       if ( (obcswfirst) .or. (obcswchanged)) then

        do bj = jtlo,jthi
         do bi = itlo,ithi
          do k = 1,Nr
           do j = jmin,jmax
            xx_obcsw0(j,k,bi,bj,iobcs) = xx_obcsw1(j,k,bi,bj,iobcs)
            tmpfldyz (j,k,bi,bj)       = 0. _d 0
           enddo
          enddo
         enddo
        enddo

        call active_read_yz( fnameobcsw, tmpfldyz,
     &                       (obcswcount1-1)*nobcs+iobcs,
     &                       doglobalread, ladinit, optimcycle,
     &                       mythid, xx_obcsw_dummy )

        do bj = jtlo,jthi
         do bi = itlo,ithi
#ifdef ALLOW_OBCS_CONTROL_MODES
          if (iobcs .gt. 2) then
           do j = jmin,jmax
            i = OB_Iw(j,bi,bj)
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
          do k = 1,Nr
           do j = jmin,jmax
            xx_obcsw1 (j,k,bi,bj,iobcs) = tmpfldyz (j,k,bi,bj)
cgg   &                                        *   maskyz (j,k,bi,bj)
           enddo
          enddo
         enddo
        enddo
       endif

c--   Add control to model variable.
       do bj = jtlo, jthi
        do bi = itlo, ithi
c--   Calculate mask for tracer cells (0 => land, 1 => water).
         do k = 1,Nr
          do j = 1,sNy
           i = OB_Iw(j,bi,bj)
           IF ( i.EQ.OB_indexNone ) i = 1
           if (iobcs .EQ. 1) then
            OBWt(j,k,bi,bj) = OBWt (j,k,bi,bj)
     &           + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
            OBWt(j,k,bi,bj) = OBWt(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 2) then
            OBWs(j,k,bi,bj) = OBWs (j,k,bi,bj)
     &           + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
            OBWs(j,k,bi,bj) = OBWs(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 3) then
            OBWu(j,k,bi,bj) = OBWu (j,k,bi,bj)
     &           + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
            OBWu(j,k,bi,bj) = OBWu(j,k,bi,bj)
     &           *maskW(i+ip1,j,k,bi,bj)
           else if (iobcs .EQ. 4) then
            OBWv(j,k,bi,bj) = OBWv (j,k,bi,bj)
     &           + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &           + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
            OBWv(j,k,bi,bj) = OBWv(j,k,bi,bj)
     &           *maskS(i,j,k,bi,bj)
           endif
          enddo
         enddo
        enddo
       enddo

C--   End over iobcs loop
      enddo

#endif /* ALLOW_OBCSW_CONTROL */

      return
      end
