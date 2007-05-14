
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif


      subroutine ctrl_getobcss(
     I                             mytime,
     I                             myiter,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE ctrl_getobcss
c     ==================================================================
c
c     o Get southern obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c
c     new flags: gebbie@mit.edu, 25 Jan 2003.
c
c     ==================================================================
c     SUBROUTINE ctrl_getobcss
c     ==================================================================

      implicit none

#ifdef ALLOW_OBCSS_CONTROL

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer ilobcss
      integer iobcs

      _RL     dummy
      _RL     obcssfac
      logical obcssfirst
      logical obcsschanged
      integer obcsscount0
      integer obcsscount1
      integer jp1

cgg      _RL maskxz   (1-olx:snx+olx,nr,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnameobcss

cgg(  Variables for splitting barotropic/baroclinic vels.
      _RL vbaro
      _RL vtop
cgg)

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
      jp1  = 1

cgg(  Initialize variables for balancing volume flux.
      vbaro = 0.d0
      vtop = 0.d0
cgg)

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
        ilobcss=ilnblnk( xx_obcss_file )
        write(fnameobcss(1:80),'(2a,i10.10)') 
     &       xx_obcss_file(1:ilobcss), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcssstartdate, xx_obcssperiod,
     O                   obcssfac, obcssfirst, obcsschanged,
     O                   obcsscount0,obcsscount1,
     I                   mytime, myiter, mythid )

      do iobcs = 1,nobcs
        if ( obcssfirst ) then
          call active_read_xz( fnameobcss, tmpfldxz, 
     &                         (obcsscount0-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcss_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax
                    j = OB_Js(I,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
                    tmpfldxz(i,1,bi,bj) = 0.d0
                    vtop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      vtop = tmpfldxz(i,k,bi,bj)*
     &                maskS(i,j+jp1,k,bi,bj) * delR(k) + vtop
cgg    Add the barotropic velocity component. 
                      if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                        tmpfldxz(i,k,bi,bj) = tmpfldxz(i,k,bi,bj)+ vbaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldxz(i,1,bi,bj) = tmpfldxz(i,1,bi,bj) 
     &                                      - vtop / delR(1)
                  enddo
                enddo
              enddo
            endif

            if (iobcs .eq. 4) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax
                    j = OB_Js(I,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
                    tmpfldxz(i,1,bi,bj) = 0.d0
                    vtop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      vtop = tmpfldxz(i,k,bi,bj)*
     &                maskW(i,j,k,bi,bj) * delR(k) + vtop
cgg    Add the barotropic velocity component. 
                      if (maskW(i,j,k,bi,bj) .ne. 0.) then
                        tmpfldxz(i,k,bi,bj) = tmpfldxz(i,k,bi,bj)+ vbaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldxz(i,1,bi,bj) = tmpfldxz(i,1,bi,bj) 
     &                                      - vtop / delR(1)
                  enddo
                enddo
              enddo
            endif
          endif

#endif /* ALLOW_CTRL_OBCS_BALANCE */

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                  xx_obcss1(i,k,bi,bj,iobcs)  = tmpfldxz (i,k,bi,bj)
cgg     &                                        *   maskxz (i,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        endif

        if ( (obcssfirst) .or. (obcsschanged)) then
          
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                  tmpfldxz(i,k,bi,bj) = xx_obcss1(i,k,bi,bj,iobcs)
                enddo
              enddo
            enddo
          enddo

          call exf_swapffields_xz( tmpfldxz2, tmpfldxz, mythid)

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                  xx_obcss0(i,k,bi,bj,iobcs) = tmpfldxz2(i,k,bi,bj)
                enddo
              enddo
            enddo
          enddo

          call active_read_xz( fnameobcss, tmpfldxz, 
     &                         (obcsscount1-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcss_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax
                    j = OB_Js(I,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
                    tmpfldxz(i,1,bi,bj) = 0.d0
                    vtop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      vtop = tmpfldxz(i,k,bi,bj)*
     &                maskS(i,j+jp1,k,bi,bj) * delR(k) + vtop
cgg    Add the barotropic velocity component. 
                      if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                        tmpfldxz(i,k,bi,bj) = tmpfldxz(i,k,bi,bj)+ vbaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldxz(i,1,bi,bj) = tmpfldxz(i,1,bi,bj) 
     &                                      - vtop / delR(1)
                  enddo
                enddo
              enddo
            endif

            if (iobcs .eq. 4) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax
                    j = OB_Js(I,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
                    tmpfldxz(i,1,bi,bj) = 0.d0
                    vtop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      vtop = tmpfldxz(i,k,bi,bj)*
     &                maskW(i,j,k,bi,bj) * delR(k) + vtop
cgg    Add the barotropic velocity component. 
                      if (maskW(i,j,k,bi,bj) .ne. 0.) then
                        tmpfldxz(i,k,bi,bj) = tmpfldxz(i,k,bi,bj)+ vbaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldxz(i,1,bi,bj) = tmpfldxz(i,1,bi,bj) 
     &                                      - vtop / delR(1)
                  enddo
                enddo
              enddo
            endif
          endif

#endif /* ALLOW_CTRL_OBCS_BALANCE */

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                  xx_obcss1 (i,k,bi,bj,iobcs) = tmpfldxz (i,k,bi,bj)
cgg     &                                        *   maskxz (i,k,bi,bj)
                 enddo
              enddo
            enddo
          enddo
        endif

c--     Add control to model variable.
        do bj = jtlo,jthi
           do bi = itlo,ithi
c--        Calculate mask for tracer cells (0 => land, 1 => water).
              do k = 1,nr
                 do i = 1,snx
                    j = OB_Js(I,bi,bj)
                    if (iobcs .EQ. 1) then
                       OBSt(i,k,bi,bj) = OBSt (i,k,bi,bj)
     &                 + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
                       OBSt(i,k,bi,bj) = OBSt(i,k,bi,bj)
     &                      *maskS(i,j+jp1,k,bi,bj)
                    else if (iobcs .EQ. 2) then
                       OBSs(i,k,bi,bj) = OBSs (i,k,bi,bj)
     &                 + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
                       OBSs(i,k,bi,bj) = OBSs(i,k,bi,bj)
     &                      *maskS(i,j+jp1,k,bi,bj)
                    else if (iobcs .EQ. 4) then
                       OBSu(i,k,bi,bj) = OBSu (i,k,bi,bj)
     &                 + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
                       OBSu(i,k,bi,bj) = OBSu(i,k,bi,bj)
     &                      *maskW(i,j,k,bi,bj)
                    else if (iobcs .EQ. 3) then
                       OBSv(i,k,bi,bj) = OBSv (i,k,bi,bj)
     &                 + obcssfac            *xx_obcss0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcssfac)*xx_obcss1(i,k,bi,bj,iobcs)
                       OBSv(i,k,bi,bj) = OBSv(i,k,bi,bj)
     &                      *maskS(i,j+jp1,k,bi,bj)
                    endif
                 enddo
              enddo
           enddo
        enddo

C--   End over iobcs loop
      enddo

#else /* ALLOW_OBCSS_CONTROL undefined */

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

c--   CPP flag ALLOW_OBCSS_CONTROL undefined.

#endif /* ALLOW_OBCSS_CONTROL */

      end

