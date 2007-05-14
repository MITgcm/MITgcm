
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif


      subroutine ctrl_getobcsw(
     I                             mytime,
     I                             myiter,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE ctrl_getobcsw
c     ==================================================================
c
c     o Get western obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c
c     modified: gebbie@mit.edu, 18-Mar-2003
c     ==================================================================
c     SUBROUTINE ctrl_getobcsw
c     ==================================================================

      implicit none

#ifdef ALLOW_OBCSW_CONTROL

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
      integer ilobcsw
      integer iobcs
      integer ip1

      _RL     dummy
      _RL     obcswfac
      logical obcswfirst
      logical obcswchanged
      integer obcswcount0
      integer obcswcount1

cgg      _RL maskyz   (1-oly:sny+oly,nr,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnameobcsw

cgg(  Variables for splitting barotropic/baroclinic vels.
      _RL ubaro
      _RL utop
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
      ip1  = 1

cgg(  Initialize variables for balancing volume flux.
      ubaro = 0.d0
      utop = 0.d0
cgg)

c--   Now, read the control vector.
      doglobalread = .false.
      ladinit      = .false.

      if (optimcycle .ge. 0) then
        ilobcsw=ilnblnk( xx_obcsw_file )
        write(fnameobcsw(1:80),'(2a,i10.10)') 
     &       xx_obcsw_file(1:ilobcsw), '.', optimcycle
      endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcswstartdate, xx_obcswperiod,
     O                   obcswfac, obcswfirst, obcswchanged,
     O                   obcswcount0,obcswcount1,
     I                   mytime, myiter, mythid )

      do iobcs = 1,nobcs
        if ( obcswfirst ) then
          call active_read_yz( fnameobcsw, tmpfldyz, 
     &                         (obcswcount0-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcsw_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do j = jmin,jmax
                    i = OB_Iw(J,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    ubaro = tmpfldyz(j,1,bi,bj)
                    tmpfldyz(j,1,bi,bj) = 0.d0
                    utop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      utop = tmpfldyz(j,k,bi,bj)*
     &                maskW(i+ip1,j,k,bi,bj) * delR(k) + utop
cgg    Add the barotropic velocity component. 
                      if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                        tmpfldyz(j,k,bi,bj) = tmpfldyz(j,k,bi,bj)+ ubaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                  tmpfldyz(j,1,bi,bj) = tmpfldyz(j,1,bi,bj) 
     &                                      - utop / delR(1)
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
                  do j = jmin,jmax
                    i = OB_Iw(J,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    ubaro = tmpfldyz(j,1,bi,bj)
                    tmpfldyz(j,1,bi,bj) = 0.d0
                    utop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      utop = tmpfldyz(j,k,bi,bj)*
     &                maskS(i,j,k,bi,bj) * delR(k) + utop
cgg    Add the barotropic velocity component. 
                      if (maskS(i,j,k,bi,bj) .ne. 0.) then
                        tmpfldyz(j,k,bi,bj) = tmpfldyz(j,k,bi,bj)+ ubaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                  tmpfldyz(j,1,bi,bj) = tmpfldyz(j,1,bi,bj) 
     &                                      - utop / delR(1)
                  enddo
                enddo
              enddo
            endif
          endif

#endif /* ALLOW_CTRL_OBCS_BALANCE */

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do j = jmin,jmax
                  xx_obcsw1(j,k,bi,bj,iobcs)  = tmpfldyz (j,k,bi,bj)
cgg     &                                        *   maskyz (j,k,bi,bj)
                 enddo
              enddo
            enddo
          enddo
        endif

        if ( (obcswfirst) .or. (obcswchanged)) then
          
cgg(    This is a terribly long way to do it. However, the dimensions do not exactly
cgg     match up. I will blame Fortran for the ugliness.

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do j = jmin,jmax
                  tmpfldyz(j,k,bi,bj) = xx_obcsw1(j,k,bi,bj,iobcs)
                enddo
              enddo
            enddo
          enddo

          call exf_swapffields_yz( tmpfldyz2, tmpfldyz, mythid)

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do j = jmin,jmax
                  xx_obcsw0(j,k,bi,bj,iobcs) = tmpfldyz2(j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo

          call active_read_yz( fnameobcsw, tmpfldyz, 
     &                         (obcswcount1-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcsw_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do j = jmin,jmax
                    i = OB_Iw(J,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    ubaro = tmpfldyz(j,1,bi,bj)
                    tmpfldyz(j,1,bi,bj) = 0.d0
                    utop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      utop = tmpfldyz(j,k,bi,bj)*
     &                maskW(i+ip1,j,k,bi,bj) * delR(k) + utop
cgg    Add the barotropic velocity component. 
                      if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                        tmpfldyz(j,k,bi,bj) = tmpfldyz(j,k,bi,bj)+ ubaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldyz(j,1,bi,bj) = tmpfldyz(j,1,bi,bj) 
     &                                      - utop / delR(1)
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
                  do j = jmin,jmax
                    i = OB_Iw(J,bi,bj)

cgg         The barotropic velocity is stored in the level 1.
                    ubaro = tmpfldyz(j,1,bi,bj)
                    tmpfldyz(j,1,bi,bj) = 0.d0
                    utop = 0.d0

                    do k = 1,Nr
cgg    If cells are not full, this should be modified with hFac.
cgg    
cgg    The xx field (tmpfldxz) does not contain the velocity at the 
cgg    surface level. This velocity is not independent; it must 
cgg    exactly balance the volume flux, since we are dealing with
cgg    the baroclinic velocity structure.. 
                      utop = tmpfldyz(j,k,bi,bj)*
     &                maskS(i,j,k,bi,bj) * delR(k) + utop
cgg    Add the barotropic velocity component. 
                      if (maskS(i,j,k,bi,bj) .ne. 0.) then
                        tmpfldyz(j,k,bi,bj) = tmpfldyz(j,k,bi,bj)+ ubaro
                      endif
                    enddo
cgg    Compute the baroclinic velocity at level 1. Should balance flux.
                    tmpfldyz(j,1,bi,bj) = tmpfldyz(j,1,bi,bj) 
     &                                      - utop / delR(1)
                  enddo
                enddo
              enddo
            endif
          endif

#endif /* ALLOW_CTRL_OBCS_BALANCE */

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do j = jmin,jmax
                  xx_obcsw1 (j,k,bi,bj,iobcs) = tmpfldyz (j,k,bi,bj)
cgg     &                                        *   maskyz (j,k,bi,bj)
                 enddo
              enddo
            enddo
          enddo
        endif

c--     Add control to model variable.
        do bj = jtlo, jthi
           do bi = itlo, ithi
c--        Calculate mask for tracer cells (0 => land, 1 => water).
              do k = 1,nr
                 do j = 1,sny
                    i = OB_Iw(j,bi,bj)
                    if (iobcs .EQ. 1) then
                       OBWt(j,k,bi,bj) = OBWt (j,k,bi,bj)
     &                 + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
                       OBWt(j,k,bi,bj) = OBWt(j,k,bi,bj)
     &                      *maskW(i+ip1,j,k,bi,bj)
                    else if (iobcs .EQ. 2) then
                       OBWs(j,k,bi,bj) = OBWs (j,k,bi,bj)
     &                 + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
                       OBWs(j,k,bi,bj) = OBWs(j,k,bi,bj)
     &                      *maskW(i+ip1,j,k,bi,bj)
                    else if (iobcs .EQ. 3) then
                       OBWu(j,k,bi,bj) = OBWu (j,k,bi,bj)
     &                 + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
                       OBWu(j,k,bi,bj) = OBWu(j,k,bi,bj)
     &                      *maskW(i+ip1,j,k,bi,bj)
                    else if (iobcs .EQ. 4) then
                       OBWv(j,k,bi,bj) = OBWv (j,k,bi,bj)
     &                 + obcswfac            *xx_obcsw0(j,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcswfac)*xx_obcsw1(j,k,bi,bj,iobcs)
                       OBWv(j,k,bi,bj) = OBWv(j,k,bi,bj)
     &                      *maskS(i,j,k,bi,bj)
                    endif
                 enddo
              enddo
           enddo
        enddo

C--   End over iobcs loop
      enddo

#else /* ALLOW_OBCSW_CONTROL undefined */

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

c--   CPP flag ALLOW_OBCSW_CONTROL undefined.

#endif /* ALLOW_OBCSW_CONTROL */

      end

