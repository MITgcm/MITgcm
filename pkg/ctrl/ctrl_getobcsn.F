
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif


      subroutine ctrl_getobcsn(
     I                             mytime,
     I                             myiter,
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE ctrl_getobcsn
c     ==================================================================
c
c     o Get northern obc of the control vector and add it
c       to dyn. fields
c
c     started: heimbach@mit.edu, 29-Aug-2001
c
c     modified: gebbie@mit.edu, 18-Mar-2003
c     ==================================================================
c     SUBROUTINE ctrl_getobcsn
c     ==================================================================

      implicit none

#ifdef ALLOW_OBCSN_CONTROL

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
      integer ilobcsn
      integer iobcs
      integer jp1

      _RL     dummy
      _RL     obcsnfac
      logical obcsnfirst
      logical obcsnchanged
      integer obcsncount0
      integer obcsncount1

cgg      _RL maskxz   (1-olx:snx+olx,nr,nsx,nsy)

      logical doglobalread
      logical ladinit

      character*(80) fnameobcsn

cgg(  Variables for splitting barotropic/baroclinic vels.
      _RL vbaro
      _RL vtop

c     == external functions ==

      integer  ilnblnk
      external ilnblnk


c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
cgg      jmin = 1-oly
cgg      jmax = sny+oly
cgg      imin = 1-olx
cgg      imax = snx+olx

      jmin = 1
      jmax = sny
      imin = 1
      imax = snx
      jp1  = 0

cgg(  Initialize variables for balancing volume flux.
      vbaro = 0.d0
      vtop = 0.d0
cgg)

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
     I                   mytime, myiter, mythid )

      do iobcs = 1,nobcs
        if ( obcsnfirst ) then
          call active_read_xz( fnameobcsn, tmpfldxz, 
     &                         (obcsncount0-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcsn_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 4.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
cgg         Except for the special point which balances barotropic vol.flux.
cgg         Special column in the NW corner. 
                    j = OB_Jn(I,bi,bj)
                    if (ob_iw(j,bi,bj).eq.(i-1).and.
     &                  ob_iw(j,bi,bj).ne. 0) then
                        print*,'Apply shiftvel1 @ i,j'
                        print*,shiftvel(1),i,j
                      vbaro = shiftvel(1)
                    endif
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

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
cgg         Except for the special point which balances barotropic vol.flux.
cgg         Special column in the NW corner. 
                    j = OB_Jn(I,bi,bj)
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
                  xx_obcsn1(i,k,bi,bj,iobcs)  = tmpfldxz (i,k,bi,bj)
cgg     &                                        *   maskxz (i,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        endif

        if ( (obcsnfirst) .or. (obcsnchanged)) then
          
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                  tmpfldxz(i,k,bi,bj) = xx_obcsn1(i,k,bi,bj,iobcs)
                enddo
              enddo
            enddo
          enddo

          call exf_swapffields_xz( tmpfldxz2, tmpfldxz, mythid)

          do bj = jtlo,jthi
            do bi = itlo,ithi
              do k = 1,nr
                do i = imin,imax
                    xx_obcsn0(i,k,bi,bj,iobcs) = tmpfldxz2(i,k,bi,bj)
                enddo
              enddo
            enddo
          enddo

          call active_read_xz( fnameobcsn, tmpfldxz, 
     &                         (obcsncount1-1)*nobcs+iobcs,
     &                         doglobalread, ladinit, optimcycle,
     &                         mythid, xx_obcsn_dummy )

#ifdef ALLOW_CTRL_OBCS_BALANCE

          if ( optimcycle .gt. 0) then           
            if (iobcs .eq. 3) then
cgg         Special attention is needed for the normal velocity.
cgg         For the north, this is the v velocity, iobcs = 3.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
cgg         Except for the special point which balances barotropic vol.flux.
cgg         Special column in the NW corner. 
                    j = OB_Jn(I,bi,bj)
                    if (ob_iw(j,bi,bj).eq.(i-1).and.
     &                    ob_iw(j,bi,bj).ne. 0) then
                        print*,'correct vbaro',vbaro
                        print*,'Apply shiftvel2 @ i,j'
                        print*,shiftvel(2),i,j
                      vbaro = shiftvel(2)
                    endif
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
cgg         For the north, this is the v velocity, iobcs = 3.
cgg         This is done on a columnwise basis here.
              do bj = jtlo,jthi
                do bi = itlo, ithi
                  do i = imin,imax

cgg         The barotropic velocity is stored in the level 1.
                    vbaro = tmpfldxz(i,1,bi,bj)
cgg         Except for the special point which balances barotropic vol.flux.
cgg         Special column in the NW corner. 
                    j = OB_Jn(I,bi,bj)
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
                  xx_obcsn1 (i,k,bi,bj,iobcs) = tmpfldxz (i,k,bi,bj)
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
                    j = OB_Jn(I,bi,bj)
                    if (iobcs .EQ. 1) then
                       OBNt(i,k,bi,bj) = OBNt (i,k,bi,bj)
     &                 + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
                       OBNt(i,k,bi,bj) = OBNt(i,k,bi,bj)
     &                         *maskS(i,j+jp1,k,bi,bj)
cgg     &                      *maskxz(i,k,bi,bj)
                    else if (iobcs .EQ. 2) then
                       OBNs(i,k,bi,bj) = OBNs (i,k,bi,bj)
     &                 + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
                       OBNs(i,k,bi,bj) = OBNs(i,k,bi,bj)
     &                         *maskS(i,j+jp1,k,bi,bj)
cgg     &                      *maskxz(i,k,bi,bj)
                    else if (iobcs .EQ. 4) then
                       OBNu(i,k,bi,bj) = OBNu (i,k,bi,bj)
     &                 + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
                       OBNu(i,k,bi,bj) = OBNu(i,k,bi,bj)
     &                         *maskW(i,j,k,bi,bj)
cgg     &                      *maskxz(i,k,bi,bj)
                    else if (iobcs .EQ. 3) then
                       OBNv(i,k,bi,bj) = OBNv (i,k,bi,bj)
     &                 + obcsnfac            *xx_obcsn0(i,k,bi,bj,iobcs)
     &                 + (1. _d 0 - obcsnfac)*xx_obcsn1(i,k,bi,bj,iobcs)
                       OBNv(i,k,bi,bj) = OBNv(i,k,bi,bj)
     &                         *maskS(i,j+jp1,k,bi,bj)
cgg     &                      *maskxz(i,k,bi,bj)
                    endif
                 enddo
              enddo
           enddo
        enddo

C--   End over iobcs loop
      enddo
      
#else /* ALLOW_OBCSN_CONTROL undefined */

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

c--   CPP flag ALLOW_OBCSN_CONTROL undefined.

#endif /* ALLOW_OBCSN_CONTROL */

      end





