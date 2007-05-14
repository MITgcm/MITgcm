
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_volflux(
     I                       obcsncount,
     O                       sumarea,
     O                       sumflux, mythid
     &                     )

c     ==================================================================
c     SUBROUTINE ctrl_volflux
c     ==================================================================
c
c     o calculate the o.b. volume flux due to control adjustments.
c     o Assume the calendar is identical
c       for all open boundaries. Need to save the barotropic adjustment
c       velocity so it can be used in all ctrl_getobcs files. 
c     o WARNING: eastern boundary (not defined) filenames have been a 
c                problem in the past.
c
c     - started G. Gebbie, MIT-WHOI, 15-June-2002
c     ==================================================================
c     SUBROUTINE ctrl_obcsvol
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBCS
# include "OBCS.h"
#endif

#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer obcsncount
      _RL sumflux
      _RL sumarea
      integer mythid


#ifdef BALANCE_CONTROL_VOLFLUX_GLOBAL
c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer il
      integer iobcs
      integer ip1
      integer jp1
      integer nrec
      integer ilfld
      integer igg

      _RL tmpflux
      _RL tmparea
      _RL dummy
      _RL gg
      _RL tmpx
      _RL tmpy
      _RL obcsnfac
      character*(80) fnamefldn
      character*(80) fnameflds
      character*(80) fnamefldw
      character*(80) fnameflde

      logical doglobalread
      logical ladinit

#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

cgg   Assume the number of records is the same for
cgg   all boundaries. Needs to be improved someday.

#if   (defined (ALLOW_OBCS_CONTROL) || \
       defined (ALLOW_OBCS_COST_CONTRIBUTION))

      tmpflux = 0. d 0
      tmparea = 0. d 0
      sumarea = 0. d 0
      sumflux = 0. d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8)')
     &  ' ctrl_volflux: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      if (optimcycle .ge. 0) then
c
#ifdef ALLOW_OBCSN_CONTROL
         ilfld=ilnblnk( xx_obcsn_file )
         write(fnamefldn(1:80),'(2a,i10.10)') 
     &        xx_obcsn_file(1:ilfld),'.', optimcycle
#endif
#ifdef ALLOW_OBCSS_CONTROL
         ilfld=ilnblnk( xx_obcss_file )
         write(fnameflds(1:80),'(2a,i10.10)') 
     &        xx_obcss_file(1:ilfld),'.',optimcycle
#endif
#ifdef ALLOW_OBCSW_CONTROL
         ilfld=ilnblnk( xx_obcsw_file )
         write(fnamefldw(1:80),'(2a,i10.10)') 
     &        xx_obcsw_file(1:ilfld),'.',optimcycle
#endif
#ifdef ALLOW_OBCSE_CONTROL
         ilfld=ilnblnk( xx_obcse_file )
         write(fnameflde(1:80),'(2a,i10.10)') 
     &        xx_obcse_file(1:ilfld),'.',optimcycle
#endif
c
      endif

#ifdef ALLOW_OBCSN_CONTROL
        jp1 = 0

        call active_read_xz(fnamefldn,tmpfldxz,
     &                         (obcsncount-1)*nobcs+3, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcsn_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

            tmpflux = 0. d0
            tmparea = 0. d0

            do k = 1, Nr
              do i = imin,imax  
                j = Ob_Jn(I,bi,bj)
                if (j.ne.0) then
cgg -- Alternatively I could read the maskobcs file. But this gives the same result.
                  if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                    if(ob_iw(j,bi,bj).ne.i .and.ob_ie(j,bi,bj).ne.i)then
CGG -- Barotropic velocity stored in level 1.
                      tmpx = tmpfldxz(i,1,bi,bj)

cgg -- Pick the special point where barotropic velocity loses one degree of freedom.
cgg -- Add up the cross-sectional area of this column for later calculations.
                      if (ob_iw(j,bi,bj).eq.(i-1) .and. 
     &                    ob_iw(j,bi,bj).ne. 0) then
                        tmpx = 0.
                        tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                        print*,'tmparea',tmparea
                      endif
cgg -- Positive is flux in.
                    tmpflux = tmpflux -tmpx*delR(k)*dxg(i,j+jp1,bi,bj)
                    endif
                  endif
                endif
              enddo
            enddo

            sumarea        = sumarea+ tmparea
            sumflux         = sumflux + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSS_CONTROL
        jp1 = 1

        call active_read_xz(fnameflds,tmpfldxz,
     &                         (obcsncount-1)*nobcs+3, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

            tmpflux = 0. d 0
#ifndef ALLOW_OBCSN_CONTROL
            tmparea = 0. d 0
#endif
            do k = 1, Nr
              do i = imin,imax
                j = Ob_Js(I,bi,bj)
                if (j .ne. 0) then
                  if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                    if (ob_iw(j,bi,bj).ne.i.and.ob_ie(j,bi,bj).ne.i)then
                      tmpx = tmpfldxz(i,1,bi,bj)
#ifndef ALLOW_OBCSN_CONTROL
cgg -- Pick the special point where barotropic velocity loses one degree of freedom.
cgg -- Add up the cross-sectional area of this column for later calculations.
cgg -- This is just the backup case where the northern boundary does not exist.
cgg -- warning: never been tested.
                      if (ob_iw(j,bi,bj).eq.(i-1).and.
     &                    ob_iw(j,bi,bj).ne. 0) then
                        tmpx = 0.
                        tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                        print*,'tmparea',tmparea
                      endif
#endif
cgg -- Positive is flux in.
                      tmpflux = tmpflux +tmpx*delR(k)*dxg(i,j+jp1,bi,bj)
                    endif
                  endif
                endif
              enddo
            enddo
#ifndef ALLOW_OBCSN_CONTROL
            sumarea        = sumarea+ tmparea
#endif
            sumflux         = sumflux + tmpflux          
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSW_CONTROL
        ip1 = 1

        call active_read_yz( fnamefldw, tmpfldyz,
     &      (obcsncount-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0
#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
            tmparea = 0. d 0
#endif 
#endif
            do k = 1, Nr
              do j = jmin,jmax
                i = ob_iw(j,bi,bj)
                if ( i .ne. 0) then
                  if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_jn(i,bi,bj).ne.j.and. ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,1,bi,bj)

#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
cgg -- Pick the special point where barotropic velocity loses one degree of freedom.
cgg -- Add up the cross-sectional area of this column for later calculations.
cgg -- This is an untested backup case.
                      if (ob_jn(i,bi,bj).eq.(j+1) .and.
     &                    ob_jn(i,bi,bj).ne. 0) then
                        tmpy = 0.
                        tmparea = tmparea + delR(k) * dyg(i+ip1,j,bi,bj)
                        print*,'tmparea',tmparea
                      endif
#endif
#endif
cgg -- Positive is flux in.
                    tmpflux = tmpflux + tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
                  endif
                  endif
                endif
              enddo
            enddo
#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL 
            sumarea        =sumarea + tmparea
#endif
#endif
            sumflux         = sumflux + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSE_CONTROL
        ip1 = 0

        call active_read_yz( fnameflde, tmpfldyz,
            (obcsncount-1)*nobcs+3, doglobalread,
                      ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0

#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
#ifndef ALLOW_OBCSW_CONTROL
            tmparea = 0. d 0
#endif
#endif
#endif
 
           do k = 1, Nr
              do j = jmin,jmax
                i = ob_ie(j,bi,bj)
                if ( i .ne. 0) then
                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_jn(i,bi,bj).ne.j .and.ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,1,bi,bj)

#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
#ifndef ALLOW_OBCSW_CONTROL
cgg -- Pick the special point where barotropic velocity loses one degree of freedom.
cgg -- Add up the cross-sectional area of this column for later calculations.
cgg -- This is an untested backup case.
                      if (ob_jn(i,bi,bj).eq.(j+1) .and.
     &                    ob_jn(i,bi,bj).ne. 0) then
                        tmpy = 0.
                        tmparea = tmparea + delR(k) * dyg(i+ip1,j,bi,bj)
                        print*,'tmparea',tmparea
                      endif
#endif
#endif
#endif

cgg -- Positive is flux in.
                    tmpflux = tmpflux - tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
#ifndef ALLOW_OBCSW_CONTROL
                    tmparea = tmparea + delR(k) *dyg(i+ip1,j,bi,bj)
#endif
#endif
#endif
                  endif
                endif
                endif
              enddo
            enddo

#ifndef ALLOW_OBCSN_CONTROL
#ifndef ALLOW_OBCSS_CONTROL
#ifndef ALLOW_OBCSW_CONTROL
            sumarea        = sumarea+ tmparea
#endif
#endif
#endif
            sumflux         = sumflux + tmpflux
          enddo
        enddo
#endif

#endif

#endif /* BALANCE_CONTROL_VOLFLUX_GLOBAL */

      return
      end







