
#include "CTRL_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine ctrl_obcsvol(
     I                       mytime,
     I                       myiter,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE ctrl_obcsvol
c     ==================================================================
c
c     o volumetrically balance the control vector contribution. 
c     o Assume the calendar is identical
c       for all open boundaries. Need to save the barotropic adjustment
c       velocity so it can be used in all ctrl_getobcs files. 
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

      integer myiter
      _RL     mytime
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

      _RL sumvol
      _RL sumarea
      _RL tmpflux
      _RL tmparea
      _RL dummy
      _RL gg
      _RL tmpx
      _RL tmpy
      character*(80) fnamefldn
      character*(80) fnameflds
      character*(80) fnamefldw
      character*(80) fnameflde

      logical doglobalread
      logical ladinit
      logical obcsnfirst, obcsnchanged
      integer obcsncount0, obcsncount1
      _RL obcsnfac
      
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
cgg   all boundaries.

      tmpflux= 0. d 0
      tmparea= 0. d 0
      sumarea= 0. d 0
      sumvol = 0. d 0

#ifdef ECCO_VERBOSE
      _BEGIN_MASTER( mythid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8)')
     &  ' ctrl_obcsvol: number of records to process: ',nrec
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

c--   Get the counters, flags, and the interpolation factor.
      call ctrl_GetRec( 'xx_obcsn',
     O                   obcsnfac, obcsnfirst, obcsnchanged,
     O                   obcsncount0,obcsncount1,
     I                   mytime, myiter, mythid )

      if (optimcycle .ge. 0) then
c
         ilfld=ilnblnk( xx_obcsn_file )
         write(fnamefldn(1:80),'(2a,i10.10)') 
     &        xx_obcsn_file(1:ilfld),'.', optimcycle
         ilfld=ilnblnk( xx_obcss_file )
         write(fnameflds(1:80),'(2a,i10.10)') 
     &        xx_obcss_file(1:ilfld),'.',optimcycle
         ilfld=ilnblnk( xx_obcsw_file )
         write(fnamefldw(1:80),'(2a,i10.10)') 
     &        xx_obcsw_file(1:ilfld),'.',optimcycle
         ilfld=ilnblnk( xx_obcse_file )
         write(fnameflde(1:80),'(2a,i10.10)') 
     &        xx_obcse_file(1:ilfld),'.',optimcycle
c
      endif

c--   Loop over records. For north boundary, we only need V velocity.

      if ( obcsnfirst ) then

        shiftvel(1) = 0. d0
        shiftvel(2) = 0. d0

#ifdef ALLOW_OBCSN_CONTROL
        jp1 = 0

        call active_read_xz(fnamefldn,tmpfldxz,
     &                         (obcsncount0-1)*nobcs+4, doglobalread,
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
                if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if(ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i) then
                    tmpx = tmpfldxz(i,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux - tmpx* delR(k)*dxg(i,j+jp1,bi,bj)
                    tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                  endif
                endif
              enddo
            enddo

            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSS_CONTROL
        jp1 = 1

        call active_read_xz(fnameflds,tmpfldxz,
     &                         (obcsncount0-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do i = imin,imax
                j = Ob_Js(I,bi,bj)
                if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_iw(j,bi,bj).ne.i .and.ob_ie(j,bi,bj).ne.i) then
                    tmpx = tmpfldxz(i,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux + tmpx* delR(k)*dxg(i,j+jp1,bi,bj)
                    tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux          
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSW_CONTROL
        ip1 = 1

        call active_read_yz( fnamefldw, tmpfldyz,
     &      (obcsncount0-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do j = jmin,jmax
                i = ob_iw(j,bi,bj)
                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux + tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
                    tmparea = tmparea + delR(k)*dyg(i+ip1,j,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        =sumarea + tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSE_CONTROL
        ip1 = 0

        call active_read_yz( fnameflde, tmpfldyz,
            (obcsncount0-1)*nobcs+3, doglobalread,
                      ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do j = jmin,jmax
                i = ob_ie(j,bi,bj)
                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux - tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
                    tmparea = tmparea + delR(k) *dyg(i+ip1,j,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

c--   Do the global summation.
        _GLOBAL_SUM_R8( sumvol, mythid )
        _GLOBAL_SUM_R8( sumarea,mythid )

        shiftvel(2) = sumvol /sumarea
      endif  
cgg    End of the obcsnfirst loop. 

      if ( ( obcsnfirst) .or. (obcsnchanged)) then

cgg     Swap the value.
        shiftvel(1) = shiftvel(2)
 
        sumvol = 0. d0
        sumarea= 0. d0

#ifdef ALLOW_OBCSN_CONTROL
        jp1 = 0

        call active_read_xz(fnamefldn,tmpfldxz,
     &                         (obcsncount1-1)*nobcs+4, doglobalread,
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
                if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i)then
                    tmpx = tmpfldxz(i,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux - tmpx* delR(k)*dxg(i,j+jp1,bi,bj)
                    tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                  endif
                endif
              enddo
            enddo

            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSS_CONTROL
        jp1 = 1

        call active_read_xz(fnameflds,tmpfldxz,
     &                         (obcsncount1-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do i = imin,imax
                j = Ob_Js(I,bi,bj)
                if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i)then
                    tmpx = tmpfldxz(i,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux + tmpx* delR(k)*dxg(i,j+jp1,bi,bj)
                    tmparea = tmparea + delR(k) * dxg(i,j+jp1,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux          
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSW_CONTROL
        ip1 = 1

        call active_read_yz( fnamefldw, tmpfldyz,
     &      (obcsncount1-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do j = jmin,jmax
                i = ob_iw(j,bi,bj)
                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let corners contribute.
                  if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux + tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
                    tmparea = tmparea + delR(k)*dyg(i+ip1,j,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        =sumarea + tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

#ifdef ALLOW_OBCSE_CONTROL
        ip1 = 0

        call active_read_yz( fnameflde, tmpfldyz,
            (obcsncount1-1)*nobcs+3, doglobalread,
                      ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

c--     Loop over this thread's tiles.
        do bj = jtlo,jthi
          do bi = itlo,ithi

c--         Determine the weights to be used.
            tmpflux = 0. d 0
            tmparea = 0. d 0

            do k = 1, Nr
              do j = jmin,jmax
                i = ob_ie(j,bi,bj)
                if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
                  if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
                    tmpy = tmpfldyz(j,k,bi,bj)
cgg -- Positive is flux in.
                    tmpflux = tmpflux - tmpy* delR(k)*dyg(i+ip1,j,bi,bj)
                    tmparea = tmparea + delR(k) *dyg(i+ip1,j,bi,bj)
                  endif
                endif
              enddo
            enddo
            sumarea        = sumarea+ tmparea
            sumvol         = sumvol + tmpflux
          enddo
        enddo
#endif

c--   Do the global summation.
        _GLOBAL_SUM_R8( sumvol, mythid )
        _GLOBAL_SUM_R8( sumarea,mythid )

        shiftvel(2) = sumvol /sumarea
      endif  
cgg    End of the obcsnfirst, obcsnchanged loop. 

#endif /* BALANCE_CONTROL_VOLFLUX_GLOBAL */

      return
      end







