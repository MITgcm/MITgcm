C $Header: /u/gcmpack/MITgcm/pkg/ctrl/Attic/ctrl_obcsvol.F,v 1.14 2011/03/10 09:14:20 mlosch Exp $
C $Name:  $

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
      integer ilfld

      _RL raxr, rayr
      _RL sumvol
      _RL sumarea
      _RL tmpflux
      _RL tmparea
      character*(80) fnamefldn
      character*(80) fnameflds
      character*(80) fnamefldw
      character*(80) fnameflde

      logical doglobalread
      logical ladinit
      logical obcsnfirst, obcsnchanged
      integer obcsncount0, obcsncount1
      _RL obcsnfac

#if (defined ALLOW_OBCSN_CONTROL || defined ALLOW_OBCSS_CONTROL)
      _RL tmpfldxz (1-olx:snx+olx,nr,nsx,nsy)
#endif 
#if (defined ALLOW_OBCSE_CONTROL || defined ALLOW_OBCSW_CONTROL)
      _RL tmpfldyz (1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ECCO_VERBOSE
      integer nrec
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
#endif /* ECCO_VERBOSE */

CML   I think that this call of ctrl_getrec was forgotten. I also think
CML   that this code has not been use in ages, so we do not know what it
CML   actually does, and if it does it correcly.
CMLc--   Get the counters, flags, and the interpolation factor.
CML      call ctrl_GetRec( 'xx_obcsn',
CML     O                   obcsnfac, obcsnfirst, obcsnchanged,
CML     O                   obcsncount0,obcsncount1,
CML     I                   mytime, myiter, mythid )
c--   Get the counters, flags, and the interpolation factor.
      call ctrl_get_gen_rec(
     I                   xx_obcsnstartdate, xx_obcsnperiod,
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

        call active_read_xz(fnamefldn,tmpfldxz,
     &                         (obcsncount0-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcsn_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi

          tmpflux = 0. d0
          tmparea = 0. d0

          do k = 1, Nr
           do i = imin,imax
            j = Ob_Jn(I,bi,bj)
            if (maskS(i,j,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
             if(ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i) then
cgg -- Positive is flux in.
              raxr = delR(k)*dxg(i,j,bi,bj)
              tmpflux = tmpflux - tmpfldxz(i,k,bi,bj) * raxr
              tmparea = tmparea + raxr
             endif
            endif
           enddo
          enddo

          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL

        call active_read_xz(fnameflds,tmpfldxz,
     &                         (obcsncount0-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi
          
          tmpflux = 0. d 0
          tmparea = 0. d 0
          
          do k = 1, Nr
           do i = imin,imax
            j = Ob_Js(I,bi,bj)
            if (maskS(i,j+1,k,bi,bj) .ne. 0.) then
cgg -- Do not let the corners contribute to the volume flux.
             if (ob_iw(j,bi,bj).ne.i .and.ob_ie(j,bi,bj).ne.i) then
cgg -- Positive is flux in.
              raxr = delR(k)*dxg(i,j+1,bi,bj)
              tmpflux = tmpflux + tmpfldxz(i,k,bi,bj) * raxr
              tmparea = tmparea + raxr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL

        call active_read_yz( fnamefldw, tmpfldyz,
     &      (obcsncount0-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

c--   Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi

c--   Determine the weights to be used.
          tmpflux = 0. d 0
          tmparea = 0. d 0
          
          do k = 1, Nr
           do j = jmin,jmax
            i = ob_iw(j,bi,bj)
            if (maskW(i+1,j,k,bi,bj) .ne. 0.) then
cgg   -- Do not let the corners contribute to the volume flux.
             if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
cgg   -- Positive is flux in.
              rayr = delR(k)*dyg(i+1,j,bi,bj)
              tmpflux = tmpflux + tmpfldyz(j,k,bi,bj) * rayr
              tmparea = tmparea + rayr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea + tmparea
          sumvol  =  sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL

        call active_read_yz( fnameflde, tmpfldyz,
     &      (obcsncount0-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

c--   Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi
          
c--   Determine the weights to be used.
          tmpflux = 0. d 0
          tmparea = 0. d 0
          
          do k = 1, Nr
           do j = jmin,jmax
            i = ob_ie(j,bi,bj)
            if (maskW(i,j,k,bi,bj) .ne. 0.) then
cgg   -- Do not let the corners contribute to the volume flux.
             if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
cgg   -- Positive is flux in.
              rayr = delR(k)*dyg(i,j,bi,bj)
              tmpflux = tmpflux - tmpfldyz(j,k,bi,bj) * rayr
              tmparea = tmparea + rayr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSE_CONTROL */

c--   Do the global summation.
        _GLOBAL_SUM_RL( sumvol, mythid )
        _GLOBAL_SUM_RL( sumarea,mythid )

        shiftvel(2) = sumvol /sumarea
      endif
cgg    End of the obcsnfirst loop.

      if ( ( obcsnfirst) .or. (obcsnchanged)) then

cgg     Swap the value.
        shiftvel(1) = shiftvel(2)

        sumvol = 0. d0
        sumarea= 0. d0

#ifdef ALLOW_OBCSN_CONTROL

        call active_read_xz(fnamefldn,tmpfldxz,
     &                         (obcsncount1-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcsn_dummy )
c--     Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi

          tmpflux = 0. d0
          tmparea = 0. d0

          do k = 1, Nr
           do i = imin,imax
            j = Ob_Jn(I,bi,bj)
            if (maskS(i,j,k,bi,bj) .ne. 0.) then
cgg   -- Do not let the corners contribute to the volume flux.
             if (ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i)then
cgg   -- Positive is flux in.
              raxr = delR(k)*dxg(i,j,bi,bj)
              tmpflux = tmpflux - tmpfldxz(i,k,bi,bj) * raxr
              tmparea = tmparea + raxr
             endif
            endif
           enddo
          enddo

          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSN_CONTROL */

#ifdef ALLOW_OBCSS_CONTROL

        call active_read_xz(fnameflds,tmpfldxz,
     &                         (obcsncount1-1)*nobcs+4, doglobalread,
     &                         ladinit, optimcycle, mythid
     &                       , xx_obcss_dummy )

c--     Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi

          tmpflux = 0. d 0
          tmparea = 0. d 0

          do k = 1, Nr
           do i = imin,imax
            j = Ob_Js(I,bi,bj)
            if (maskS(i,j+1,k,bi,bj) .ne. 0.) then
cgg   -- Do not let the corners contribute to the volume flux.
             if (ob_iw(j,bi,bj).ne.i .and. ob_ie(j,bi,bj).ne.i)then
cgg   -- Positive is flux in.
              raxr = delR(k)*dxg(i,j+1,bi,bj)
              tmpflux = tmpflux + tmpfldxz(i,k,bi,bj) * raxr
              tmparea = tmparea + raxr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSS_CONTROL */

#ifdef ALLOW_OBCSW_CONTROL

        call active_read_yz( fnamefldw, tmpfldyz,
     &      (obcsncount1-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcsw_dummy )

c--   Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi
          
c--   Determine the weights to be used.
          tmpflux = 0. d 0
          tmparea = 0. d 0
          
          do k = 1, Nr
           do j = jmin,jmax
            i = ob_iw(j,bi,bj)
            if (maskW(i+1,j,k,bi,bj) .ne. 0.) then
cgg   -- Do not let corners contribute.
             if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
cgg   -- Positive is flux in.
              rayr = delR(k)*dyg(i+1,j,bi,bj)
              tmpflux = tmpflux + tmpfldyz(j,k,bi,bj) * rayr
              tmparea = tmparea + rayr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea + tmparea
          sumvol  =  sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSW_CONTROL */

#ifdef ALLOW_OBCSE_CONTROL

        call active_read_yz( fnameflde, tmpfldyz,
     &      (obcsncount1-1)*nobcs+3, doglobalread,
     &                ladinit, optimcycle, mythid
     &                       , xx_obcse_dummy )

c--   Loop over this thread tiles.
        do bj = jtlo,jthi
         do bi = itlo,ithi

c--   Determine the weights to be used.
          tmpflux = 0. d 0
          tmparea = 0. d 0
          
          do k = 1, Nr
           do j = jmin,jmax
            i = ob_ie(j,bi,bj)
            if (maskW(i,j,k,bi,bj) .ne. 0.) then
cgg   -- Do not let the corners contribute to the volume flux.
             if (ob_jn(i,bi,bj).ne.j .and. ob_js(i,bi,bj).ne.j)then
cgg   -- Positive is flux in.
              rayr = delR(k)*dyg(i,j,bi,bj)
              tmpflux = tmpflux - tmpfldyz(j,k,bi,bj) * rayr 
              tmparea = tmparea + rayr
             endif
            endif
           enddo
          enddo
          sumarea = sumarea+ tmparea
          sumvol  = sumvol + tmpflux
         enddo
        enddo
#endif /* ALLOW_OBCSE_CONTROL */

c--   Do the global summation.
        _GLOBAL_SUM_RL( sumvol, mythid )
        _GLOBAL_SUM_RL( sumarea,mythid )

        shiftvel(2) = sumvol /sumarea
      endif
cgg    End of the obcsnfirst, obcsnchanged loop.

#endif /* BALANCE_CONTROL_VOLFLUX_GLOBAL */

      return
      end







