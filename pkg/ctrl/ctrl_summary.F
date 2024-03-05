#include "CTRL_OPTIONS.h"

      subroutine ctrl_Summary( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================
c
c     o Summarize the control vector settings.
c
c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_PARAMS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# if (   defined ALLOW_GENARR2D_CONTROL \
      || defined ALLOW_GENARR3D_CONTROL \
      || defined ALLOW_GENTIM2D_CONTROL  )
#  include "CTRL_GENARR.h"
# endif
#endif

c     == routine arguments ==
      integer myThid

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer k
      integer ivar
      integer il
      integer errCount
      integer nwetcenter
      integer nwetsouth
      integer nwetwest

      character*(max_len_mbuf) msgbuf

#if (defined (ALLOW_GENARR2D_CONTROL) \
      || defined (ALLOW_GENARR3D_CONTROL) \
      || defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr, jarr, iw
#endif
#if ( defined ALLOW_GENTIM2D_CONTROL && defined ALLOW_CAL )
      integer i, timeint(4)
#endif

c     == end of interface ==

      errCount = 0

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// control vector configuration  >>> START <<<'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      write(msgbuf,'(a)')
     &' Total number of ocean points per tile:'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &' --------------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a,i8)') ' sNx*sNy*Nr = ',sNx*sNy*Nr
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &' Number of ocean points per tile:'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &' --------------------------------'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      do bj = 1,nSy
        do bi = 1,nSx
          nwetcenter = 0
          nwetsouth  = 0
          nwetwest   = 0
          do k = 1,Nr
            nwetcenter = nwetcenter + nwetctile(bi,bj,k)
            nwetsouth  = nwetsouth  + nwetstile(bi,bj,k)
            nwetwest   = nwetwest   + nwetwtile(bi,bj,k)
          enddo
c         write(msgbuf,'(a,i5.4,i5.4,i7.6,i7.6,i7.6)')
          write(msgbuf,'(a,i4.3,i4.3,i8,i8,i8)')
     &    ' bi,bj,#(c/s/w):',bi,bj,nwetcenter, nwetsouth, nwetwest
          call print_message( msgbuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        enddo
      enddo

      do ivar = 1, maxcvars
C     only print active variables for which ncvarindex has be set
C     (default = -1)
       if ( ncvarindex(ivar) .GE. 0 ) then

        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        il   = ILNBLNK( ncvarfname(ivar) )
C     find out if current variable is a generic control variable name
#ifdef ALLOW_GENARR3D_CONTROL
        if ( ncvartype(ivar) .eq. 'Arr3D') then
         iarr = ncvarindex(ivar)
         if ( xx_genarr3d_file(iarr) .eq. ncvarfname(ivar) ) then
          write(msgbuf,'(a,i3,a)')
     &    ' -> 3d control, genarr3d no.',iarr,' is in use'
          call print_message( msgbuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         else
          write(msgbuf,'(a,i6)')
     &    ' -> something is wrong with 3d control, genarr3d no.',iarr
          call print_error( msgbuf, myThid )
          errCount = errCount + 1
         endif
        endif
#endif
#ifdef ALLOW_GENARR2D_CONTROL
        if ( ncvartype(ivar) .eq. 'Arr2D') then
         iarr = ncvarindex(ivar)
         if ( xx_genarr2d_file(iarr) .eq. ncvarfname(ivar) ) then
          write(msgbuf,'(a,i3,a)')
     &    ' -> 2d control, genarr2d no.',iarr,' is in use'
          call print_message( msgbuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         else
          write(msgbuf,'(a,i6)')
     &    ' -> something is wrong with 2d control, genarr2d no.',iarr
          call print_error( msgbuf, myThid )
          errCount = errCount + 1
         endif
        endif
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
        if ( ncvartype(ivar) .eq. 'Tim2D') then
         iarr = ncvarindex(ivar)
         if ( xx_gentim2d_file(iarr) .eq. ncvarfname(ivar) ) then
          write(msgbuf,'(2a,i3,a)') ' -> ',
     &    'time variable 2d control, gentim2d no.',iarr,' is in use'
          call print_message( msgbuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         else
          write(msgbuf,'(2a,i6)') ' -> something is wrong with ',
     &    'time variable  2d control, gentim2d no.',iarr
          call print_error( msgbuf, myThid )
          errCount = errCount + 1
         endif
        endif
#endif
        write(msgbuf,'(a,a)')
     &'      file       = ',ncvarfname(ivar)(1:il)
        call print_message( msgbuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        write(msgbuf,'(a,a)')
     &'      ncvartype  = ', ncvartype(ivar)
        call print_message( msgbuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        write(msgbuf,'(a,i5,a)')
     &'      index      = ', ivar, '  (use this for pkg/grdchk)'
        call print_message( msgbuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        write(msgbuf,'(a,i5)')
     &'      ncvarindex = ', ncvarindex(ivar)
        call print_message( msgbuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C     extra information only available for generic control variables
#ifdef ALLOW_GENARR3D_CONTROL
        if ( ncvartype(ivar) .eq. 'Arr3D') then
         iarr = ncvarindex(ivar)
         iw   = ILNBLNK( xx_genarr3d_weight(iarr) )
         write(msgbuf,'(a,a)')
     &'      weight     = ',xx_genarr3d_weight(iarr)(1:iw)
         call print_message( msgbuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         do jarr=1,maxCtrlProc
          if (xx_genarr3d_preproc(jarr,iarr).NE.' ') then
           il = ilnblnk(xx_genarr3d_preproc(jarr,iarr))
           write(msgbuf,'(a,a)') ' preprocess = ',
     &          xx_genarr3d_preproc(jarr,iarr)(1:il)
           call print_message( msgbuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          endif
         enddo
        endif
#endif
#ifdef ALLOW_GENARR2D_CONTROL
        if ( ncvartype(ivar) .eq. 'Arr2D' ) then
         iarr = ncvarindex(ivar)
         iw   = ILNBLNK( xx_genarr2d_weight(iarr) )
         write(msgbuf,'(a,a)')
     &'      weight     = ',xx_genarr2d_weight(iarr)(1:iw)
         call print_message( msgbuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         do jarr=1,maxCtrlProc
          if (xx_genarr2d_preproc(jarr,iarr).NE.' ') then
           il = ilnblnk(xx_genarr2d_preproc(jarr,iarr))
           write(msgbuf,'(a,a)') ' preprocess = ',
     &          xx_genarr2d_preproc(jarr,iarr)(1:il)
           call print_message( msgbuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
          endif
         enddo
        endif
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
        if ( ncvartype(ivar) .eq. 'Tim2D' ) then
         iarr = ncvarindex(ivar)
         iw   = ILNBLNK( xx_gentim2d_weight(iarr) )
         write(msgbuf,'(a,a)')
     &'      weight     = ',xx_gentim2d_weight(iarr)(1:iw)
         call print_message( msgbuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_CAL
         if ( useCAL ) then
          call cal_TimeInterval( xx_gentim2d_period(iarr),
     &                          'secs', timeint, myThid )
          write(msgbuf,'(a,i9.8,i7.6)')
     &'      period     = ',(timeint(i), i=1,2)
          call print_message( msgbuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         endif
#endif

         do jarr=1,maxCtrlProc
          if (xx_gentim2d_preproc(jarr,iarr).NE.' ') then
           il = ilnblnk(xx_gentim2d_preproc(jarr,iarr))
           write(msgbuf,'(a,a)') '      preprocess = ',
     &          xx_gentim2d_preproc(jarr,iarr)(1:il)
           call print_message( msgbuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
C
           if (xx_gentim2d_preproc_c(jarr,iarr).NE.' ') then
            il = ilnblnk(xx_gentim2d_preproc_c(jarr,iarr))
            write(msgbuf,'(a,a)') '        param. (text)= ',
     &           xx_gentim2d_preproc_c(jarr,iarr)(1:il)
            call print_message( msgbuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
           endif
C
           if (xx_gentim2d_preproc_i(jarr,iarr).NE.0) then
            write(msgbuf,'(a,i6)') '        param. (int.)= ',
     &           xx_gentim2d_preproc_i(jarr,iarr)
            call print_message( msgbuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
           endif
C
           if (xx_gentim2d_preproc_r(jarr,iarr).NE.0. _d 0) then
            write(msgbuf,'(a,e10.3)') '        param. (real)= ',
     &           xx_gentim2d_preproc_r(jarr,iarr)
            call print_message( msgbuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
           endif

          endif
         enddo
        endif
#endif
       endif
      enddo

      if ( errCount.GE.1 ) then
        write(msgbuf,'(a,i3,a)')
     &       'ctrl_summary: detected', errCount,' fatal error(s)'
        call print_error( msgbuf, myThid )
        stop 'ABNORMAL END: S/R CTRL_SUMMARY'
      endif

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// control vector configuration  >>> END <<<'
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      return
      end
