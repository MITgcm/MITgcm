#include "CTRL_OPTIONS.h"

      subroutine ctrl_Summary( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================
c
c     o Summarize the control vector settings.
c
c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================

      implicit none

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
# include "ctrl.h"
# include "CTRL_SIZE.h"
# if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
#  include "CTRL_GENARR.h"
# endif
#endif

c     == routine arguments ==
      integer mythid

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer k
      integer il
      integer nwetcenter
      integer nwetsouth
      integer nwetwest

      character*(max_len_mbuf) msgbuf

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr, jarr
#endif
#if ( ( defined ALLOW_GENTIM2D_CONTROL && defined ALLOW_CAL ) || defined ECCO_CTRL_DEPRECATED )
      integer i, timeint(4)
#endif

c     == end of interface ==

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// control vector configuration  >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &' Total number of ocean points per tile:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' --------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i8)') ' snx*sny*nr = ',snx*sny*nr
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Number of ocean points per tile:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' --------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      do bj = 1,nsy
        do bi = 1,nsx
          nwetcenter = 0
          nwetsouth  = 0
          nwetwest   = 0
          do k = 1,nr
            nwetcenter = nwetcenter + nwetctile(bi,bj,k)
            nwetsouth  = nwetsouth  + nwetstile(bi,bj,k)
            nwetwest   = nwetwest   + nwetwtile(bi,bj,k)
          enddo
          write(msgbuf,'(a,i5.4,i5.4,i7.6,i7.6,i7.6)')
     &    ' bi,bj,#(c/s/w):',bi,bj,nwetcenter,
     &                             nwetsouth,
     &                             nwetwest
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
        enddo
      enddo

#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_THETA0_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Initial state temperature contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control variable index:   ',ncvarindex(1)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_SALT0_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Initial state salinity contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control variable index:   ',ncvarindex(2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_HFLUX_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Heat flux contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control variable index:   ',ncvarindex(3)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      il = ilnblnk(xx_hflux_file)
      call cal_TimeInterval( xx_hfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Heat flux contribution starts at:           ',
     &    (xx_hfluxstartdate(i), i=1,2),
     &     dayofweek(xx_hfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Heat flux contribution period is:           ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Heat flux contribution is read from file:   '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',xx_hflux_file(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_SFLUX_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Salt flux contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control varibale index:   ',ncvarindex(4)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      il = ilnblnk(xx_sflux_file)
      call cal_TimeInterval( xx_sfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Salt flux contribution starts at:           ',
     &    (xx_sfluxstartdate(i), i=1,2),
     &     dayofweek(xx_sfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Salt flux contribution period is:           ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Salt flux contribution is read from file:   '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',xx_sflux_file(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_USTRESS_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Zonal wind stress contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control variable index:   ',ncvarindex(5)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      il = ilnblnk(xx_tauu_file)
      call cal_TimeInterval( xx_tauuperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Zonal wind stress contribution starts at:   ',
     &    (xx_tauustartdate(i), i=1,2),
     &     dayofweek(xx_tauustartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Zonal wind stress contribution period is:   ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind stress contribution is read from file:   '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',xx_tauu_file(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_VSTRESS_CONTROL
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Meridional wind stress contribution:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5.4)')
     &' Control variable index:   ',ncvarindex(6)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      il = ilnblnk(xx_tauv_file)
      call cal_TimeInterval( xx_tauvperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Merid. wind stress contribution starts at:  ',
     &    (xx_hfluxstartdate(i), i=1,2),
     &     dayofweek(xx_hfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Merid. wind stress contribution period is:  ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Merid. wind stress contribution is read from file:   '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',xx_tauv_file(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_SMOOTH
#ifdef ECCO_CTRL_DEPRECATED

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &' Settings of diffusion based correlation operators:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' --------------------------------------------------'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,L5,a)')
     &' ctrlSmoothCorrel3D        = ',ctrlSmoothCorrel3D,
     &' /* use 3D controls correlation */'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,L5,a)')
     &' ctrlSmoothCorrel2D        = ',ctrlSmoothCorrel2D,
     &' /* use 2D controls correlation */'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_AUTODIFF
      if (ctrlSmoothCorrel2D.AND..NOT.useSmoothCorrel2DinAdMode) then
c allow for switching off correl2d in adjoint
      write(msgbuf,'(a,L5,a)')
     &' useSmoothCorrel2DinAdMode = ',useSmoothCorrel2DinAdMode,
     &' /* use in adjoint mode */'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      endif
#endif

#endif /* ECCO_CTRL_DEPRECATED */
#endif /* ALLOW_SMOOTH */

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Settings of generic controls:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' -----------------------------'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,L5,a)')
     &' ctrlUseGen  = ',ctrlUseGen,
     &' /* use generic controls */'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_GENARR2D_CONTROL
      do iarr = 1, maxCtrlArr2D
       if (xx_genarr2d_weight(iarr).NE.' ') then

        write(msgbuf,'(a,i2,a)')
     &' -> 2D control, genarr2d no. ',iarr,
     &' is in use'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        il   = ILNBLNK( xx_genarr2d_file(iarr) )
        write(msgbuf,'(a,a)')
     &'      file       = ',xx_genarr2d_file(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        il   = ILNBLNK( xx_genarr2d_weight(iarr) )
        write(msgbuf,'(a,a)')
     &'      weight     = ',xx_genarr2d_weight(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      index      = ', 100+iarr
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      ncvarindex = ', ncvarindex(100+iarr)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        do jarr=1,maxCtrlProc
         if (xx_genarr2d_preproc(jarr,iarr).NE.' ') then
          il = ilnblnk(xx_genarr2d_preproc(jarr,iarr))
          write(msgbuf,'(a,a)') ' preprocess = ',
     &                            xx_genarr2d_preproc(jarr,iarr)(1:il)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
         endif
        enddo

       endif
      enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
      do iarr = 1, maxCtrlArr3D
       if (xx_genarr3d_weight(iarr).NE.' ') then

        write(msgbuf,'(a,i2,a)')
     &' -> 3d control, genarr3d no. ',iarr,
     &' is in use'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        il   = ILNBLNK( xx_genarr3d_file(iarr) )
        write(msgbuf,'(a,a)')
     &'      file       = ',xx_genarr3d_file(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        il   = ILNBLNK( xx_genarr3d_weight(iarr) )
        write(msgbuf,'(a,a)')
     &'      weight     = ',xx_genarr3d_weight(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      index      = ', 200+iarr
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      ncvarindex = ', ncvarindex(200+iarr)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        do jarr=1,maxCtrlProc
         if (xx_genarr3d_preproc(jarr,iarr).NE.' ') then
          il = ilnblnk(xx_genarr3d_preproc(jarr,iarr))
          write(msgbuf,'(a,a)') ' preprocess = ',
     &         xx_genarr3d_preproc(jarr,iarr)(1:il)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
         endif
        enddo

       endif
      enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      do iarr = 1, maxCtrlTim2D
       if (xx_gentim2d_weight(iarr).NE.' ') then

        write(msgbuf,'(a,i2,a)')
     &' -> time variable 2D control, gentim2d no. ',iarr,
     &' is in use'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        il   = ILNBLNK( xx_gentim2d_file(iarr) )
        write(msgbuf,'(a,a)')
     &'      file       = ',xx_gentim2d_file(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

        il   = ILNBLNK( xx_gentim2d_weight(iarr) )
        write(msgbuf,'(a,a)')
     &'      weight     = ',xx_gentim2d_weight(iarr)(1:il)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      index      = ', 300+iarr
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i5.4)')
     &'      ncvarindex = ', ncvarindex(300+iarr)
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_CAL
        if ( useCAL ) then
         call cal_TimeInterval( xx_gentim2d_period(iarr),
     &                          'secs', timeint, mythid )
         write(msgbuf,'(a,i9.8,i7.6)')
     &        '      period     = ',(timeint(i), i=1,2)
         call print_message( msgbuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
        endif
#endif

        do jarr=1,maxCtrlProc
         if (xx_gentim2d_preproc(jarr,iarr).NE.' ') then
          il = ilnblnk(xx_gentim2d_preproc(jarr,iarr))
          write(msgbuf,'(a,a)') '      preprocess = ',
     &         xx_gentim2d_preproc(jarr,iarr)(1:il)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
C
          if (xx_gentim2d_preproc_c(jarr,iarr).NE.' ') then
           il = ilnblnk(xx_gentim2d_preproc_c(jarr,iarr))
           write(msgbuf,'(a,a)') '        param. (text)= ',
     &          xx_gentim2d_preproc_c(jarr,iarr)(1:il)
           call print_message( msgbuf, standardmessageunit,
     &                         SQUEEZE_RIGHT , mythid)
          endif
C
          if (xx_gentim2d_preproc_i(jarr,iarr).NE.0) then
           write(msgbuf,'(a,i6)') '        param. (int.)= ',
     &          xx_gentim2d_preproc_i(jarr,iarr)
           call print_message( msgbuf, standardmessageunit,
     &                         SQUEEZE_RIGHT , mythid)
          endif
C
          if (xx_gentim2d_preproc_r(jarr,iarr).NE.0. _d 0) then
           write(msgbuf,'(a,e10.3)') '        param. (real)= ',
     &          xx_gentim2d_preproc_r(jarr,iarr)
           call print_message( msgbuf, standardmessageunit,
     &                         SQUEEZE_RIGHT , mythid)
          endif

         endif
        enddo

       endif
      enddo
#endif

#endif

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// control vector configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      return
      end
