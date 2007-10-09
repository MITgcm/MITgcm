C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_summary.F,v 1.4 2007/10/09 00:00:01 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"

      subroutine ctrl_Summary( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================
c
c     o Summarize the control vector part of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 06-Mar-2000
c
c     changed: Christian Eckert eckert@mit.edu
c
c     ==================================================================
c     SUBROUTINE ctrl_Summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#include "ctrl.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,k
      integer il
      integer timeint(4)
      integer nwetcenter
      integer nwetsouth
      integer nwetwest

      character*(max_len_mbuf) msgbuf

c     == external ==

      integer  ilnblnk
      external ilnblnk

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
     &'// ECCO control vector configuration  >>> START <<<'
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
      write(msgbuf,'(a)')
     &' '
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

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO control vector configuration  >>> END <<<'
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

