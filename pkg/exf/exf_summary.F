c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_summary.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_Summary(
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE exf_Summary
c     ==================================================================
c
c     o List all the settings of the external forcing.
c
c     started: Christian Eckert eckert@mit.edu  11-Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu  12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c              Patrick Heimbach, heimbach@mit.edu  04-May-2000
c
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c
c     ==================================================================
c     SUBROUTINE exf_Summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"
#include "exf_param.h"

c     == routine arguments ==

c     mythid  - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

      integer i
      integer il
      integer timeint(4)

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
     &'// External forcing configuration >>> START <<<'
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
      write(msgbuf,'(a,a)')
     &'External forcing version: ',externalforcingversion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'Uses Calendar version:    ',usescalendarversion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     For each data set used the summary prints the calendar data
c     and the corresponding file from which the data will be read.

#ifdef ALLOW_BULKFORMULAE
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#ifdef ALLOW_ATM_TEMP
      write(msgbuf,'(a)')
     &'// '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_ATM_WIND
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#else
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_KPP
      write(msgbuf,'(a)')
     &'// ALLOW_KPP:                          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_KPP:                      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_BULKFORMULAE

#ifdef ALLOW_ATM_TEMP
c     Atmospheric temperature.
      il = ilnblnk(atempfile)
      call cal_TimeInterval( atempperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Atmospheric temperature starts at           ',
     &    (atempstartdate(i), i=1,2), dayofweek(atempstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Atmospheric temperature period is           ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric temperature is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',atempfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Atmospheric specific humidity.
      il = ilnblnk(aqhfile)
      call cal_TimeInterval( aqhperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Atmospheric specific humidity starts at     ',
     &    (aqhstartdate(i), i=1,2), dayofweek(aqhstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Atmospheric specific humidity period is     ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric specific humidity is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',aqhfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Short wave radiative flux.
      il = ilnblnk(swfluxfile)
      call cal_TimeInterval( swfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Short wave rad. flux forcing starts at      ',
     &    (swfluxstartdate(i), i=1,2), dayofweek(swfluxstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Short wave rad. flux forcing period is      ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Short wave rad. flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Long wave radiative flux.
      il = ilnblnk(lwfluxfile)
      call cal_TimeInterval( lwfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Long wave rad. flux forcing starts at       ',
     &    (lwfluxstartdate(i), i=1,2), dayofweek(lwfluxstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Long wave rad. flux forcing period is       ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Long wave rad. flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Precipitation.
      il = ilnblnk(precipfile)
      call cal_TimeInterval( precipperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Precipitation data set starts at            ',
     &    (precipstartdate(i), i=1,2), dayofweek(precipstartdate(4)),
     &'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Precipitation data period is                ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Precipitation data is read from file:       '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',precipfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#else
c     Heat flux.
      il = ilnblnk(hfluxfile)
      call cal_TimeInterval( hfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Heat flux forcing starts at                ',
     &    (hfluxstartdate(i), i=1,2), dayofweek(hfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Heat flux forcing period is                 ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Heat flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',hfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Salt flux.
      il = ilnblnk(sfluxfile)
      call cal_TimeInterval( sfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Salt flux forcing starts at                 ',
     &    (sfluxstartdate(i), i=1,2), dayofweek(sfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Salt flux forcing period is                 ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Salt flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',sfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_KPP
c     Short wave radiative flux.
      il = ilnblnk(swfluxfile)
      call cal_TimeInterval( swfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Short wave rad. flux forcing starts at      ',
     &    (swfluxstartdate(i), i=1,2), dayofweek(swfluxstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Short wave rad. flux forcing period is      ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Short wave rad. flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#endif

#ifdef ALLOW_ATM_WIND
c     Zonal wind.
      il = ilnblnk(uwindfile)
      call cal_TimeInterval( uwindperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Zonal wind forcing starts at                ',
     &    (uwindstartdate(i), i=1,2), dayofweek(uwindstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Zonal wind forcing period is                ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',uwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Meridional wind.
      il = ilnblnk(vwindfile)
      call cal_TimeInterval( vwindperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Meridional wind forcing starts at           ',
     &    (vwindstartdate(i), i=1,2), dayofweek(vwindstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Meridional wind forcing period is           ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Meridional wind forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vwindfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
c     Zonal wind stress.
      il = ilnblnk(ustressfile)
      call cal_TimeInterval( ustressperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Zonal wind stress forcing starts at         ',
     &    (ustressstartdate(i), i=1,2), dayofweek(ustressstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Zonal wind stress forcing period is         ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',ustressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Meridional wind stress.
      il = ilnblnk(vstressfile)
      call cal_TimeInterval( vstressperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Meridional wind stress forcing starts at    ',
     &    (vstressstartdate(i), i=1,2), dayofweek(vstressstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Meridional wind stress forcing period is    ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Meridional wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vstressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#else
c     Heat flux.
      il = ilnblnk(hfluxfile)
      call cal_TimeInterval( hfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Heat flux forcing starts at                 ',
     &    (hfluxstartdate(i), i=1,2), dayofweek(hfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Heat flux forcing period is                 ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Heat flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',hfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Salt flux.
      il = ilnblnk(sfluxfile)
      call cal_TimeInterval( sfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Salt flux forcing starts at                 ',
     &    (sfluxstartdate(i), i=1,2), dayofweek(sfluxstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Salt flux forcing period is                 ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Salt flux forcing is read from file:        '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',sfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Zonal wind stress.
      il = ilnblnk(ustressfile)
      call cal_TimeInterval( ustressperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Zonal wind stress forcing starts at         ',
     &    (ustressstartdate(i), i=1,2), dayofweek(ustressstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Zonal wind stress forcing period is         ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Zonal wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',ustressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c     Meridional wind stress.
      il = ilnblnk(vstressfile)
      call cal_TimeInterval( vstressperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Meridional wind stress forcing starts at    ',
     &    (vstressstartdate(i), i=1,2), dayofweek(vstressstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Meridional wind stress forcing period is    ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Meridional wind stress forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',vstressfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_KPP
c     Short wave radiative flux.
      il = ilnblnk(swfluxfile)
      call cal_TimeInterval( swfluxperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Short wave rad. flux forcing starts at      ',
     &    (swfluxstartdate(i), i=1,2), dayofweek(swfluxstartdate(4)),
     &    '.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Short wave rad. flux forcing period is      ',
     &    (timeint(i), i=1,2)
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Short wave rad. flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#endif

#ifdef ALLOW_KPP
      write(msgbuf,'(a)')
     &'// ALLOW_KPP:                          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_KPP:                      NOT defined'
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
     &'// External forcing configuration  >>> END <<<'
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

      end
