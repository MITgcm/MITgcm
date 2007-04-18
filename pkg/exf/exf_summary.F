C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_summary.F,v 1.10 2007/04/18 05:02:11 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_Summary(
     I                        mythid
     &                      )

c     ==================================================================
c     SUBROUTINE exf_Summary
c     ==================================================================
c
c     o List all the settings of the external forcing.
c
c     started: Christian Eckert eckert@mit.edu 11-Jan-1999
c
c     changed: Christian Eckert eckert@mit.edu 12-Feb-2000
c              - changed routine names (package prefix: exf_)
c
c     changed: Patrick Heimbach heimbach@mit.edu 04-May-2000
c              - changed the handling of precip and sflux with respect
c                to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
c
c     changed: Dimitris Menemenlis menemenlis@jpl.nasa.gov 20-Dec-2002
c              - modifications for using pkg/exf with pkg/seaice
c
c     ==================================================================
c     SUBROUTINE exf_Summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "EXF_PARAM.h"

c     == routine arguments ==

c     mythid  - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

      integer il

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
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   Print settings of some CPP flags.

#ifdef ALLOW_ATM_TEMP
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                     defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_TEMP:                 NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATM_WIND
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                     defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATM_WIND:                 NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_DOWNWARD_RADIATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_BULKFORMULAE
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_BULKFORMULAE:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

c--   For each data set used the summary prints the calendar data
c     and the corresponding file from which the data will be read.

#ifndef ALLOW_ATM_WIND
c--   Zonal wind stress.
      il = ilnblnk(ustressfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing starts at         ',
     &    ustressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind stress forcing period is         ',
     &    ustressperiod
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

c--   Meridional wind stress.
      il = ilnblnk(vstressfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing starts at    ',
     &    vstressstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind stress forcing period is    ',
     &    vstressperiod
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

#ifndef ALLOW_ATM_TEMP
c--   Heat flux.
      il = ilnblnk(hfluxfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing starts at                ',
     &    hfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Heat flux forcing period is                 ',
     &    hfluxperiod
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

c--   Salt flux.
      il = ilnblnk(sfluxfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing starts at                 ',
     &    sfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Salt flux forcing period is                 ',
     &    sfluxperiod
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
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
c--   Net shortwave.
      il = ilnblnk(swfluxfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing starts at      ',
     &    swfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net shortwave flux forcing period is      ',
     &    swfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Net shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATM_WIND
c--   Zonal wind.
      il = ilnblnk(uwindfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing starts at                ',
     &    uwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Zonal wind forcing period is                ',
     &    uwindperiod
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

c--   Meridional wind.
      il = ilnblnk(vwindfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing starts at           ',
     &    vwindstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Meridional wind forcing period is           ',
     &    vwindperiod
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
#endif

#ifdef ALLOW_ATM_TEMP
c--   Atmospheric temperature.
      il = ilnblnk(atempfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature starts at           ',
     &    atempstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric temperature period is           ',
     &    atempperiod
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

c--   Atmospheric specific humidity.
      il = ilnblnk(aqhfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity starts at     ',
     &    aqhstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric specific humidity period is     ',
     &    aqhperiod
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

c--   Net longwave.
      il = ilnblnk(lwfluxfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing starts at       ',
     &    lwfluxstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Net longwave flux forcing period is       ',
     &    lwfluxperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Net longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwfluxfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   Precipitation.
      il = ilnblnk(precipfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data set starts at            ',
     &    precipstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Precipitation data period is                ',
     &    precipperiod
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
#endif

#ifdef EXF_READ_EVAP
c--   Evaporation.
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(evapfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation starts at     ',
     &    evapstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Evaporation period is     ',
     &    evapperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Evaporation is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',evapfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// EXF_READ_EVAP:                  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_RUNOFF
c--   Runoff.
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      il = ilnblnk(runofffile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Runnoff starts at     ',
     &    runoffstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Runoff period is     ',
     &    runoffperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Runoff is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',runofffile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else /* ALLOW_RUNOFF */
      write(msgbuf,'(a)')
     &'// ALLOW_RUNOFF:                   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif /* ALLOW_RUNOFF */

#ifdef DOWNWARD_RADIATION
c--   Downward shortwave.
      il = ilnblnk(swdownfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing starts at      ',
     &    swdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward shortwave flux forcing period is      ',
     &    swdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Downward shortwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',swdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   Downward longwave.
      il = ilnblnk(lwdownfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing starts at       ',
     &    lwdownstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Downward longwave flux forcing period is       ',
     &    lwdownperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Downward longwave flux forcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',lwdownfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ATMOSPHERIC_LOADING
c--   Atmospheric pressure.
      il = ilnblnk(apressurefile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing starts at      ',
     &    apressurestartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Atmospheric pressure forcing period is      ',
     &    apressureperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Atmospheric pressureforcing is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',apressurefile(1:il),'  <<'
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
