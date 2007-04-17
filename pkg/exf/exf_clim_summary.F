C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_summary.F,v 1.10 2007/04/17 23:51:22 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_clim_summary(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_clim_summary
c     ==================================================================
c
c     o List all the settings of the external forcing.
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_clim_summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "EXF_PARAM.h"
#include "EXF_CLIM_PARAM.h"

c     == routine arguments ==

c     mythid  - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

#if defined(ALLOW_CLIMTEMP_RELAXATION) || \
    defined(ALLOW_CLIMSALT_RELAXATION) || \
    defined(ALLOW_CLIMSST_RELAXATION ) || \
    defined(ALLOW_CLIMSSS_RELAXATION )
      integer il
#endif

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
     &'// External forcing climatology configuration >>> START <<<'
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

#ifdef ALLOW_CLIMTEMP_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMTEMP_RELAXATION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMTEMP_RELAXATION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSALT_RELAXATION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSALT_RELAXATION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSST_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:           defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_CLIMSSS_RELAXATION:       NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

c     The climatological data sets are assumed to contain monthly
c     data. This can be changed in a later version to an arbitrary
c     number of intervals during a given year.

#ifdef ALLOW_CLIMTEMP_RELAXATION
c     Relaxation to pot. temperature climatology.
      il = ilnblnk(climtempfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,f12.0)')
     &'   Climatological pot. temperature starts at         ',
     &    climtempstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological pot. temperature period is         ',
     &    climtempperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a)')
     &'   Climatological pot. temperature is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climtempfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
c     Relaxation to salinity climatology.
      il = ilnblnk(climsaltfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological salinity starts at         ',
     &    climsaltstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological salinity period is         ',
     &    climsaltperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Climatological salinity is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsaltfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
c     Relaxation to SST climatology.
      il = ilnblnk(climsstfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST starts at         ',
     &    climsststartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SST period is         ',
     &    climsstperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Climatological SST is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsstfile(1:il),'  <<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
c     Relaxation to SSS climatology.
      il = ilnblnk(climsssfile)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS starts at         ',
     &    climsssstartdate
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,f12.0)')
     &'   Climatological SSS period is         ',
     &    climsssperiod
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'   Climatological SSS is read from file:'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a,a)')
     &'   >>  ',climsssfile(1:il),'  <<'
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
     &'// External forcing climatology configuration  >>> END <<<'
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
