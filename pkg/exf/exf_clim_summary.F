c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_summary.F,v 1.1 2001/05/14 22:08:40 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


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
c
c     ==================================================================
c     SUBROUTINE exf_clim_summary
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "cal.h"
#include "exf.h"

#include "exf_clim_param.h"

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
      call cal_TimeInterval( climtempperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Climatological pot. temperature starts at         ',
     &    (climtempstartdate(i), i=1,2),
     & dayofweek(climtempstartstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Climatological pot. temperature period is         ',
     &    (timeint(i), i=1,2)
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
      call cal_TimeInterval( climsaltperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Climatological salinity starts at         ',
     &    (climsaltstartdate(i), i=1,2),
     & dayofweek(climsaltstartstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Climatological salinity period is         ',
     &    (timeint(i), i=1,2)
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
      call cal_TimeInterval( climsstperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Climatological SST starts at         ',
     &    (climsststartdate(i), i=1,2),
     & dayofweek(climsststartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Climatological SST period is         ',
     &    (timeint(i), i=1,2)
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
      call cal_TimeInterval( climsssperiod, 'secs', timeint, mythid )

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6,1x,a,a)')
     &'   Climatological SSS starts at         ',
     &    (climsssstartdate(i), i=1,2),
     & dayofweek(climsssstartdate(4)),'.'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9.8,i7.6)')
     &'   Climatological SSS period is         ',
     &    (timeint(i), i=1,2)
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
