C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

#ifdef ALLOW_CAL_NENDITER
      subroutine cal_Set(
     I                    modstart,
     I                    modend,
     I                    modstep,
     I                    modcalendartype,
     I                    modstartdate_1,
     I                    modstartdate_2,
     I                    modenddate_1,
     I                    modenddate_2,
     I                    moditerini,
     I                    moditerend,
     I                    modintsteps,
     I                    mythid
     &                  )
#else
      subroutine cal_Set(
     I                    modstart,
     I                    modend,
     I                    modstep,
     I                    modcalendartype,
     I                    modstartdate_1,
     I                    modstartdate_2,
     I                    modenddate_1,
     I                    modenddate_2,
     I                    moditerini,
     I                    modintsteps,
     I                    mythid
     &                  )
#endif

c     ==================================================================
c     SUBROUTINE cal_Set
c     ==================================================================
c
c     o This routine initialises the calendar according to the user
c       specifications in "data".
c
c     Purpose: Precalculations for the calendar.
c
c              Given the type of calendar that should be used date
c              arrays and some additional information is returned.
c
c              Check for consistency with other specifications such
c              as modintsteps.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  19-Jan-2000
c
c              - Changed the role of the routine arguments. Chris Hill
c                proposed to make the calendar less "invasive". The tool
c                now assumes that the MITgcmUV already provides an ade-
c                quate set of time stepping parameters. The calendar
c                only associates a date with the given starttime of the
c                numerical model. startdate corresponds to zero start-
c                time. So, given niter0 or startdate .ne. zero the actual
c                startdate of the current integration is shifted by the
c                time interval correponding to niter0, startdate respec-
c                tively.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c              Christian Eckert eckert@mit.edu  23-Feb-2000
c
c              - Corrected the declaration of *modelrundate*
c                --> integer modelrundate(4)
c
c     ==================================================================
c     SUBROUTINE cal_Set
c     ==================================================================

      implicit none

c     == global variables ==

#include "cal.h"

c     == routine arguments ==

c     modcalendartype - the type of calendar that is to be used.
c                       Available: 'model'
c                                  'gregorian'
c     modstartdate_1  - startdate of the integration: yyyymmdd
c     modstartdate_2  - startdate of the integration: hhmmss
c     modenddate_1    - enddate   of the integration: yyyymmdd
c     modenddate_2    - enddate   of the integration: hhmmss
c     moditerini      - initial iteration number of the model
c     moditerend      - last iteration number of the model
c     modstep         - timestep of the numerical model
c     modintsteps     - number of timesteps that are to be performed.
c     mythid          - number of this instance of the subrotuine.

      _RL     modstart
      _RL     modend
      _RL     modstep
      character*(*) modcalendartype
      integer modstartdate_1
      integer modstartdate_2
      integer modenddate_1
      integer modenddate_2
      integer moditerini
#ifdef ALLOW_CAL_NENDITER
      integer moditerend
#endif
      integer modintsteps
      integer mythid

c     == local variables ==

      integer i,j,k
      integer ierr
      integer datediff(4)
      integer timediff(4)
      integer iterinitime(4)
      integer modelrundate(4)
      _RL     runtimesecs
      _RL     iterinisecs

c     == external ==

      integer  cal_IntYears
      external cal_IntYears

      integer  cal_IntMonths
      external cal_IntMonths

      integer  cal_IntDays
      external cal_IntDays

      integer  cal_nStepDay
      external cal_nStepDay

c     == end of interface ==

c     Initialise some variables.
      usingNoCalendar        = .false.
      usingGregorianCalendar = .false.
      usingModelCalendar     = .false.
      usingJulianCalendar    = .false.

c     Map the numerical model's parameters. --> common blocks in
c                                               CALENDAR.h
      modelstart       = modstart
      modelend         = modend
      modelstep        = modstep
      modeliter0       = moditerini
      modelintsteps    = modintsteps

#ifdef ALLOW_CAL_NENDITER
      modeliterend     = moditerend
#else
      modeliterend     = 0
#endif

c     Do first consistency checks (most are taken from the MITgcmUV).
c     o Time step.
      if ( modelstep .le. 0. ) then
        ierr = 102
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      endif
      if ( modelstep .lt. 1. ) then
        ierr = 103
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      endif
      if ( abs(modelstep - nint(modelstep)) .gt. 0.000001 ) then
        ierr = 104
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      else
        modelstep = float(nint(modelstep))
      endif

c     o Start time
      if ( modeliter0 .ne. 0 .and. modelstart .eq. 0. ) then
         modelstart = modelstep*float(modeliter0)
      endif
c     o modeliter0
      if ( modeliter0 .eq. 0 .and. modelstart .ne. 0. ) then
         modeliter0 = int( modelstart/modelstep )
      endif

c     o modelintsteps
      if ( modelintsteps .eq. 0 .and. modeliterend .ne. 0 )
     &     modelintsteps = modeliterend - modeliter0
      if ( modelintsteps .eq. 0 .and. modelend .ne. 0. )
     &     modelintsteps = int(0.5 + (modelend - modelstart)/modelstep)

c     o modeliterend
      if ( modeliterend .eq. 0 .and. modelintsteps .ne. 0 )
     &     modeliterend = modeliter0 + modelintsteps
      if ( modeliterend .eq. 0 .and. modelend .ne. 0. )
     &     modeliterend = int(0.5 + modelend/modelstep)

c     o modelend
      if ( modelend .eq. 0. .and. modelintsteps .ne. 0 )
     &     modelend = modelstart + modelstep*float(modelintsteps)
      if ( modelend .eq. 0. .and. modeliterend .ne. 0 )
     &     modelend = modelstep*float(modeliterend)

c     Start setting the calendar's parameters.

c     The calendar type.
      if ( modcalendartype .eq. 'none') then
        usingNoCalendar = .true.
      endif
      if ( modcalendartype .eq. 'gregorian') then
        usingGregorianCalendar = .true.
      endif
      if ( modcalendartype .eq. 'model') then
        usingModelCalendar = .true.
      endif
      if ( modcalendartype .eq. 'julian') then
        usingJulianCalendar = .true.
      endif

      if ( usingGregorianCalendar ) then
c       The reference date for the Gregorian Calendar.
c       and its format: ( yymmdd , hhmmss , leap year, weekday )
c                                             (1/2)    (1 - 7)
c       The Gregorian calendar starts on Friday, 15 Oct. 1582.
        refdate(1) = 15821015
        refdate(2) = 0
        refdate(3) = 1
        refdate(4) = 1

c       Number of months per year and other useful numbers.
        nmonthyear       = 12
        ndaysnoleap      = 365
        ndaysleap        = 366
        nmaxdaymonth     = 31
        hoursperday      = 24
        minutesperday    = 1440
        minutesperhour   = 60
        secondsperday    = 86400
        secondsperhour   = 3600
        secondsperminute = 60

c       Number of days per month.
c       The "magic" number 2773 derives from the sequence: 101010110101
c         read in reverse and interpreted as a dual number. An
c         alternative would be to take 2741 with the loop being
c         executed in reverse order. Accidentially, the latter
c         is a prime number.
        k=2773
        do i=1,nmonthyear
          j = mod(k,2)
          k = (k-j)/2
          ndaymonth(i,1) = 30+j
          ndaymonth(i,2) = 30+j
        enddo
        ndaymonth(2,1) = 28
        ndaymonth(2,2) = 29

c       Week days.
        dayofweek(1) = 'FRI'
        dayofweek(2) = 'SAT'
        dayofweek(3) = 'SUN'
        dayofweek(4) = 'MON'
        dayofweek(5) = 'TUE'
        dayofweek(6) = 'WED'
        dayofweek(7) = 'THU'

      else if ( usingModelCalendar ) then
c       Assume a model calendar having 12 months with thirty days each.
c       Reference date is the first day of year 0 at 0am, and model
c       day 1.
        refdate(1) = 1
        refdate(2) = 0
        refdate(3) = 1
        refdate(4) = 1

c       Some useful numbers.
        nmonthyear       = 12
        ndaysnoleap      = 360
        ndaysleap        = 360
        nmaxdaymonth     = 30
        hoursperday      = 24
        minutesperday    = 1440
        minutesperhour   = 60
        secondsperday    = 86400
        secondsperhour   = 3600
        secondsperminute = 60
        do i=1,nmonthyear
          ndaymonth(i,1) = 30
          ndaymonth(i,2) = 30
        enddo

c       Week days (Model Day 1 - 7).
        dayofweek(1) = 'MD1'
        dayofweek(2) = 'MD2'
        dayofweek(3) = 'MD3'
        dayofweek(4) = 'MD4'
        dayofweek(5) = 'MD5'
        dayofweek(6) = 'MD6'
        dayofweek(7) = 'MD7'

      else if ( usingJulianCalendar ) then

        ierr = 110
        call cal_PrintError( ierr, mythid )

        refdate(1) =   -4370
        refdate(2) = -120000
        refdate(3) =       0
        refdate(4) =      -1

c       Some useful numbers.
        nmonthyear       = 12
        ndaysnoleap      = 0
        ndaysleap        = 0
        nmaxdaymonth     = 0
        hoursperday      = 24
        minutesperday    = 1440
        minutesperhour   = 60
        secondsperday    = 86400
        secondsperhour   = 3600
        secondsperminute = 60
        do i=1,nmonthyear
          ndaymonth(i,1) = 0
          ndaymonth(i,2) = 0
        enddo
        stop ' stopped in cal_Set (Julian Calendar).'

      else if ( usingNoCalendar ) then

        ierr = 111
        call cal_PrintError( ierr, mythid )

        refdate(1) =  0
        refdate(2) =  0
        refdate(3) =  0
        refdate(4) = -1

c       Some useful numbers.
        nmonthyear       = 12
        ndaysnoleap      = 0
        ndaysleap        = 0
        nmaxdaymonth     = 0
        hoursperday      = 24
        minutesperday    = 1440
        minutesperhour   = 60
        secondsperday    = 86400
        secondsperhour   = 3600
        secondsperminute = 60
        do i=1,nmonthyear
          ndaymonth(i,1) = 0
          ndaymonth(i,2) = 0
        enddo

        stop ' stopped in cal_Set (No Calendar).'

      else

        ierr = 101
        call cal_PrintError( ierr, mythid )
        stop

      endif

c     A next set of checks of the user specifications.
c     Number of possible modelsteps per calendar day.
      modelstepsperday = cal_nStepDay(mythid)
      if (modelstepsperday .eq. 0 ) then
        ierr = 105
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      endif

c     Complete the start date specification to get a full date array.
      call cal_FullDate( modstartdate_1, modstartdate_2,
     &                   modelstartdate, mythid )

c     From here on, the final calendar settings are determined by the
c     following variables:
c
c           modelstep, modelstart, modelstartdate, and modeliter0.

c     Two scenarios are allowed:
c
c     First case:  modelintsteps is given as well, modelenddate is
c                  set to zero.
c     Second case: modelintsteps is set to zero, modelenddate is given.

      if ( (modelintsteps   .ne. 0)    .and.
     &     ( (modenddate_1 .eq. 0)     .and.
     &       (modenddate_2 .eq. 0) ) ) then

        runtimesecs = float(modelintsteps)*modelstep
        modelend    = modelstart + runtimesecs

      else if ( (modelintsteps   .eq. 0)        .and.
     &          (.not. ( (modenddate_1 .eq. 0 ) .and.
     &                   (modenddate_2 .eq. 0) ) ) ) then

        call cal_FullDate( modenddate_1, modenddate_2, modelenddate,
     &                     mythid )
        call cal_TimePassed( modelstartdate, modelenddate, datediff,
     &                       mythid )
        call cal_ToSeconds( datediff, runtimesecs, mythid )

        if ( runtimesecs .lt. 0.) then
          ierr = 107
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_Set.'
        endif

        modelintsteps = int(runtimesecs/modelstep)
        runtimesecs   = modelintsteps*modelstep
        modelend      = modelstart + runtimesecs

      else
        ierr = 106
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      endif

c     Determine the startdate of the integration.
c     (version 0.1.3 >> START << )
      iterinisecs = float(modeliter0)*modelstep
      call cal_TimeInterval( iterinisecs, 'secs', iterinitime, mythid )
      call cal_AddTime( modelstartdate, iterinitime, modelrundate,
     &                  mythid )
      call cal_CopyDate( modelrundate, modelstartdate, mythid )
c     (version 0.1.3 >> END << )

      call cal_TimeInterval( runtimesecs, 'secs', timediff, mythid )
      call cal_AddTime( modelstartdate, timediff, modelenddate,
     &                  mythid )

      modeliterend = modeliter0 + modelintsteps

c     Check consistency of the numerical model and the calendar tool.
      if ( modelstart .ne. modstart) then
        ierr = 112
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      else if ( modelend .ne. modend ) then
        ierr = 113
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      else if ( modelstep .ne. modstep ) then
        ierr = 114
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      else if ( modeliter0 .ne. moditerini ) then
        ierr = 115
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
#ifdef ALLOW_CAL_NENDITER
      else if ( modeliterend .ne. moditerend ) then
        ierr = 116
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
#endif
      else if ( modelintsteps .ne. modintsteps) then
        ierr = 117
        call cal_PrintError( ierr, mythid )
        stop ' stopped in cal_Set.'
      endif

      return
      end

