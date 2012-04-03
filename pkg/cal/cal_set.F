C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.7 2012/04/03 15:12:21 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_SET(
     I                    modstart, modend, modstep,
     I                    moditerini, moditerend, modintsteps,
     I                    modstartdate_1, modstartdate_2,
     I                    modcalendartype,
     I                    myThid )

C     ==================================================================
C     SUBROUTINE cal_Set
C     ==================================================================
C
C     o This routine initialises the calendar according to the user
C       specifications in "data".
C
C     Purpose: Precalculations for the calendar.
C
C              Given the type of calendar that should be used date
C              arrays and some additional information is returned.
C
C              Check for consistency with other specifications such
C              as modintsteps.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  19-Jan-2000
C
C              - Changed the role of the routine arguments. Chris Hill
C                proposed to make the calendar less "invasive". The tool
C                now assumes that the MITgcmUV already provides an ade-
C                quate set of time stepping parameters. The calendar
C                only associates a date with the given starttime of the
C                numerical model. startdate corresponds to zero start-
C                time. So, given niter0 or startdate .ne. zero the actual
C                startdate of the current integration is shifted by the
C                time interval correponding to niter0, startdate respec-
C                tively.
C
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C
C              Christian Eckert eckert@mit.edu  23-Feb-2000
C
C              - Corrected the declaration of *modelrundate*
C                --> integer modelrundate(4)
C
C     ==================================================================
C     SUBROUTINE cal_Set
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "cal.h"

C     == routine arguments ==

C     modstart        :: start time of the model integration
C     modend          :: end time of the model integration
C     modstep         :: timestep of the numerical model
C     moditerini      :: initial iteration number of the model
C     moditerend      :: last iteration number of the model
C     modintsteps     :: number of timesteps that are to be performed.
C     modstartdate_1  :: startdate of the integration: yyyymmdd
C     modstartdate_2  :: startdate of the integration: hhmmss
C     modcalendartype :: the type of calendar that is to be used.
C                        Available: 'model' or 'gregorian'
C     myThid          :: my Thread Id number
C-jmc: should remove modenddate_1 & modenddate_2 (always identically zero in
C      cal_readparams)

      _RL     modstart
      _RL     modend
      _RL     modstep
      INTEGER moditerini
      INTEGER moditerend
      INTEGER modintsteps
      INTEGER modstartdate_1
      INTEGER modstartdate_2
      CHARACTER*(*) modcalendartype
      INTEGER myThid

C     == local variables ==
      INTEGER i,j,k
      INTEGER ierr
      INTEGER timediff(4)
      INTEGER iterinitime(4)
      INTEGER modelrundate(4)
      _RL     runtimesecs
      _RL     iterinisecs

C     == end of interface ==

C     Initialise some variables.
      usingNoCalendar        = .false.
      usingGregorianCalendar = .false.
      usingModelCalendar     = .false.
      usingJulianCalendar    = .false.

C     Map the numerical model parameters. --> common blocks in CALENDAR.h
      modelstart       = modstart
      modelend         = modend
      modelstep        = modstep
      modeliter0       = moditerini
      modeliterend     = moditerend
      modelintsteps    = modintsteps

C     Do first consistency checks
C     o Time step.
      if ( modelstep .le. 0. ) then
        ierr = 102
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_Set.'
      endif
      if ( modelstep .lt. 1. ) then
        ierr = 103
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_Set.'
      endif
      if ( abs(modelstep - nint(modelstep)) .gt. 0.000001 ) then
        ierr = 104
        call cal_PrintError( ierr, myThid )
        stop ' stopped in cal_Set.'
      else
        modelstep = float(nint(modelstep))
      endif

C     o Start time
c     if ( modeliter0 .ne. 0 .and. modelstart .eq. 0. ) then
c        modelstart = modelstep*float(modeliter0)
c     endif
C     o modeliter0
c     if ( modeliter0 .eq. 0 .and. modelstart .ne. 0. ) then
c        modeliter0 = int( modelstart/modelstep )
c     endif

C     o modelintsteps
c     if ( modelintsteps .eq. 0 .and. modeliterend .ne. 0 )
c    &     modelintsteps = modeliterend - modeliter0
c     if ( modelintsteps .eq. 0 .and. modelend .ne. 0. )
c    &     modelintsteps = int(0.5 + (modelend - modelstart)/modelstep)

C     o modeliterend
c     if ( modeliterend .eq. 0 .and. modelintsteps .ne. 0 )
c    &     modeliterend = modeliter0 + modelintsteps
c     if ( modeliterend .eq. 0 .and. modelend .ne. 0. )
c    &     modeliterend = int(0.5 + modelend/modelstep)

C     o modelend
c     if ( modelend .eq. 0. .and. modelintsteps .ne. 0 )
c    &     modelend = modelstart + modelstep*float(modelintsteps)
c     if ( modelend .eq. 0. .and. modeliterend .ne. 0 )
c    &     modelend = modelstep*float(modeliterend)

C     Start setting the calendar parameters.

C     The calendar type.
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
C       The reference date for the Gregorian Calendar.
C       and its format: ( yymmdd , hhmmss , leap year, weekday )
C                                             (1/2)    (1 - 7)
C       The Gregorian calendar starts on Friday, 15 Oct. 1582.
        refdate(1) = 15821015
        refdate(2) = 0
        refdate(3) = 1
        refdate(4) = 1

C       Number of months per year and other useful numbers.
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

C       Number of days per month.
C       The "magic" number 2773 derives from the sequence: 101010110101
C         read in reverse and interpreted as a dual number. An
C         alternative would be to take 2741 with the loop being
C         executed in reverse order. Accidentially, the latter
C         is a prime number.
        k=2773
        do i=1,nmonthyear
          j = mod(k,2)
          k = (k-j)/2
          ndaymonth(i,1) = 30+j
          ndaymonth(i,2) = 30+j
        enddo
        ndaymonth(2,1) = 28
        ndaymonth(2,2) = 29

C       Week days.
        dayofweek(1) = 'FRI'
        dayofweek(2) = 'SAT'
        dayofweek(3) = 'SUN'
        dayofweek(4) = 'MON'
        dayofweek(5) = 'TUE'
        dayofweek(6) = 'WED'
        dayofweek(7) = 'THU'

      else if ( usingModelCalendar ) then
C       Assume a model calendar having 12 months with thirty days each.
C       Reference date is the first day of year 0 at 0am, and model
C       day 1.
        refdate(1) = 00000101
        refdate(2) = 0
        refdate(3) = 1
        refdate(4) = 1

C       Some useful numbers.
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

C       Week days (Model Day 1 - 7).
        dayofweek(1) = 'MD1'
        dayofweek(2) = 'MD2'
        dayofweek(3) = 'MD3'
        dayofweek(4) = 'MD4'
        dayofweek(5) = 'MD5'
        dayofweek(6) = 'MD6'
        dayofweek(7) = 'MD7'

c     else if ( usingJulianCalendar ) then
c       stop ' stopped in cal_Set (Julian Calendar).'
c     else if ( usingNoCalendar ) then
c       stop ' stopped in cal_Set (No Calendar).'
      else
        ierr = 101
        call cal_PrintError( ierr, myThid )
        stop
      endif

C     A next set of checks of the user specifications.
C     Number of possible modelsteps per calendar day.
C-jmc: modelstepsperday is not used and can be removed
c     modelstepsperday = cal_nStepDay(mythid)
cdm   if (modelstepsperday .eq. 0 ) then
cdm     ierr = 105
cdm     call cal_PrintError( ierr, mythid )
cdm     stop ' stopped in cal_Set.'
cdm   endif

C     Complete the start date specification to get a full date array.
      call cal_FullDate( modstartdate_1, modstartdate_2,
     &                   modelstartdate, myThid )

C     From here on, the final calendar settings are determined by the
C     following variables:
C
C           modelstep, modelstart, modelstartdate, and modeliter0.
C-jmc: simplify so that calendar settings only function of:
C           modelstart, modelstep*modelintsteps & modelstartdate

C     Two scenarios are allowed:
C
C     First case:  modelintsteps is given as well, modelenddate is
C                  set to zero.
C     Second case: modelintsteps is set to zero, modelenddate is given.
C-jmc: can only be 1rst case since modelintsteps (coming from model)
C      is always set and modenddate_1 & 2 are always zero.

c     if ( (modelintsteps   .ne. 0)    .and.
c    &     ( (modenddate_1 .eq. 0)     .and.
c    &       (modenddate_2 .eq. 0) ) ) then

        runtimesecs = modelintsteps*modelstep
C-jmc: no need to reset modelend !
c       modelend    = modelstart + runtimesecs

c     else
c      CALL PRINT_ERROR('S/R CAL_SET: you win ! error 106 !!!',myThid)
c       ierr = 106
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     endif

C     Determine the startdate of the integration.
C     (version 0.1.3 >> START << )
c     iterinisecs = float(modeliter0)*modelstep
C-jmc: above does not work if baseTime <> 0 ; fix it below:
      iterinisecs = modelstart
      call cal_TimeInterval( iterinisecs, 'secs', iterinitime, myThid )
      call cal_AddTime( modelstartdate, iterinitime, modelrundate,
     &                  myThid )
      call cal_CopyDate( modelrundate, modelstartdate, myThid )
C     (version 0.1.3 >> END << )

      call cal_TimeInterval( runtimesecs, 'secs', timediff, myThid )
      call cal_AddTime( modelstartdate, timediff, modelenddate,
     &                  myThid )

C-jmc: no need to reset modeliterend !
c     modeliterend = modeliter0 + modelintsteps

C     Check consistency of the numerical model and the calendar tool.
C-jmc: all model time/iter were set earlier and have not been changed
C-jmc ==> removed
c     if ( modelstart .ne. modstart) then
c       ierr = 112
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     else if ( modelend .ne. modend ) then
c       ierr = 113
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     else if ( modelstep .ne. modstep ) then
c       ierr = 114
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     else if ( modeliter0 .ne. moditerini ) then
c       ierr = 115
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     else if ( modeliterend .ne. moditerend ) then
c       ierr = 116
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     else if ( modelintsteps .ne. modintsteps) then
c       ierr = 117
c       call cal_PrintError( ierr, mythid )
c       stop ' stopped in cal_Set.'
c     endif

      RETURN
      END
