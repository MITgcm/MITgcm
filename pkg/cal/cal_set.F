C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.8 2012/04/05 22:00:08 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_SET(
     I                    modstart, modend, modstep,
     I                    moditerini, moditerend, modintsteps,
     I                    myThid )

C     ==================================================================
C     SUBROUTINE cal_Set
C     ==================================================================
C
C     o This routine initialises the calendar according to the user
C       specifications in "data".
C
C     Purpose: Precalculations for the calendar.
C              Given the type of calendar that should be used date
C              arrays and some additional information is returned.
C              Check for consistency with other specifications such
C              as modintsteps.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C              Christian Eckert eckert@mit.edu  19-Jan-2000
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
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C              Christian Eckert eckert@mit.edu  23-Feb-2000
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
C     myThid          :: my Thread Id number

      _RL     modstart
      _RL     modend
      _RL     modstep
      INTEGER moditerini
      INTEGER moditerend
      INTEGER modintsteps
      INTEGER myThid

C     == local variables ==
C     modelBaseDate :: full date array for startdate_1,startdate_2
C                       (corresponds to model baseTime, iter=0)
      INTEGER i,j,k
      INTEGER ierr
      INTEGER timediff(4)
      INTEGER iterinitime(4)
      INTEGER modelBaseDate(4)
      _RL     runtimesecs
      _RL     iterinisecs
C     == end of interface ==

      _BEGIN_MASTER(myThid)

C     Initialise some variables.
      usingGregorianCalendar = .FALSE.
      usingModelCalendar     = .FALSE.
      usingJulianCalendar    = .FALSE.

C     The calendar type: Start setting the calendar parameters.

      if ( TheCalendar .eq. 'gregorian') then
        usingGregorianCalendar = .TRUE.
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

      else if ( TheCalendar .eq. 'model') then
        usingModelCalendar = .TRUE.
C       Assume a model calendar having 12 months with thirty days each.
C       Reference date is the first day of year 0 at 0am, and model day 1.
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

c     else if ( TheCalendar .eq. 'julian') then
c       usingJulianCalendar = .TRUE.
c       stop ' stopped in cal_Set (Julian Calendar).'
c     else if ( TheCalendar .eq. 'none') then
c       usingNoCalendar = .TRUE.
c       stop ' stopped in cal_Set (No Calendar).'
      else
        ierr = 101
        call cal_PrintError( ierr, myThid )
        stop
      endif

C-    Record completion of calendar settings: stage 1 = calendar is defined
      cal_setStatus = 1

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

C-    Record completion of calendar settings: stage 2 = numerical model parms
      cal_setStatus = 2

C     Complete the start date specification to get a full date array.
      call cal_FullDate( startdate_1, startdate_2,
     &                   modelBaseDate, myThid )

C     From here on, the final calendar settings are determined by the
C     following variables:
C               modelstart, modelstep*modelintsteps & modelBaseDate

      runtimesecs = modelintsteps*modelstep

C     Determine the startdate of the integration.
c     iterinisecs = float(modeliter0)*modelstep
C-jmc: above does not work if baseTime <> 0 ; fix it below:
      iterinisecs = modelstart
      call cal_TimeInterval( iterinisecs, 'secs', iterinitime, myThid )
      call cal_AddTime( modelBaseDate, iterinitime, modelstartdate,
     &                  myThid )

      call cal_TimeInterval( runtimesecs, 'secs', timediff, myThid )
      call cal_AddTime( modelstartdate, timediff, modelenddate,
     &                  myThid )

C-    Record completion of calendar settings: stage 3 = fully set-up.
      cal_setStatus = 3

      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be set
      _BARRIER

      RETURN
      END
