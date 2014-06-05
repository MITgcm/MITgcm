C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_set.F,v 1.10 2014/06/05 19:38:45 jmc Exp $
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

C-    Initialise some variables.
      usingNoLeapYearCal     = .FALSE.
      usingGregorianCalendar = .FALSE.
      usingModelCalendar     = .FALSE.
      usingJulianCalendar    = .FALSE.

C-    Set calendar parameters which are independent of the calendar choice:
      hoursPerDay      = 24
      minutesPerHour   = 60
      minutesPerDay    = minutesPerHour*hoursPerDay
      secondsPerMinute = 60
      secondsPerHour   = secondsPerMinute*minutesPerHour
      secondsPerDay    = secondsPerMinute*minutesPerDay

C-    Select which calendar type to use:
      IF ( theCalendar .EQ. 'gregorian') THEN
        usingGregorianCalendar = .TRUE.
c     ELSE IF ( theCalendar .EQ. 'julian') THEN
c       usingJulianCalendar = .TRUE.
c       STOP ' stopped in cal_Set (Julian Calendar).'
      ELSE IF ( theCalendar .EQ. 'noLeapYear') THEN
        usingNoLeapYearCal = .TRUE.
      ELSE IF ( theCalendar .EQ. 'model') THEN
        usingModelCalendar = .TRUE.
c     ELSE IF ( theCalendar .EQ. 'none') THEN
c       usingNoCalendar = .TRUE.
c       STOP ' stopped in cal_Set (No Calendar).'
      ELSE
        ierr = 101
        CALL cal_PrintError( ierr, myThid )
        STOP
      ENDIF

C-    Set calendar parameters according to the calendar type:

      IF ( usingGregorianCalendar .OR. usingNoLeapYearCal ) THEN
C       The reference date for the Gregorian Calendar.
C       and its format: ( yymmdd , hhmmss , leap year, weekday )
C                                             (1/2)    (1 - 7)
C       The Gregorian calendar starts on Friday, 15 Oct. 1582.
        refDate(1) = 15821015
        refDate(2) = 0
        refDate(3) = 1
        refDate(4) = 1

C       Number of months per year and other useful numbers.
        nDaysNoLeap      = 365
        nDaysLeap        = 366
        nMaxDayMonth     = 31

C       Number of days per month.
C       The "magic" number 2773 derives from the sequence: 101010110101
C         read in reverse and interpreted as a dual number. An
C         alternative would be to take 2741 with the loop being
C         executed in reverse order. Accidentially, the latter
C         is a prime number.
        k=2773
        DO i=1,nMonthYear
          j = MOD(k,2)
          k = (k-j)/2
          nDayMonth(i,1) = 30+j
          nDayMonth(i,2) = 30+j
        ENDDO
        nDayMonth(2,1) = 28
        nDayMonth(2,2) = 29

C       Week days.
        dayOfWeek(1) = 'FRI'
        dayOfWeek(2) = 'SAT'
        dayOfWeek(3) = 'SUN'
        dayOfWeek(4) = 'MON'
        dayOfWeek(5) = 'TUE'
        dayOfWeek(6) = 'WED'
        dayOfWeek(7) = 'THU'
      ENDIF

      IF ( usingModelCalendar ) THEN
C       Assume a model calendar having 12 months with thirty days each.
C       Reference date is the first day of year 0 at 0am, and model day 1.
        refDate(1) = 00000101
        refDate(2) = 0
        refDate(3) = 1
        refDate(4) = 1

C       Some useful numbers.
        nDaysNoLeap      = 360
        nDaysLeap        = 360
        nMaxDayMonth     = 30
        DO i=1,nMonthYear
          nDayMonth(i,1) = 30
          nDayMonth(i,2) = 30
        ENDDO

C       Week days (Model Day 1 - 7).
        dayOfWeek(1) = 'MD1'
        dayOfWeek(2) = 'MD2'
        dayOfWeek(3) = 'MD3'
        dayOfWeek(4) = 'MD4'
        dayOfWeek(5) = 'MD5'
        dayOfWeek(6) = 'MD6'
        dayOfWeek(7) = 'MD7'

      ENDIF

C-    Record completion of calendar settings: stage 1 = calendar is defined
      cal_setStatus = 1

C     Map the numerical model parameters. --> common blocks in CALENDAR.h
      modelStart       = modstart
      modelEnd         = modend
      modelStep        = modstep
      modelIter0       = moditerini
      modelIterEnd     = moditerend
      modelIntSteps    = modintsteps

C     Do first consistency checks
C     o Time step.
      IF ( modelStep .LE. 0. ) THEN
        ierr = 102
        CALL cal_PrintError( ierr, myThid )
        STOP ' stopped in cal_Set.'
      ENDIF
      IF ( modelStep .LT. 1. ) THEN
        ierr = 103
        CALL cal_PrintError( ierr, myThid )
        STOP ' stopped in cal_Set.'
      ENDIF
      IF ( ABS(modelStep - NINT(modelStep)) .GT. 0.000001 ) THEN
        ierr = 104
        CALL cal_PrintError( ierr, myThid )
        STOP ' stopped in cal_Set.'
      ELSE
        modelStep = FLOAT(NINT(modelStep))
      ENDIF

C-    Record completion of calendar settings: stage 2 = numerical model parms
      cal_setStatus = 2

C     Complete the start date specification to get a full date array.
      CALL cal_FullDate( startdate_1, startdate_2,
     &                   modelBaseDate, myThid )

C     From here on, the final calendar settings are determined by the
C     following variables:
C               modelStart, modelStep*modelIntSteps & modelBaseDate

      runtimesecs = modelIntSteps*modelStep

C     Determine the startdate of the integration.
c     iterinisecs = float(modelIter0)*modelStep
C-jmc: above does not work if baseTime <> 0 ; fix it below:
      iterinisecs = modelStart
      CALL cal_TimeInterval( iterinisecs, 'secs', iterinitime, myThid )
      CALL cal_AddTime( modelBaseDate, iterinitime, modelStartDate,
     &                  myThid )

      CALL cal_TimeInterval( runtimesecs, 'secs', timediff, myThid )
      CALL cal_AddTime( modelStartDate, timediff, modelEndDate,
     &                  myThid )

C-    Record completion of calendar settings: stage 3 = fully set-up.
      cal_setStatus = 3

      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be set
      _BARRIER

      RETURN
      END
