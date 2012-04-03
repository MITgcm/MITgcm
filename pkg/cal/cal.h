C $Header: /u/gcmpack/MITgcm/pkg/cal/cal.h,v 1.6 2012/04/03 15:20:29 jmc Exp $
C $Name:  $

C     ==================================================================
C     HEADER calendar
C     ==================================================================
C
C     o This header file contains variables that are used by the
C       calendar tool. The calendar tool can be used in the ECCO
C       SEALION release of the MITgcmUV.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  17-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C
C     ==================================================================
C     HEADER calendar
C     ==================================================================
C
C   - The calendar version:
      CHARACTER*(5) calendarversion
      PARAMETER(    calendarversion = '0.2.0' )

C   - Parameters of the numerical model:
C
C     modelstart       - start time of the numerical model.
C     modelstartdate   - start date of the numerical model.
C     modelend         - end   time of the numerical model.
C     modelenddate     - end   date of the numerical model.
C     modelstep        - timestep of the numerical model.
C     modelintsteps    - number of timestep that are to be performed.
C     modeliter0       - the numerical models initial timestep number.
C     modeliterend     - the models last timestep number.
cC    modelstepsperday - number of model time steps per calendar day.
C
C   - Parameters used by the calendar:
C
C     refdate          - first day of the Gregorian Calendar.
C     nmonthyear       - number months in a year.
C     ndaymonth        - days per month depending on the year being a
C                        leap year or not. If the Gregorian calendar is
C                        not used a 360 days year with 30 days months is
C                        used instead.
C     ndaysnoleap      - number of days in a usual year.
C     ndaysleap        - number of days in a leap year.
C     nmaxdaymonth     - maximum number of days in a years month.
C     hoursperday      - number of hours   in a calendars day.
C     minutesperday    - number of minutes in a calendars day.
C     minutesperhour   - number of minutes in a calendars hour.
C     secondsperday    - number of seconds in a calendars day.
C     secondsperhour   - number of seconds in a calendars hour.
C     secondsperminute - number of seconds in a calendars minute.

      COMMON /cal_rl/
     &                modelstart,
     &                modelend,
     &                modelstep
      _RL modelstart
      _RL modelend
      _RL modelstep

      COMMON /cal_i/
     &               refdate,
     &               nmonthyear,
     &               ndaymonth,
     &               ndaysnoleap,
     &               ndaysleap,
     &               nmaxdaymonth,
     &               hoursperday,
     &               minutesperday,
     &               minutesperhour,
     &               secondsperday,
     &               secondsperhour,
     &               secondsperminute,
     &               modelstartdate,
     &               modelenddate,
     &               modeliter0,
     &               modeliterend,
     &               modelintsteps,
     &               startdate_1,
     &               startdate_2

      INTEGER refdate(4)
      INTEGER nmonthyear
      INTEGER ndaymonth(12,2)
      INTEGER ndaysnoleap
      INTEGER ndaysleap
      INTEGER nmaxdaymonth
      INTEGER hoursperday
      INTEGER minutesperday
      INTEGER minutesperhour
      INTEGER secondsperday
      INTEGER secondsperhour
      INTEGER secondsperminute

      INTEGER modelstartdate(4)
      INTEGER modelenddate(4)
      INTEGER modeliter0
      INTEGER modeliterend
      INTEGER modelintsteps

      INTEGER startdate_1
      INTEGER startdate_2


C   calendarDumps :: When set, approximate months (30-31 days) and years (360-372 days)
C                    for parameters chkPtFreq, pChkPtFreq, taveFreq, SEAICE_taveFreq,
C                    KPP_taveFreq, and freq in pkg/diagnostics are converted to exact
C                    calendar months and years.  Requires pkg/cal.
      COMMON /cal_l/
     &               calendarDumps,
     &               usingNoCalendar,
     &               usingModelCalendar,
     &               usingJulianCalendar,
     &               usingGregorianCalendar
      LOGICAL calendarDumps
      LOGICAL usingNoCalendar
      LOGICAL usingModelCalendar
      LOGICAL usingJulianCalendar
      LOGICAL usingGregorianCalendar

C     dayofweek   - Week day number one is the week day of refdate.
C                   For the Gregorian calendar this is Friday, 15-Oct-1582.
C
C     monthofyear - Both available calendars are assumed to have twelve
C                   months.
      COMMON /calendar_ch/
     &                     dayofweek,
     &                     monthofyear
      CHARACTER*(3) dayofweek(7)
      CHARACTER*(3) monthofyear(12)

