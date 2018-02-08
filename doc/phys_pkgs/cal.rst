.. _sub_phys_pkg_cal:

CAL: The calendar package
-------------------------


Authors: Christian Eckert and Patrick Heimbach

This calendar tool was originally intended to enable the use of absolute
dates (Gregorian Calendar dates) in MITgcm. There is, however, a fair
number of routines that can be used independently of the main MITgcm
executable. After some minor modifications the whole package can be used
either as a stand-alone calendar or in connection with any dynamical
model that needs calendar dates. Some straightforward extensions are
still pending e.g. the availability of the Julian Calendar, to be able
to resolve fractions of a second, and to have a time- step that is
longer than one day.

Basic assumptions for the calendar tool
#######################################

It is assumed that the SMALLEST TIME INTERVAL to be resolved is ONE
SECOND.

Further assumptions are that there is an INTEGER NUMBER OF MODEL STEPS
EACH DAY, and that AT LEAST ONE STEP EACH DAY is made.

Not each individual routine depends on these assumptions; there are only
a few places where they enter.

Format of calendar dates
########################

In this calendar tool a complete date specification is defined as the
following integer array:

::

    c           integer date(4)
    c
    c           ( yyyymmdd, hhmmss, leap_year, dayofweek )
    c
    c             date(1) = yyyymmdd    <-- Year-Month-Day
    c             date(2) =   hhmmss    <-- Hours-Minutes-Seconds
    c             date(3) = leap_year   <-- Leap Year/No Leap Year
    c             date(4) = dayofweek   <-- Day of the Week
    c
    c             leap_year is either equal to 1 (normal year)
    c                              or equal to 2 (leap year)
    c
    c             dayofweek has a range of 1 to 7.

In case the Gregorian Calendar is used, the first day of the week is
Friday, since day of the Gregorian Calendar was Friday, 15 Oct. 1582. As
a date array this date would be specified as

::

    c               refdate(1) = 15821015
    c               refdate(2) =        0
    c               refdate(3) =        1
    c               refdate(4) =        1

Calendar dates and time intervals
#################################

Subtracting calendar dates yields time intervals. Time intervals have
the following format:

::

    c         integer datediff(4)
    c
    c           datediff(1) = # Days
    c           datediff(2) = hhmmss
    c           datediff(3) =      0
    c           datediff(4) =     -1

Such time intervals can be added to or can be subtracted from calendar
dates. Time intervals can be added to and be subtracted from each other.

Using the calendar together with MITgcm
#######################################

Each routine has as an argument the thread number that it is belonging
to, even if this number is not used in the routine itself.

In order to include the calendar tool into the MITgcm setup the MITgcm
subroutine “initialise.F” or the routine “initilise\_fixed.F”, depending
on the MITgcm release, has to be modified in the following way:

::

    c         #ifdef ALLOW_CALENDAR
    c         C--   Initialise the calendar package.
    c         #ifdef USE_CAL_NENDITER
    c               CALL cal_Init(
    c              I               startTime,
    c              I               endTime,
    c              I               deltaTclock,
    c              I               nIter0,
    c              I               nEndIter,
    c              I               nTimeSteps,
    c              I               myThid
    c              &             )
    c         #else
    c               CALL cal_Init(
    c              I               startTime,
    c              I               endTime,
    c              I               deltaTclock,
    c              I               nIter0,
    c              I               nTimeSteps,
    c              I               myThid
    c              &             )
    c         #endif
    c               _BARRIER
    c         #endif

It is useful to have the CPP flag ALLOW\_CALENDAR in order to switch
from the usual MITgcm setup to the one that includes the calendar tool.
The CPP flag USE\_CAL\_NENDITER has been introduced in order to enable
the use of the calendar for MITgcm releases earlier than checkpoint 25
which do not have the global variable \*nEndIter\*.

The individual calendars
########################

Simple model calendar:

This calendar can be used by defining

::

    c                  TheCalendar='model'

in the calendar’s data file “data.cal”.

In this case a year is assumed to have 360 days. The model year is
divided into 12 months with 30 days each.

Gregorian Calendar:

This calendar can be used by defining

::

    c                  TheCalendar='gregorian'

in the calendar’s data file “data.cal”.

Short routine description
#########################

::

    c      o  cal_Init          - Initialise the calendar. This is the interface
    c                             to MITgcm.
    c
    c      o  cal_Set           - Sets the calendar according to the user
    c                             specifications.
    c
    c      o  cal_GetDate       - Given the model's current timestep or the
    c                             model's current time return the corresponding
    c                             calendar date.
    c
    c      o  cal_FullDate      - Complete a date specification (leap year and
    c                             day of the week).
    c
    c      o  cal_IsLeap        - Determine whether a given year is a leap year.
    c
    c      o  cal_TimePassed    - Determine the time passed between two dates.
    c
    c      o  cal_AddTime       - Add a time interval either to a time interval
    c                             or to a date.
    c
    c      o  cal_TimeInterval  - Given a time interval return the corresponding
    c                             date array.
    c
    c      o  cal_SubDates      - Determine the time interval between two dates
    c                             or between two time intervals.
    c
    c      o  cal_ConvDate      - Decompose a date array or a time interval
    c                             array into its components.
    c
    c      o  cal_CopyDate      - Copy a date array or a time interval array to
    c                             another array.
    c
    c      o  cal_CompDates     - Compare two calendar dates or time intervals. 
    c
    c      o  cal_ToSeconds     - Given a time interval array return the number
    c                             of seconds.
    c
    c      o  cal_WeekDay       - Return the weekday as a string given the
    c                             calendar date.
    c
    c      o  cal_NumInts       - Return the number of time intervals between two
    c                             given dates.
    c
    c      o  cal_StepsPerDay   - Given an iteration number or the current
    c                             integration time return the number of time
    c                             steps to integrate in the current calendar day.
    c
    c      o  cal_DaysPerMonth  - Given an iteration number or the current
    c                             integration time return the number of days
    c                             to integrate in this calendar month.
    c
    c      o  cal_MonthsPerYear - Given an iteration number or the current
    c                             integration time return the number of months
    c                             to integrate in the current calendar year.
    c
    c      o  cal_StepsForDay   - Given the integration day return the number
    c                             of steps to be integrated, the first step,
    c                             and the last step in the day specified. The
    c                             first and the last step refer to the total
    c                             number of steps (1, ... , cal_IntSteps).
    c
    c      o  cal_DaysForMonth  - Given the integration month return the number
    c                             of days to be integrated, the first day,
    c                             and the last day in the month specified. The
    c                             first and the last day refer to the total
    c                             number of steps (1, ... , cal_IntDays).
    c
    c      o  cal_MonthsForYear - Given the integration year return the number
    c                             of months to be integrated, the first month,
    c                             and the last month in the year specified. The
    c                             first and the last step refer to the total
    c                             number of steps (1, ... , cal_IntMonths).
    c
    c      o  cal_Intsteps      - Return the number of calendar years that are
    c                             affected by the current integration.
    c
    c      o  cal_IntDays       - Return the number of calendar days that are
    c                             affected by the current integration.
    c
    c      o  cal_IntMonths     - Return the number of calendar months that are
    c                             affected by the current integration.
    c
    c      o  cal_IntYears      - Return the number of calendar years that are
    c                             affected by the current integration.
    c
    c      o  cal_nStepDay      - Return the number of time steps that can be
    c                             performed during one calendar day.
    c
    c      o  cal_CheckDate     - Do some simple checks on a date array or on a
    c                             time interval array.
    c
    c      o  cal_PrintError    - Print error messages according to the flags
    c                             raised by the calendar routines.
    c
    c      o  cal_PrintDate     - Print a date array in some format suitable for
    c                             MITgcm's protocol output.
    c
    c      o  cal_TimeStamp     - Given the time and the iteration number return
    c                             the date and print all the above numbers.
    c
    c      o  cal_Summary       - List all the setttings of the calendar tool.


Experiments and tutorials that use cal
######################################

-  Global ocean experiment in global\_with\_exf verification directory.

-  Labrador Sea experiment in lab\_sea verification directory.


