C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_printerror.F,v 1.1 2001/05/14 22:07:27 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

      subroutine cal_PrintError(
     I                           calerr,
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cal_PrintError
c     ==================================================================
c
c     Purpose: Use the MITgcmuv's print routines to document errors that
c              occured during the execution of the calendar tool.
c
c     o Calling this routine allows to print out an error message for
c       several errors that might occur.
c
c       The error codes for the calendar tool are specified by a four
c       digit integer:
c                         RRCC
c
c       The RR digits identify the routine that detected the error.
c       The CC digits identify the specific error in the routine that
c          detected the error.
c
c       RR translates to routines in the following way:
c
c         cal_Init             0
c         cal_Set              1
c         cal_GetDate          2
c         cal_FullDate         3
c         cal_IsLeap           4
c         cal_TimePassed       5
c         cal_AddTime          6
c         cal_TimeInterval     7
c         cal_SubDates         8
c         cal_ConvDate         9
c         cal_ToSeconds       10
c         cal_StepsPerDay     11
c         cal_DaysPerMonth    12
c         cal_MonthsPerYear   13
c         cal_IntYears        14
c         cal_IntMonths       15
c         cal_IntDays         16
c         cal_nStepDay        17
c         cal_CheckDate       18
c         cal_PrintError      19
c         cal_PrintDate       20
c         cal_NumInts         25
c
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              Christian Eckert eckert@mit.edu  10-Jan-2000
c
c              - Corrected the print statement for error code 104.
c                It contained more than 72 characters in one line.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c              Christian Eckert eckert@mit.edu  24-Feb-2000
c
c              - Included cal_NumInts error code.
c
c     ==================================================================
c     SUBROUTINE cal_PrintError
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.
c     calerr - error code

      integer mythid
      integer calerr

c     == local variables ==

      integer nroutine
      integer nerrcode
      logical missingerrcode

c     == end of interface ==

      nerrcode = mod(calerr,100)
      nroutine = (calerr - nerrcode)/100
      missingerrcode = .false.

      if (nroutine .eq. 0) then
c     Error in cal_Init

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_Init: Unable to open calendar parameter file'
          print*,
     &    '            file "data.calendar".'
        endif

      else if (nroutine .eq. 1) then
c     Error in cal_Set

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_Set: No appropriate calendar has been specified.'
        else if (nerrcode .eq. 2) then
          print*,
     &    '  cal_Set: The time step specified is not valid.'
        else if (nerrcode .eq. 3) then
          print*,
     &    '  cal_Set: The time step is less than a second.'
        else if (nerrcode .eq. 4) then
          print*,
     &    '  cal_Set: The time step contains fractions of a',
     &                   ' second.'
        else if (nerrcode .eq. 5) then
          print*,
     &    '  cal_Set: Less than one time step per calendar day.'
        else if (nerrcode .eq. 6) then
          print*,
     &    '  cal_Set: The specifications are incomplete. Please'
          print*,
     &    '                refer to the documentation.'
        else if (nerrcode .eq. 7) then
          print*,
     &    '  cal_Set: The final date of integration is before its'
          print*,
     &    '                start date.'
        else if (nerrcode .eq. 10) then
          print*,
     &    '  cal_Set: The Julian Calendar is not implemented yet.'
        else if (nerrcode .eq. 11) then
          print*,
     &    '  cal_Set: The No Calendar case is not implemented yet.'
        else if ( nerrcode .eq. 12) then
          print*,
     &    '  cal_Set: modelstart .ne. startTime ... please check.'
        else if ( nerrcode .eq. 13 ) then
          print*,
     &    '  cal_Set: modelend .ne. endTime ... please check.'
        else if ( nerrcode .eq. 14 ) then
          print*,
     &    '  cal_Set: modelstep .ne. deltaTclock ... please check.'
        else if ( nerrcode .eq. 15 ) then
          print*,
     &    '  cal_Set: modeliter0 .ne. nIter0 ... please check.'
        else if ( nerrcode .eq. 16 ) then
          print*,
     &    '  cal_Set: modeliterend .ne. nEndIter ... please check.'
        else if ( nerrcode .eq. 17 ) then
          print*,
     &    '  cal_Set: modelintsteps .ne. nTimeSteps',
     &    ' ... please check.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 2) then
c     Error in cal_GetDate

        missingerrcode = .true.

      else if (nroutine .eq. 3) then
c     Error in cal_FullDate

        missingerrcode = .true.

      else if (nroutine .eq. 4) then
c     Error in cal_IsLeap

        missingerrcode = .true.

      else if (nroutine .eq. 5) then
c     Error in cal_TimePassed

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_TimePassed: calendar and timeinterval cannot',
     &    ' be compared.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 6) then
c     Error in cal_AddTime

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_AddTime: not a valid time interval.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 7) then
c     Error in cal_TimeInterval

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_TimeInterval: not a valid time unit.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 8) then
c     Error in cal_SubDates

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_SubDates: Not a valid combination of calendar dates'
          print*,
     &    '             or time intervals.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 9) then
c     Error in cal_ConvDate

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_ConvDate: date specification has mixed signs.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 10) then
c     Error in cal_ToSeconds

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_ToSeconds: input not a time interval array.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 11) then
c     Error in cal_StepsPerDay

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_StepsPerDay: nothing else to do.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 12) then
c     Error in cal_DaysPerMonth

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_DaysPerMonth: current year after final year.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 13) then
c     Error in cal_MonthsPerYear

        missingerrcode = .true.

      else if (nroutine .eq. 14) then
c     Error in cal_IntYears

        missingerrcode = .true.

      else if (nroutine .eq. 15) then
c     Error in cal_IntMonths

        missingerrcode = .true.

      else if (nroutine .eq. 16) then
c     Error in cal_IntDays

        missingerrcode = .true.

      else if (nroutine .eq. 17) then
c     Error in cal_nStepDay

        missingerrcode = .true.

      else if (nroutine .eq. 18) then
c     Error in cal_CheckDate

        if (nerrcode .eq. 0) then
          print*,
     &    '   cal_CheckDate: A valid date specification!'
          print*,
     &    '               --> This only means that the format is ok!'
        else if (nerrcode .eq. 1) then
          print*,
     &    '   cal_CheckDate: Last component of array not valid!'
        else if (nerrcode .eq. 2) then
          print*,
     &    '   cal_CheckDate: Third component of interval array',
     &    '  not zero!'
        else if (nerrcode .eq. 3) then
          print*,
     &    '   cal_CheckDate: Signs of first two components unequal!'
        else if (nerrcode .eq. 4) then
          print*,
     &    '   cal_CheckDate: Second component not in hhmmss format!'
        else if (nerrcode .eq. 5) then
          print*,
     &    '   cal_CheckDate: Weekday indentifier not correct!'
        else if (nerrcode .eq. 6) then
          print*,
     &    '   cal_CheckDate: Leap year identifier not correct!'
        else if (nerrcode .eq. 7) then
          print*,
     &    '   cal_CheckDate: Calendar date before predefined reference',
     &    '               date!'
        else if (nerrcode .eq. 8) then
          print*,
     &    '   cal_CheckDate: First component not in yymmdd format!'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 19) then
c     Error in cal_PrintError

        missingerrcode = .true.

      else if (nroutine .eq. 20) then
c     Error in cal_PrintDate

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_PrintDate: date not a legal calendar array.'
        else
          missingerrcode = .true.
        endif

      else if (nroutine .eq. 21) then
c     Error in cal_PrintError

        missingerrcode = .true.

      else if (nroutine .eq. 25) then
c     Error in cal_NumInts

        if (nerrcode .eq. 1) then
          print*,
     &    '  cal_NumInts: Expected a time interval as third argument.'
        else
          missingerrcode = .true.
        endif

      else
        missingerrcode = .true.
      endif

      if (missingerrcode) then
        print*,'  cal_PrintError: routine called by an undefined'
        print*,'                  error code.'
        print*,'  cal_PrintError: error code = ',calerr
        stop   '  stopped in cal_PrintError.'
      endif

      return
      end

