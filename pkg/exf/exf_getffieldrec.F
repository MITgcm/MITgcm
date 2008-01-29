C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffieldrec.F,v 1.18 2008/01/29 11:25:53 dimitri Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_GetFFieldRec(
     I                             fldstartdate, fldperiod,
     I                             usefldyearlyfields,
     O                             fac,
     O                             first, changed,
     O                             count0, count1, year0, year1,
     I                             mytime,
     I                             myiter, mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_GetFFieldRec
c     ==================================================================
c
c     o Get flags, counters, and the linear interpolation factor for a
c       given field.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c              Curtis Heisey cheisey@mit.edu    19-Dec-2002
c              - added "repeatPeriod" for cycling of forcing datasets
c
c     menemenlis@jpl.nasa.gov
c     27-Dec-2002 bug fix for verification/global_with_exf
c     8-Oct-2003 speed-up computations for long integration interval
c
c     ==================================================================
c     SUBROUTINE exf_GetFFieldRec
c     ==================================================================

      implicit none

c     == global variables ==

c cal: modelstart, modelstep
#include "EEPARAMS.h"
#include "cal.h"
#include "EXF_PARAM.h"

c     == routine arguments ==

      _RL     fldstartdate, fldperiod
      logical usefldyearlyfields
      _RL     fac
      logical first, changed
      integer count0, count1, year0, year1
      _RL     mytime
      integer myiter, mythid

c     == local variables ==

c     mydate        :: model date of current time step
c     yearStartDate :: start of year date for flux record just before mydate
c     difftime      :: time difference between yearStartDate and mydate
c     fldsectot     :: time in seconds from fldstartdate to mydate
c     fldsecs       :: time from start of current forcing period to mydate
c     fldsecs0      :: time from start of repeat period to mydate
c     fldsecs1      :: time from end of current forcing period to mydate
c     secondsInYear :: seconds in the flux year just before mydate
c     myDateSeconds :: seconds from beginning of year to mydate

      integer mydate(4)
      integer yearStartDate(4)
      integer difftime(4)
      _RL fldsectot, fldsecs, fldsecs0, fldsecs1
      _RL secondsInYear, myDateSeconds

#ifdef EXF_VERBOSE
      character*(max_len_mbuf) msgbuf
#endif

c     == external ==

      integer  cal_IsLeap
      external cal_IsLeap

c     == end of interface ==

c     Set some default values.
      first = ((mytime - modelstart) .lt. 0.5*modelstep)
      changed = .false.

      if (.not.usefldyearlyfields) then

c     Determine offset in seconds from beginning of input data
c     to current date.
       fldsectot = mytime - fldstartdate

c     Determine the flux records just before and after mycurrentdate.
       if ( repeatPeriod .eq. 0. ) then

        if ( fldsectot .lt. 0 ) then
         print *, 'flux data not available for this date'
         stop 'ABNORMAL END: S/R EXF_GETFFIELDREC'
        endif
        count0       = int((fldsectot+0.5)/fldperiod) + 1
        count1       = count0 + 1
        fldsecs      = mod(fldsectot,fldperiod)

       else
c     if ( repeatPeriod .gt. 0. )

c     If using repeating data then make fldsectot cycle around.
        if(fldsectot.lt.0) fldsectot = fldsectot + repeatPeriod
        fldsecs0     = mod(fldsectot,repeatPeriod)
        count0       = int((fldsecs0+0.5)/fldperiod) + 1
        fldsecs1     = mod(fldsectot+fldperiod,repeatPeriod)
        count1       = int((fldsecs1+0.5)/fldperiod) + 1
        fldsecs      = mod(fldsecs0,fldperiod)

       endif

c     Weight belonging to count0 for linear interpolation purposes.
       fac = 1. - fldsecs/fldperiod

      else
c     if (usefldyearlyfields)

c     Determine seconds from beginning of year to model current time.
       call cal_GetDate( myiter, mytime, mydate, mythid )
       year0            = int(mydate(1)/10000.)
       yearStartDate(1) = year0 * 10000 + 101
       yearStartDate(2) = 0
       yearStartDate(3) = mydate(3)
       yearStartDate(4) = mydate(4)
       CALL cal_TimePassed(yearStartDate,mydate,difftime,myThid)
       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)

c     Determine the flux year just before mycurrentdate.
        if ( myDateSeconds .lt. fldstartdate ) year0 = year0 - 1

c     Determine seconds in the flux year just before mycurrentdate.
        secondsInYear = 31536000.
        if ( cal_IsLeap(year0,mythid) .eq. 2) secondsInYear = 31622400.

c     Determine the record just before mycurrentdate.
        if ( myDateSeconds .lt. fldstartdate )
     &       myDateSeconds = myDateSeconds + secondsInYear
        fldsectot = myDateSeconds - fldstartdate
        count0    = int((fldsectot+0.5)/fldperiod) + 1
        
c     Determine the flux year and record just after mycurrentdate.
        year1  = year0
        count1 = count0 + 1
        if ( (fldstartdate+count0*fldperiod) .gt. secondsInYear ) then
         year1  = year0 + 1
         count1 = 1
        endif

c     Weight belonging to count0 for linear interpolation purposes.
        fldsecs = mod(fldsectot,fldperiod)
        fac     = 1. - fldsecs/fldperiod
        if ( year0 .ne. year1 )
     &       fac = 1. - fldsecs/(secondsInYear-(count0-1)*fldperiod)

      endif
c     if (usefldyearlyfields)

c     Set switch for reading new record.
      if ( fldsecs - modelstep .lt. 0. ) changed = .true.

#ifdef EXF_VERBOSE
c     Do some printing for the protocol.
      _BEGIN_MASTER( mythid )
        write(msgbuf,'(a)') ' exf_GetFFieldsRec:'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,2x,l2,2x,l2,2x,D15.8)')
     &    ' exf_GetFFieldsRec: first, changed, fac:',
     &                         first, changed, fac
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,3(1x,i6))')
     &    ' exf_GetFFieldsRec: myiter, count0, count1:',
     &                         myiter, count0, count1
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' exf_GetFFieldsRec:'
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
      _END_MASTER( mythid )
#endif

      end
