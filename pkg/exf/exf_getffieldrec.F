C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getffieldrec.F,v 1.26 2014/06/07 17:50:34 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_GetFFieldRec(
     I                             fldstartdate, fldperiod,
     I                             usefldyearlyfields,
     O                             fac,
     O                             first, changed,
     O                             count0, count1, year0, year1,
     I                             myTime, myIter, myThid )

c     ==================================================================
c     o Get flags, counters, and the linear interpolation factor for a
c       given field.
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

c cal: modelstart, modelstep
#include "EEPARAMS.h"
#include "cal.h"
#include "EXF_PARAM.h"

c     == input arguments ==
c     fldstartdate       :: time in seconds of first fld record from the
c                           beginning of the model integration or, if
c                           usefldyearlyfields, from the beginning of year
c     fldperiod          :: period between forcing field records
c     usefldyearlyfields :: when set, use yearly forcing files
c     myTime             :: current time in simulation
c     myIter             :: current iteration number in simulation
c     myThid             :: my thread identification number

      _RL     fldstartdate, fldperiod
      logical usefldyearlyfields
      _RL     myTime
      INTEGER myIter, myThid

c     == output arguments ==
c     fac     :: weight of record count0 for linear interpolation purposes
c     first   :: model initialization flag: read two forcing records
c     changed :: flag indicating that a new forcing record must be read
c     count0  :: record number for forcing field preceding myTime
c     count1  :: record number for forcing field following myTime
c     year0   :: year of forcing file for record preceding myTime
c     year1   :: year of forcing file for record following myTime

      _RL     fac
      LOGICAL first, changed
      INTEGER count0, count1, year0, year1

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

      INTEGER mydate(4)
      INTEGER yearStartDate(4)
      INTEGER difftime(4)
      _RL fldsectot, fldsecs, fldsecs0, fldsecs1
      _RL secondsInYear, myDateSeconds

      CHARACTER*(max_len_mbuf) msgBuf

c     == external ==

      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap

c     == end of interface ==

c     Set some default values.
      first = ((myTime - modelstart) .lt. 0.5*modelstep)
      changed = .FALSE.

      if ( fldperiod .eq. 0. _d 0 ) then
c     Read field only once in the beginning. Hack: count1=count0 causes
c     the model to read the first record twice, but since this this is
c     done only the first time around it is not too much of an overhead.
       first   = ((myTime - modelstart) .lt. 0.5*modelstep)
       changed = .FALSE.
       fac     = 1. _d 0
       count0  = 1
       count1  = count0
c     Give these variables some unproblematic values although they are
c     never used in this context.
       year0   = 0
       year1   = year0
      else
c     fldperiod .ne. 0
      if (.not.usefldyearlyfields) then

c     Determine offset in seconds from beginning of input data
c     to current date.
       fldsectot = myTime - fldstartdate

c     Determine the flux records just before and after mycurrentdate.
       if ( repeatPeriod .eq. 0. _d 0 ) then

        if ( fldsectot .lt. 0. _d 0 ) then
         WRITE(msgBuf,'(2A)') 'EXF_GetFFieldRec: ',
     &        'myTime earlier than 1rst reccord (field-startdate)'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_GetFFieldRec'
        endif
        count0       = int((fldsectot+0.5)/fldperiod) + 1
        count1       = count0 + 1
        fldsecs      = mod(fldsectot,fldperiod)

       else
c     if ( repeatPeriod .gt. 0. )

c     If using repeating data then make fldsectot cycle around.
        if(fldsectot.lt.0. _d 0) fldsectot = fldsectot + repeatPeriod
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
       call cal_GetDate( myIter, myTime, mydate, myThid )
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
        secondsInYear = ndaysnoleap * secondsperday
        if ( cal_IsLeap(year0,myThid) .eq. 2)
     &       secondsInYear = ndaysleap * secondsperday

c     Determine the record just before mycurrentdate.
        if ( myDateSeconds .lt. fldstartdate )
     &       myDateSeconds = myDateSeconds + secondsInYear
        fldsectot = myDateSeconds - fldstartdate
        count0    = int((fldsectot+0.5)/fldperiod) + 1

c     Determine the flux year and record just after mycurrentdate.
        year1  = year0
        count1 = count0 + 1
        if ( (fldstartdate+count0*fldperiod) .ge. secondsInYear ) then
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
      if ( fldsecs - modelstep .lt. 0. _d 0 ) changed = .TRUE.

      endif
c     if (fldperiod .eq. 0.)

      RETURN
      END
