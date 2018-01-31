#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_GetFFieldRec
C     !INTERFACE:
      SUBROUTINE EXF_GetFFieldRec(
     I               fldStartTime, fldPeriod, fldRepeatCycle,
     I               fldName, usefldyearlyfields,
     O               fac, first, changed,
     O               count0, count1, year0, year1,
     I               myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_GetFFieldRec
C     | o Get flags, counters, and the linear interpolation
C     |   factor for a given field.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif

C     !INPUT PARAMETERS:
C     fldStartTime       :: time in seconds of first fld record from the
C                           beginning of the model integration or, if
C                           usefldyearlyfields, from the beginning of year
C     fldPeriod          :: period between forcing field records
C     fldRepeatCycle     :: time duration of a repeating cycle
C     fldName            :: field short name (to print mesg)
C     usefldyearlyfields :: when set, use yearly forcing files
C     myTime             :: current time in simulation
C     myIter             :: current iteration number in simulation
C     myThid             :: my thread identification number
      _RL     fldStartTime, fldPeriod, fldRepeatCycle
      CHARACTER*(*) fldName
      LOGICAL usefldyearlyfields
      _RL     myTime
      INTEGER myIter, myThid

C     !OUTPUT PARAMETERS:
C     fac     :: weight of record count0 for linear interpolation purposes
C     first   :: model initialization flag: read two forcing records
C     changed :: flag indicating that a new forcing record must be read
C     count0  :: record number for forcing field preceding myTime
C     count1  :: record number for forcing field following myTime
C     year0   :: year of forcing file for record preceding myTime
C     year1   :: year of forcing file for record following myTime
      _RL     fac
      LOGICAL first, changed
      INTEGER count0, count1, year0, year1

C     !FUNCTIONS:
#ifdef ALLOW_CAL
      INTEGER  cal_IsLeap
      EXTERNAL cal_IsLeap
#endif

C     !LOCAL VARIABLES:
C     mydate        :: model date of current time step
C     yearStartDate :: start of year date for flux record just before mydate
C     difftime      :: time difference between yearStartDate and mydate
C     fldsectot     :: time in seconds from fldStartTime to mydate
C     fldsecs       :: time from start of current forcing period to mydate
C     fldsecs0      :: time from start of repeat period to mydate
C     fldsecs1      :: time from end of current forcing period to mydate
C     secondsInYear :: seconds in the flux year just before mydate
C     myDateSeconds :: seconds from beginning of year to mydate
#ifdef ALLOW_CAL
      INTEGER mydate(4)
      INTEGER yearStartDate(4)
      INTEGER difftime(4)
      _RL fldsectot, fldsecs, fldsecs0, fldsecs1
      _RL secondsInYear, myDateSeconds
#endif
       INTEGER intimeP, intime0, intime1
       _RL locTime, aWght, bWght
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_CAL
      IF ( useCAL ) THEN

C     Set some default values.
       first = ((myTime - modelstart) .lt. 0.5*modelstep)
       changed = .FALSE.

       if ( fldPeriod .eq. 0. _d 0 ) then
C     Read field only once in the beginning. Hack: count1=count0 causes
C     the model to read the first record twice, but since this this is
C     done only the first time around it is not too much of an overhead.
        first   = ((myTime - modelstart) .lt. 0.5*modelstep)
        changed = .FALSE.
        fac     = 1. _d 0
        count0  = 1
        count1  = count0
C     Give these variables some unproblematic values although they are
C     never used in this context.
        year0   = 0
        year1   = year0
       else
C       fldPeriod .ne. 0
        if (.not.usefldyearlyfields) then

C     Determine offset in seconds from beginning of input data
C     to current date.
         fldsectot = myTime - fldStartTime

C     Determine the flux records just before and after mycurrentdate.
         if ( fldRepeatCycle .eq. 0. _d 0 ) then

          if ( fldsectot .lt. 0. _d 0 ) then
           WRITE(msgBuf,'(4A,1P1E17.10,A)') 'EXF_GetFFieldRec ',
     &       'for field "', fldName, '": myTime=', myTime, ' earlier'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(2A,1P1E18.10,A)') 'EXF_GetFFieldRec: ',
     &       'than 1rst reccord (field-startdate=', fldStartTime, ')'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R EXF_GetFFieldRec'
          endif
          count0       = INT((fldsectot+0.5)/fldPeriod) + 1
          count1       = count0 + 1
          fldsecs      = MOD(fldsectot,fldPeriod)

         else
C        if ( fldRepeatCycle .gt. 0. )

C     If using repeating data then make fldsectot cycle around.
          if (fldsectot.lt.0. _d 0)
     &        fldsectot = fldsectot + fldRepeatCycle
          fldsecs0     = MOD(fldsectot,fldRepeatCycle)
          count0       = INT((fldsecs0+0.5)/fldPeriod) + 1
          fldsecs1     = MOD(fldsectot+fldPeriod,fldRepeatCycle)
          count1       = INT((fldsecs1+0.5)/fldPeriod) + 1
          fldsecs      = MOD(fldsecs0,fldPeriod)

         endif

C     Weight belonging to count0 for linear interpolation purposes.
         fac = 1. - fldsecs/fldPeriod

        else
C       if (usefldyearlyfields)

C     Determine seconds from beginning of year to model current time.
         CALL cal_GetDate( myIter, myTime, mydate, myThid )
         year0            = INT(mydate(1)/10000.)
         yearStartDate(1) = year0 * 10000 + 101
         yearStartDate(2) = 0
         yearStartDate(3) = mydate(3)
         yearStartDate(4) = mydate(4)
         CALL cal_TimePassed(yearStartDate,mydate,difftime,myThid)
         CALL cal_ToSeconds (difftime,myDateSeconds,myThid)

C     Determine the flux year just before mycurrentdate.
         if ( myDateSeconds .lt. fldStartTime ) year0 = year0 - 1

C     Determine seconds in the flux year just before mycurrentdate.
         secondsInYear = ndaysnoleap * secondsperday
         if ( cal_IsLeap(year0,myThid) .eq. 2)
     &       secondsInYear = ndaysleap * secondsperday

C     Determine the record just before mycurrentdate.
         if ( myDateSeconds .lt. fldStartTime )
     &       myDateSeconds = myDateSeconds + secondsInYear
         fldsectot = myDateSeconds - fldStartTime
         count0    = INT((fldsectot+0.5)/fldPeriod) + 1

C     Determine the flux year and record just after mycurrentdate.
         year1  = year0
         count1 = count0 + 1
         if ( (fldStartTime+count0*fldPeriod) .ge. secondsInYear ) then
          year1  = year0 + 1
          count1 = 1
         endif

C     Weight belonging to count0 for linear interpolation purposes.
         fldsecs = MOD(fldsectot,fldPeriod)
         fac     = 1. - fldsecs/fldPeriod
         if ( year0 .ne. year1 )
     &       fac = 1. - fldsecs/(secondsInYear-(count0-1)*fldPeriod)

       endif
C      if (usefldyearlyfields)

C     Set switch for reading new record.
       if ( fldsecs - modelstep .lt. 0. _d 0 ) changed = .TRUE.

       endif
C      if (fldPeriod .eq. 0.)

      ELSE
#else /* ALLOW_CAL */
      IF ( .TRUE. ) THEN
#endif /* ALLOW_CAL */

       year0 = 0
       year1 = 0

       IF ( fldPeriod .EQ. 0. _d 0 ) THEN
        fac     = 1. _d 0
        first   = ( myIter.EQ.nIter0 )
        changed = .FALSE.
C     Read field only once in the beginning. Hack: count1=count0 causes
C     the model to read the first record twice, but since this this is
C     done only the first time around it is not too much of an overhead.
        count0  = 1
        count1  = 1

       ELSE
        locTime = myTime - fldStartTime + fldPeriod*halfRL
        CALL GET_PERIODIC_INTERVAL(
     O                  intimeP, intime0, intime1, bWght, aWght,
     I                  fldRepeatCycle, fldPeriod,
     I                  deltaTClock, locTime, myThid )

C      Fld @ t = bWght*Fld(intime0) + aWght*Fld(intime1)
        fac     =  bWght
        first   = ( myIter .EQ.nIter0 )
        changed = ( intime0.NE.intimeP )
        count0  = intime0
        count1  = intime1

        IF ( intime0 .LE. 0 ) THEN
           WRITE(msgBuf,'(3A,I10,A,1P1E17.10)')
     &       'EXF_GetFFieldRec: for field "', fldName,
     &       '" @ Iter=', myIter, ' , myTime=', myTime
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2(A,1P1E18.10))')
     &       'EXF_GetFFieldRec:  fldRepeatCycle=', fldRepeatCycle,
     &                            ' , fldPeriod=', fldPeriod
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(3(A,I8))') 'EXF_GetFFieldRec:  intimeP=',
     &       intimeP, ', intime0=', intime0, ', intime1=', intime1
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2(A,1P1E14.7),A,1P1E16.9)')
     &      'EXF_GetFFieldRec:  bWght,aWght=', bWght, ',', aWght,
     &      ' @ locTime=', locTime
           CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
           WRITE(msgBuf,'(2A)') 'EXF_GetFFieldRec: ',
     &        'Reccord number "intime0" not valid ; possible cause:'
           CALL PRINT_ERROR( msgBuf, myThid )
           WRITE(msgBuf,'(2A,1P2E18.10)') 'EXF_GetFFieldRec: ',
     &        ' myTime earlier than field-StartTime=', fldStartTime
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R EXF_GetFFieldRec'
        ENDIF

C-     end if fldPeriod=0
       ENDIF

C--   end if/else useCAL
      ENDIF

      RETURN
      END
