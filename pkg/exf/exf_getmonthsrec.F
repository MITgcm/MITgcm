#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_GetMonthsRec
C     !INTERFACE:
      SUBROUTINE EXF_GetMonthsRec(
     I                             fldStartTime, useFldYearlyFields,
     O                             fac, first, changed,
     O                             count0, count1, year0, year1,
     I                             myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_GetMonthsRec
C     | o Get flags, counters, and the linear interpolation
C     |   factor for a given field for the case of monthly,
C     |   interannual forcing
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==

C     !INPUT PARAMETERS:
C     fldStartTime       :: time in seconds of first fld record from the
C                           beginning of the model integration or, if
C                           usefldyearlyfields, from the beginning of year
C     useFldYearlyFields :: when set, use yearly forcing files
C     myTime             :: current time in simulation
C     myIter             :: current iteration number in simulation
C     myThid             :: my thread identification number
      _RL     fldStartTime
      LOGICAL useFldYearlyFields
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

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

#ifdef ALLOW_EXF
#ifdef ALLOW_CAL
C     !LOCAL VARIABLES:
      INTEGER startDate(4)
      INTEGER yy, mm, dd, ss, lp, wd
CEOP

      CALL cal_GetMonthsRec(
     O                       fac, first, changed,
     O                       count0, count1, year0, year1,
     I                       myTime, myIter, myThid )
      IF (.NOT.useFldYearlyFields) THEN
        CALL CAL_GETDATE( 0, fldStartTime, startDate, myThid )
        CALL CAL_CONVDATE( startDate, yy, mm, dd, ss, lp, wd, myThid )
        count0 = (year0-yy)*12 + count0 - mm + 1
        count1 = (year1-yy)*12 + count1 - mm + 1
        year0 = 0
        year1 = 0
      ENDIF

#endif /* ALLOW_CAL */
#endif /* ALLOW_EXF */

      RETURN
      END
