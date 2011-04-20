C $Header: /u/gcmpack/MITgcm/eesupp/src/get_periodic_interval.F,v 1.2 2011/04/20 01:36:09 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: GET_PERIODIC_INTERVAL

C     !INTERFACE:
      SUBROUTINE GET_PERIODIC_INTERVAL(
     O               tRec0, tRec1, tRec2, wght1, wght2,
     I               cycleLength, recSpacing, deltaT,
     I               currentTime, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GET\_PERIODIC\_INTERVAL
C     | o Provide time-record indices arround current time
C     |   from a periodic, regularly spaced, time sequence
C     | o Extended to non-periodic, regularly spaced, time
C     |   sequence (case cycleLength=0) as in pkg/rbcs
C     *==========================================================*
C     | From a regularly-spaced sequence of time records
C     | this routine returns the index of the two records
C     | surrounding the current time and the record index of
C     | corresponding to the previous time-step ; also provides
C     | the weighting factor for a linear interpolation
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     cycleLength :: length of the periodic cycle (in s), zero if non-periodic
C     recSpacing  :: time record spacing
C     deltaT      :: time-step
C     currentTime :: current time
C     myThid      :: my Thread Id number
      _RL      cycleLength, recSpacing, deltaT, currentTime
      INTEGER  myThid
C     !OUTPUT PARAMETERS:
C     tRec0       :: time-record intex corresponding to the previous time-step
C     tRec1       :: 1rst time-record intex just before current time
C     tRec2       ::  2nd time-record intex just after current time
C     wght1       :: linear interpolation weight (applies to 1rst record)
C     wght2       :: linear interpolation weight (applies to 2nd  record)
      INTEGER  tRec0, tRec1, tRec2
      _RL      wght1, wght2

C     !LOCAL VARIABLES:
C     == Local variables ==
C     nbRec       :: number of time-records
C     msgBuf      :: Informational/error message buffer
      INTEGER  nbRec
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL      locTime, modTime, tmpTime
CEOP

C     Implicit function:
      _RL F90MODULO, arg1, arg2
C statement function to emulate Fortran 90 MODULO
C this modulo has the same sign as arg2 (and absolute value < |arg2|)
      F90MODULO(arg1,arg2) = MOD(MOD(arg1,arg2)+arg2,arg2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      tRec0 = 0
      tRec1 = 0
      tRec2 = 0
      wght1 = 0.
      wght2 = 0.

      IF ( cycleLength.LT.0. .OR.
     &     recSpacing .LE.0. ) THEN
        IF ( cycleLength.LT.0. ) WRITE(msgBuf,'(A)')
     &      'GET_PERIODIC_INTERVAL requires cycleLength >= 0'
        IF ( recSpacing .LE.0. ) WRITE(msgBuf,'(A)')
     &      'GET_PERIODIC_INTERVAL requires recSpacing > 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,2(A,1PE16.8))') 'GET_PERIODIC_INTERVAL: ',
     &     'cycleLength=', cycleLength, ' , recSpacing=', recSpacing
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GET_PERIODIC_INTERVAL'
      ELSE
        nbRec = NINT(cycleLength/recSpacing)
      ENDIF
      tmpTime = nbRec*recSpacing
      IF ( cycleLength.NE.tmpTime ) THEN
        WRITE(msgBuf,'(2A,I5,A)') 'GET_PERIODIC_INTERVAL: ',
     &     'cycleLength not multiple of recSpacing:'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,2(A,1PE16.8))') 'GET_PERIODIC_INTERVAL: ',
     &     'cycleLength=', cycleLength, ' , recSpacing=', recSpacing
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GET_PERIODIC_INTERVAL'
      ENDIF

      IF ( cycleLength.EQ.0. _d 0 ) THEN
C--   Non-periodic time-record sequence:

        locTime = currentTime - recSpacing*0.5
        modTime = F90MODULO(locTime,recSpacing)

C-    time-record before (tRec1) and after (tRec2) current time:
        tRec1 = 1 + NINT( (locTime-modTime)/recSpacing )
        tRec2 = 1 + tRec1

C-    linear interpolation weights:
        wght2 = modTime / recSpacing
        wght1 = 1. _d 0 - wght2

C-    previous time-step record:
        locTime = locTime-deltaT
        modTime = F90MODULO( locTime, recSpacing )
        tRec0 = 1 + NINT( (locTime-modTime)/recSpacing )

      ELSE
C--   Periodic time-record sequence:

        locTime = currentTime - recSpacing*0.5
     &          + cycleLength*( 2 - NINT(currentTime/cycleLength) )

C-    time-record before (tRec1) and after (tRec2) current time:
        tmpTime = MOD( locTime, cycleLength )
        tRec1 = 1 + INT( tmpTime/recSpacing )
        tRec2 = 1 + MOD( tRec1, nbRec )

C-    linear interpolation weights:
        wght2 = ( tmpTime - recSpacing*(tRec1 - 1) )/recSpacing
        wght1 = 1. _d 0 - wght2

C-    previous time-step record:
        tmpTime = MOD( locTime-deltaT, cycleLength )
        tRec0 = 1 + INT(tmpTime/recSpacing)

      ENDIF

      RETURN
      END
