C $Header: /u/gcmpack/MITgcm/eesupp/src/timers.F,v 1.9 2001/02/04 14:38:44 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C--   File utils.F: General purpose support routines
C--    Contents
C--   TIMER_INDEX     - Returns index associated with timer name.
C-- M TIMER_CONTROL   - Implements timer functions for given machine.
C--   TIMER_PRINT     - Print CPU timer statitics.
C--   TIMER_PRINTALL  - Prints all CPU timers statistics.
C--   TIMER_START     - Starts CPU timer for code section.
C--   TIMER_STOP      - Stop CPU tier for code section.
C--   Routines marked "M" contain specific machine dependent code.
C--   Routines marked "U" contain UNIX OS calls.

CStartOfInterface
      INTEGER FUNCTION TIMER_INDEX ( 
     I        name,timerNames,maxTimers,nTimers )
C     /==========================================================\
C     | FUNCTION TIMER_INDEX                                     |
C     | o Timing support routine.                                |
C     |==========================================================|
C     | Return index in timer data structure of timer named      |
C     | by the function argument "name".                         |
C     \==========================================================/
      IMPLICIT NONE
      INTEGER maxTimers
      INTEGER nTimers
      CHARACTER*(*) name
      CHARACTER*(*) timerNames(maxTimers)
CEndOfInterface
      INTEGER I
C
      TIMER_INDEX = 0
      IF ( name .EQ. ' ' ) THEN
        TIMER_INDEX = -1
      ELSE
        DO 10 I = 1, nTimers
          IF ( name .NE. timerNames(I) ) GOTO 10
            TIMER_INDEX = I
            GOTO 11
   10   CONTINUE
   11   CONTINUE
      ENDIF
      RETURN
      END

CStartOfInterface
      SUBROUTINE TIMER_CONTROL ( name , action , callProc , myThreadId )
C     /==========================================================\
C     | SUBROUTINE TIMER_CONTROL                                 |
C     | o Timing routine.                                        |
C     |==========================================================|
C     | User callable interface to timing routines. Timers are   |
C     | created, stopped, started and queried only through this  |
C     | rtouine.                                                 |
C     \==========================================================/
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
      CHARACTER*(*) name
      CHARACTER*(*) action
      CHARACTER*(*) callProc
      INTEGER myThreadId
CEndOfInterface
C
      INTEGER  TIMER_INDEX
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL TIMER_INDEX
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK
C
      INTEGER maxTimers
      INTEGER maxString
      PARAMETER ( maxTimers = 40 )
      PARAMETER ( maxString = 80 )
C
      INTEGER timerStarts( maxTimers , MAX_NO_THREADS)
      SAVE    timerStarts
      INTEGER timerStops ( maxTimers , MAX_NO_THREADS)
      SAVE    timerStops 
      Real*8 timerUser  ( maxTimers , MAX_NO_THREADS)
      SAVE timerUser 
      Real*8 timerWall  ( maxTimers , MAX_NO_THREADS)
      SAVE timerWall 
      Real*8 timerSys   ( maxTimers , MAX_NO_THREADS)
      SAVE timerSys    
      Real*8 timerT0User( maxTimers , MAX_NO_THREADS)
      SAVE timerT0User   
      Real*8 timerT0Wall( maxTimers , MAX_NO_THREADS)
      SAVE timerT0Wall 
      Real*8 timerT0Sys ( maxTimers , MAX_NO_THREADS)
      SAVE timerT0Sys 
C     ===============================================================
C
      INTEGER timerStatus( maxTimers , MAX_NO_THREADS)
      SAVE    timerStatus
      INTEGER timerNameLen( maxTimers , MAX_NO_THREADS)
      SAVE    timerNameLen
      CHARACTER*(maxString) timerNames( maxTimers , MAX_NO_THREADS)
      SAVE                  timerNames
      INTEGER nTimers(MAX_NO_THREADS)
      CHARACTER*(maxString) tmpName
      CHARACTER*(maxString) tmpAction
      INTEGER iTimer
      INTEGER ISTART
      INTEGER IEND
      INTEGER STOPPED
      PARAMETER ( STOPPED = 0 )
      INTEGER RUNNING
      PARAMETER ( RUNNING = 1 )
      CHARACTER*(*) STOP
      PARAMETER ( STOP = 'STOP' )
      CHARACTER*(*) START
      PARAMETER ( START = 'START' )
      CHARACTER*(*) PRINT
      PARAMETER ( PRINT = 'PRINT' )
      CHARACTER*(*) PRINTALL
      PARAMETER ( PRINTALL = 'PRINTALL' )
      INTEGER I
      Real*8 userTime
      Real*8 systemTime
      Real*8 wallClockTime
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
C
      DATA nTimers  /MAX_NO_THREADS*0/
      SAVE nTimers
C
      ISTART = IFNBLNK(name)
      IEND   = ILNBLNK(name)
      IF ( IEND - ISTART + 1 .GT. maxString ) GOTO 901
      IF ( ISTART .NE. 0 ) THEN
        tmpName = name(ISTART:IEND)
        CALL UCASE( tmpName )
      ELSE
        tmpName = ' '
      ENDIF
      ISTART = IFNBLNK(action)
      IEND   = ILNBLNK(action)
      IF ( ISTART            .EQ. 0         ) GOTO 902
      IF ( IEND - ISTART + 1 .GT. maxString ) GOTO 903
      tmpAction = action(ISTART:IEND)
      CALL UCASE( tmpAction )
C
      iTimer=TIMER_INDEX(tmpName,timerNames(1,myThreadId),
     &       maxTimers,nTimers(myThreadId))
C
      IF     ( tmpAction .EQ. START ) THEN
        IF ( iTimer .EQ. 0 ) THEN
          IF ( nTimers(myThreadId) .EQ. maxTimers ) GOTO 904
            nTimers(myThreadId) = nTimers(myThreadId) + 1
            iTimer  = nTimers(myThreadId)
            timerNames(iTimer,myThreadId)    = tmpName
            timerNameLen(iTimer,myThreadId)  = 
     &       ILNBLNK(tmpName)-IFNBLNK(tmpName)+1
            timerUser(iTimer,myThreadId)     = 0.
            timerSys (iTimer,myThreadId)     = 0.
            timerWall(iTimer,myThreadId)     = 0.
            timerStarts(iTimer,myThreadId)   = 0
            timerStops (iTimer,myThreadId)   = 0
            timerStatus(iTimer,myThreadId)   = STOPPED
        ENDIF
        IF ( timerStatus(iTimer,myThreadId) .NE. RUNNING ) THEN
          CALL TIMER_GET_TIME( userTime, systemTime, wallClockTime )
          timerT0User(iTimer,myThreadId) = userTime
          timerT0Sys(iTimer,myThreadId)  = systemTime
          timerT0Wall(iTimer,myThreadId) = wallClockTime
          timerStatus(iTimer,myThreadId) = RUNNING
          timerStarts(iTimer,myThreadId) = 
     &       timerStarts(iTimer,myThreadId)+1
        ENDIF
      ELSEIF ( tmpAction .EQ. STOP ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        IF ( timerStatus(iTimer,myThreadId) .EQ. RUNNING ) THEN
          CALL TIMER_GET_TIME( userTime, systemTime, wallClockTime )
          timerUser(iTimer,myThreadId)    = 
     &       timerUser(iTimer,myThreadId) + 
     &                           userTime          -
     &                           timerT0User(iTimer,myThreadId)
          timerSys (iTimer,myThreadId)    = 
     &       timerSys(iTimer,myThreadId) + 
     &                           systemTime -
     &                           timerT0Sys(iTimer,myThreadId)
          timerWall(iTimer,myThreadId)    = 
     &       timerWall(iTimer,myThreadId) + 
     &                           wallClockTime -
     &                           timerT0Wall(iTimer,myThreadId)
          timerStatus(iTimer,myThreadId)  = STOPPED
          timerStops (iTimer,myThreadId)  = 
     &    timerStops (iTimer,myThreadId)+1
        ENDIF
      ELSEIF ( tmpAction .EQ. PRINT ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        WRITE(msgBuffer,*)
     &  ' Seconds in section "',
     &  timerNames(iTimer,myThreadId)(1:timerNameLen(iTimer,myThreadId))
     &  ,'":'
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '         User time:',
     &  timerUser(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '       System time:',
     &  timerSys(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '   Wall clock time:',
     &  timerWall(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '        No. starts:',
     &  timerStarts(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '         No. stops:',
     &  timerStops(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      ELSEIF ( tmpAction .EQ. PRINTALL ) THEN
        DO 10 I = 1, nTimers(myThreadId)
         WRITE(msgBuffer,*) ' Seconds in section "',
     &            timerNames(I,myThreadId)(1:timerNameLen(I,myThreadId))
     &  ,'":'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '         User time:',
     &  timerUser(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '       System time:',
     &  timerSys(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '   Wall clock time:',
     &  timerWall(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '        No. starts:',
     &  timerStarts(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '         No. stops:',
     &  timerStops(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
   10   CONTINUE
      ELSE
        GOTO 903
      ENDIF
C
 1000 CONTINUE
C
      RETURN
  901 CONTINUE
      WRITE(msgBuffer,'(A)')
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name "',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Names must have fewer than',maxString+1,' characters.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  902 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' No timer action specified.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  903 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer action"',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  904 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer "',name(ISTART:IEND),'" cannot be created.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Only ',maxTimers,' timers are allowed.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  905 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name is blank.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' A name must be used with "START", "STOP" or  "PRINT".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
      END

CStartOfInterface
      SUBROUTINE TIMER_GET_TIME( 
     O                           userTime, 
     O                           systemTime, 
     O                           wallClockTime )
C     /==========================================================\
C     | SUBROUTINE TIMER_GET_TIME                                |
C     | o Query system timer routines.                           |
C     |==========================================================|
C     | Routine returns total elapsed time for program so far.   |
C     | Three times are returned that conventionally are used as |
C     | user time, system time and wall-clock time. Not all these|
C     | numbers are available on all machines.                   |
C     \==========================================================/
      IMPLICIT NONE
      Real*8 userTime
      Real*8 systemTime
      Real*8 wallClockTime
CEndOfInterface
      Real*4 ETIME, ACTUAL, TARRAY(2)
#ifndef TARGET_T3E
#ifndef TARGET_CRAY_VECTOR
      EXTERNAL ETIME
#endif
#endif
      Real*8 wtime
C     Real*8 MPI_Wtime
C     EXTERNAL MPI_Wtime

#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
      userTime = 0.
      systemTime = 0.
#else
      ACTUAL = ETIME(TARRAY)
      userTime      = TARRAY(1)
      systemTime    = TARRAY(2)
#endif

#if (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
      wallClockTime = 0.
#else
      CALL CLOC(wTime)
      wallClockTime = wtime
#endif

#ifdef ALLOW_USE_MPI
C     wtime         = MPI_Wtime()
C     wallClockTime = wtime
#endif /* ALLOW_USE_MPI */
#ifndef ALLOW_USE_MPI
C     wallClockTime = 0.
#endif
      
      RETURN
      END

CStartOfInterface
      SUBROUTINE TIMER_PRINTALL( myThreadId )
C     /==========================================================\
C     | SUBROUTINE TIMER_PRINTALL                                |
C     | o Print timer information                                |
C     |==========================================================|
C     | Request print out of table of timing from all timers.    |
C     \==========================================================/
      IMPLICIT NONE
      INTEGER myThreadId
CEndOfInterface
C     Print out value for every timer.
C
      CALL TIMER_CONTROL( ' ', 'PRINTALL', 'TIMER_PRINTALL' , 
     &                   myThreadId )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_START ( string , myThreadId )
C     Return start timer named "string".
      IMPLICIT NONE
      CHARACTER*(*) string
      INTEGER myThreadId
C
      CALL TIMER_CONTROL( string, 'START', 'TIMER_START' , myThreadId)
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_STOP  ( string , myThreadId)
C     Return start timer named "string".
      IMPLICIT NONE
      CHARACTER*(*) string
      INTEGER myThreadId
C
      CALL TIMER_CONTROL( string, 'STOP', 'TIMER_STOP' , myThreadId )
C
      RETURN
      END
C***********************************************************************
