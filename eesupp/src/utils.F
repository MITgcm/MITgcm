C $Header: /u/gcmpack/MITgcm/eesupp/src/utils.F,v 1.3 1998/05/21 18:30:08 cnh Exp $

#include "CPP_EEOPTIONS.h"

C--   File utils.F: General purpose support routines
C--    Contents
C-- U DATE            - Returns date and time.
C--   IFNBLNK         - Returns index of first non-blank string character.
C--   ILNBLNK         - Returns index of last non-blank string character.
C--   IO_ERRCOUNT     - Reads IO error counter.
C--   LCASE           - Translates to lower case.
C--UM MACHINE         - Returns character string identifying computer.
C--   TIMER_INDEX     - Returns index associated with timer name.
C-- M TIMER_CONTROL   - Implements timer functions for given machine.
C--   TIMER_PRINT     - Print CPU timer statitics.
C--   TIMER_PRINTALL  - Prints all CPU timers statistics.
C--   TIMER_START     - Starts CPU timer for code section.
C--   TIMER_STOP      - Stop CPU tier for code section.
C--   UCASE           - Translates to upper case.
C--   Routines marked "M" contain specific machine dependent code.
C--   Routines marked "U" contain UNIX OS calls.

CStartOfInterface
      SUBROUTINE DATE ( string , myThreadId )
C     /==========================================================\
C     | SUBROUTINE DATE                                          |
C     | o Return current date                                    |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
C
      CHARACTER*(*) string
      INTEGER myThreadId
CEndOfInterface
C
      INTEGER lDate
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
C
      lDate = 24
      IF ( LEN(string) .LT. lDate ) GOTO 901
      string = ' '
      CALL FDATE( string )
C   
 1000 CONTINUE
      RETURN
  901 CONTINUE
      WRITE(msgBuffer,'(A)')
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'procedure: "DATE".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'Variable passed to S/R DATE is too small.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &' Argument must be at least',lDate,'characters long.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,'(A)')
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
      END

CStartOfInterface
      INTEGER FUNCTION IFNBLNK( string )
C     /==========================================================\
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     \==========================================================/
C
      CHARACTER*(*) string
CEndOfInterface
C
      INTEGER L, LS
C
      LS     = LEN(string)
      IFNBLNK = 0
      DO 10 L = 1, LS
       IF ( string(L:L) .EQ. ' ' ) GOTO 10
        IFNBLNK = L
        GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

CStartOfInterface
      INTEGER FUNCTION ILNBLNK( string )
C     /==========================================================\
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     \==========================================================/
      CHARACTER*(*) string
CEndOfInterface
      INTEGER L, LS
C
      LS      = LEN(string)
      ILNBLNK = LS
      DO 10 L = LS, 1, -1
        IF ( string(L:L) .EQ. ' ' ) GOTO 10
         ILNBLNK = L
         GOTO 11
   10 CONTINUE
   11 CONTINUE
C
      RETURN
      END

CStartofinterface
      INTEGER FUNCTION IO_ERRCOUNT(myThid)
C     /==========================================================\
C     | FUNCTION IO_ERRCOUNT                                     |
C     | o Reads IO error counter.                                |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DFILE.h"

C     == Routine arguments ==
      INTEGER myThid
CEndofinterface

      IO_ERRCOUNT = ioErrorCount(myThid)

      RETURN
      END

CStartOfInterface
      SUBROUTINE LCASE ( string )
C     /==========================================================\
C     | SUBROUTINE LCASE                                         |
C     | o Convert character string to all lower case.            |
C     \==========================================================/
      CHARACTER*(*) string
CEndOfInterface
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
C
      DO 10 I = 1, LEN(string)
        L = INDEX(UPPER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
         string(I:I) = LOWER(L:L)
   10 CONTINUE
C   
      RETURN
      END

CStartOfInterface
      SUBROUTINE MACHINE ( string )
C     /==========================================================\
C     | SUBROUTINE MACHINE                                       |
C     | o Return computer identifier in string.                  |
C     \==========================================================/
#include "SIZE.h"
#include "EEPARAMS.h"
      CHARACTER*(*) string
CEndOfInterface
C
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK
C
      INTEGER  iFirst
      INTEGER  iLast 
      INTEGER  iEnd  
      INTEGER  iFree
      INTEGER  idSize
      CHARACTER*1024 strTmp
      CHARACTER*1024 idString

      strTmp = 'UNKNOWN'
      iFree  = 1
      idSize = LEN(string)
      CALL GETENV('USER',strTmp  )
      IF ( strTmp .NE. ' ' ) THEN
        iFirst = IFNBLNK(strTmp)
        iLast  = ILNBLNK(strTmp)
        iEnd   = iLast-iFirst+1
        IF (iEnd .GE. 0 ) THEN
         idString(iFree:) = strTmp(iFirst:iFirst+iEnd)
        ENDIF
        iFree = iFree+iEnd+1
        IF ( iFree .LE. idSize ) THEN
          idString(iFree:iFree) = '@'
          iFree = iFree+1
        ENDIF
      ENDIF
      strTmp = 'UNKNOWN'
      CALL GETENV('HOST',strtmp )
      IF ( strTmp .NE. ' ' ) THEN
        iFirst = IFNBLNK(strTmp)
        iLast  = ILNBLNK(strTmp)
        iEnd   = iLast-iFirst+1
        iEnd   = MIN(iEnd,idSize-iFree)
        iEnd   = iEnd-1
        IF (iEnd .GE. 0 ) THEN
          idString(iFree:) = strTmp(iFirst:iFirst+iEnd)
        ENDIF
        iFree = iFree+iEnd+1
      ENDIF
C
      string = idString
C
 1000 CONTINUE
      RETURN
      END

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
      CHARACTER*(maxString) timerAction
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
      iTimer=TIMER_INDEX(tmpName,timerNames(myThreadId,1),maxTimers,nTimers(myThreadId))
C
      IF     ( tmpAction .EQ. START ) THEN
        IF ( iTimer .EQ. 0 ) THEN
          IF ( nTimers(myThreadId) .EQ. maxTimers ) GOTO 904
            nTimers(myThreadId) = nTimers(myThreadId) + 1
            iTimer  = nTimers(myThreadId)
            timerNames(iTimer,myThreadId)    = tmpName
            timerNameLen(iTimer,myThreadId)  = ILNBLNK(tmpName)-IFNBLNK(tmpName)+1
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
          timerStarts(iTimer,myThreadId) = timerStarts(iTimer,myThreadId)+1
        ENDIF
      ELSEIF ( tmpAction .EQ. STOP ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        IF ( timerStatus(iTimer,myThreadId) .EQ. RUNNING ) THEN
          CALL TIMER_GET_TIME( userTime, systemTime, wallClockTime )
          timerUser(iTimer,myThreadId)    = timerUser(iTimer,myThreadId) + 
     &                           userTime          -
     &                           timerT0User(iTimer,myThreadId)
          timerSys (iTimer,myThreadId)    = timerSys(iTimer,myThreadId) + 
     &                           systemTime -
     &                           timerT0Sys(iTimer,myThreadId)
          timerWall(iTimer,myThreadId)    = timerWall(iTimer,myThreadId) + 
     &                           wallClockTime -
     &                           timerT0Wall(iTimer,myThreadId)
          timerStatus(iTimer,myThreadId)  = STOPPED
          timerStops (iTimer,myThreadId)  = timerStops (iTimer,myThreadId)+1
        ENDIF
      ELSEIF ( tmpAction .EQ. PRINT ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        WRITE(msgBuffer,*)
     &  ' Seconds in section "',
     &  timerNames(iTimer,myThreadId)(1:timerNameLen(iTimer,myThreadId)),'":'
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '         User time:',timerUser(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '       System time:',timerSys(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '   Wall clock time:',timerWall(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '        No. starts:',timerStarts(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
        WRITE(msgBuffer,*) '         No. stops:',timerStops(iTimer,myThreadId)
        CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      ELSEIF ( tmpAction .EQ. PRINTALL ) THEN
        DO 10 I = 1, nTimers(myThreadId)
         WRITE(msgBuffer,*) ' Seconds in section "',
     &            timerNames(I,myThreadId)(1:timerNameLen(I,myThreadId)),'":'
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '         User time:',timerUser(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '       System time:',timerSys(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '   Wall clock time:',timerWall(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '        No. starts:',timerStarts(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
         WRITE(msgBuffer,*) '         No. stops:',timerStops(I,myThreadId)
         CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
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
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name "',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Names must have fewer than',maxString+1,' characters.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  902 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' No timer action specified.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  903 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer action"',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  904 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer "',name(ISTART:IEND),'" cannot be created.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Only ',maxTimers,' timers are allowed.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  905 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name is blank.'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' A name must be used with "START", "STOP" or  "PRINT".'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,SQUEEZE_RIGHT,myThreadId)
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
      Real*8 userTime
      Real*8 systemTime
      Real*8 wallClockTime
CEndOfInterface
      Real*4 ETIME, ACTUAL, TARRAY(2)
      EXTERNAL ETIME
      Real*8 wtime
      Real*8 MPI_Wtime
      EXTERNAL MPI_Wtime

      ACTUAL = ETIME(TARRAY)
    
      userTime      = TARRAY(1)
      systemTime    = TARRAY(2)
#ifdef ALLOW_USE_MPI
      wtime         = MPI_Wtime()
      WRITE(0,*) ' Wtime = ', wtime
      wallClockTime = wtime
      WRITE(0,*) ' WallClocktime = ', wallClockTime
#endif /* ALLOW_USE_MPI */
#ifndef ALLOW_USE_MPI
      wallClockTime = 0.
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
      INTEGER myThreadId
CEndOfInterface
C     Print out value for every timer.
C
      CALL TIMER_CONTROL( ' ', 'PRINTALL', 'TIMER_PRINTALL' , myThreadId )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_START ( string , myThreadId )
C     Return start timer named "string".
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
      CHARACTER*(*) string
      INTEGER myThreadId
C
      CALL TIMER_CONTROL( string, 'STOP', 'TIMER_STOP' , myThreadId )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE UCASE ( string )
C     Translate string to upper case.
      CHARACTER*(*) string
      CHARACTER*26  LOWER
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
      SAVE LOWER
      CHARACTER*26  UPPER
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      SAVE UPPER
      INTEGER   I, L
C
      DO 10 I = 1, LEN(string)
        L = INDEX(LOWER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
          string(I:I) = UPPER(L:L)
   10 CONTINUE
C   
      RETURN
      END
C************************************************************************
