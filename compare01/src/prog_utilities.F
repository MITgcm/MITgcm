C $Id: prog_utilities.F,v 1.1 1998/05/25 20:21:05 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C     General purpose routines used performing miscellaneous non-numerical
C     tasks.
C     Routines:
C  U  CURDATE            - Returns date and time.
C     DIFFERENT_MULTIPLE - Determines if arg2 is a different integer multiple of arg1 to
C                          arg3. Used to determine when to perform periodic operations.
C     IFNBLNK            - Returns -1 of first non-blank string character.
C     ILNBLNK            - Returns -1 of last non-blank string character.
C     LCASE              - Translates to lower case.
C  UM PLTFRM             - Returns character string identifying computer.
C     TIMER_INDEX        - Returns -1 associated with timer name.
C   M TIMER_CONTROL      - Implements timer functions for given machine.
C     TIMER_PRINT        - Print CPU timer statitics.
C     TIMER_PRINTALL     - Prints all CPU timers statistics.
C     TIMER_START        - Starts CPU timer for code section.
C     TIMER_STOP         - Stop CPU tier for code section.
C     UCASE              - Translates to upper case.
C     Routines marked "M" contain specific machine dependent code.
C     Routines marked "U" contain UNIX OS calls.
C***********************************************************************
      SUBROUTINE CURDATE ( string )
      IMPLICIT NONE
C     Return current date
      CHARACTER*(*) string
      CHARACTER*8 cdate
      CHARACTER*10 ctime
      CHARACTER*5 czone
      INTEGER values(8)
C
      INTEGER lDate, idate, itime
C
      lDate = 24
      IF ( LEN(string) .LT. lDate ) THEN
         PRINT *
         PRINT *, '*** WARNING WARNING WARNING WARNING WARNING ***'
         PRINT *, 'procedure: "CURDATE".'
         PRINT *, 'Variable passed to S/R CURDATE is too small.'
         PRINT *, ' Argument must be at least',lDate,'characters long.'
         PRINT *, '***************************************************'
      ELSE
         STRING = 'UNKNOWN'
      END IF
      RETURN
      END
C***********************************************************************
      LOGICAL FUNCTION DIFFERENT_MULTIPLE( freq, val1, val2 )
      IMPLICIT NONE
C     Returns TRUE if val1 and val2 are different multiples of freq.
      REAL freq, val1, val2
      IF ( freq .EQ. 0. ) THEN
        DIFFERENT_MULTIPLE = .FALSE.
      ELSE
        DIFFERENT_MULTIPLE = INT(val1/freq) .NE. INT(val2/freq)
      ENDIF
      END
      INTEGER FUNCTION IFNBLNK( string )
C
      CHARACTER*(*) string
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
C***********************************************************************
      INTEGER FUNCTION ILNBLNK( string )
      IMPLICIT NONE
C
      CHARACTER*(*) string
C
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
C***********************************************************************
      SUBROUTINE LCASE ( string )
      IMPLICIT NONE
C     Translate string to lower case.
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
        L = INDEX(UPPER,string(I:I))
        IF ( L .EQ. 0 ) GOTO 10
          string(I:I) = LOWER(L:L)
   10 CONTINUE
C   
      RETURN
      END
C***********************************************************************
      SUBROUTINE PLTFRM  ( string )
      IMPLICIT NONE
#include "EXTERNAL.h"
C     Return computer identifier in string.
      CHARACTER*(*) string
C
      INTEGER     L
      INTEGER     idSize
      PARAMETER ( idSize = 80 )
C
      L = LEN(string)
      IF ( L .LT. idSize ) THEN
        PRINT *, '                                        '
        PRINT *,'*** WARNING WARNING WARNING WARNING WARNING ***'
        PRINT *, 'procedure: "PLTFRM ".'
        PRINT *, 'Variable passed to S/R PLTFRM  is too small.'
        PRINT *,'Argument must be at least',idSize,'characters long.'
        PRINT *,'***************************************************'
      ELSE
        string = 'Cray test version'
      ENDIF

      RETURN
      END
C***********************************************************************
      INTEGER FUNCTION TIMER_INDEX (
     I        name,timerNames,maxTimers,nTimers )
      IMPLICIT NONE
C     Return index in timer data structure of timer named "name".
      INTEGER maxTimers
      INTEGER nTimers
      CHARACTER*(*) name
      CHARACTER*(*) timerNames(maxTimers)
C
      INTEGER I
C
      TIMER_INDEX = 0
      IF ( name .EQ. ' ' ) THEN
         TIMER_INDEX = -1
      ELSE
         DO I = 1, nTimers
            IF ( name .EQ. timerNames(I) ) THEN
               TIMER_INDEX = I
               GOTO 11
            END IF
         END DO
 11      CONTINUE
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_CONTROL ( name , action , callProc )
      IMPLICIT NONE
#include "EXTERNAL.h"
C     Controls timer setting, starting, stopping and printing.
      CHARACTER*(*) name
      CHARACTER*(*) action
      CHARACTER*(*) callProc
C
      INTEGER maxTimers
      INTEGER maxString
      PARAMETER ( maxTimers = 40 )
      PARAMETER ( maxString = 80 )
C
C     ====== Machine specific variables ============================
      REAL timerBusy   ( maxTimers )
      SAVE timerBusy
      REAL timerElapsed( maxTimers )
      SAVE timerElapsed
      REAL timerT0Elapsed( maxTimers )
      SAVE timerT0Elapsed
      REAL timerT0Busy( maxTimers )
      SAVE timerT0Busy
C     ===============================================================
C
      INTEGER timerStatus( maxTimers )
      SAVE    timerStatus
      INTEGER timerNameLen( maxTimers )
      SAVE    timerNameLen
      CHARACTER*(maxString) timerNames( maxTimers )
      SAVE                  timerNames
      CHARACTER*(maxString) timerAction
      INTEGER nTimers
      CHARACTER*(maxString) tmpName
      CHARACTER*(maxString) tmpAction
      INTEGER iTimer
      INTEGER ISTART
      INTEGER IEND
      LOGICAL firstCall
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

      DATA nTimers  /0/
      DATA firstCall /.TRUE./
      SAVE nTimers
      SAVE firstCall

      IF ( firstCall ) THEN
CRG        CALL CM_TIMER_CLEAR(0)
      ENDIF
CRG      CALL CM_TIMER_STOP(0)
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
      iTimer=TIMER_INDEX(tmpName,timerNames,maxTimers,nTimers)
C
      IF     ( tmpAction .EQ. START ) THEN
        IF ( iTimer .EQ. 0 ) THEN
          IF ( nTimers .EQ. maxTimers ) GOTO 904
            nTimers = nTimers + 1
            iTimer  = nTimers
            timerNames(iTimer)    = tmpName
            timerNameLen(iTimer)  = ILNBLNK(tmpName)-IFNBLNK(tmpName)+1
            timerBusy(iTimer)     = 0.
            timerElapsed(iTimer)  = 0.
            timerStatus(iTimer)   = STOPPED
        ENDIF
        IF ( timerStatus(iTimer) .NE. RUNNING ) THEN
          timerT0Busy(iTimer)    = 0.
          timerT0Elapsed(iTimer) = 0.
          timerStatus(iTimer)    = RUNNING
        ENDIF
      ELSEIF ( tmpAction .EQ. STOP ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        IF ( timerStatus(iTimer) .EQ. RUNNING ) THEN
          timerBusy(iTimer)    = timerBusy(iTimer) +
     &                           0. -
     &                           timerT0Busy(iTimer)
          timerElapsed(iTimer) = timerElapsed(iTimer) +
     &                           0. -
     &                           timerT0Elapsed(iTimer)
          timerStatus(iTimer)  = STOPPED
        ENDIF
      ELSEIF ( tmpAction .EQ. PRINT ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
        PRINT *, ' Seconds in section "',
     &           timerNames(iTimer)(1:timerNameLen(iTimer)),'":'
        PRINT *, '      Elapsed time:',timerElapsed(iTimer)
        PRINT *, '         Busy time:',timerBusy(iTimer)
      ELSEIF ( tmpAction .EQ. PRINTALL ) THEN
        DO 10 I = 1, nTimers
          PRINT *, ' Seconds in section "',
     &             timerNames(I)(1:timerNameLen(I)),'":'
          PRINT *, '      Elapsed time:',timerElapsed(I)
          PRINT *, '         Busy time:',timerBusy(I)
   10   CONTINUE
      ELSE
        GOTO 903
      ENDIF
C
 1000 CONTINUE
      firstCall = .FALSE.
CRG      CALL CM_TIMER_START(0)
C
      RETURN
  901 CONTINUE
      PRINT *, '                                                       '
      PRINT *, '*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      PRINT *, 'procedure: "',callProc,'".'
      PRINT *, 'Timer name "',name(ISTART:IEND),'" is invalid.'
      PRINT *, ' Names must have fewer than',maxString+1,' characters.'
      PRINT *, '*******************************************************'
      GOTO 1000
  902 CONTINUE
      PRINT *, '                                                       '
      PRINT *, '*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      PRINT *, 'procedure: "',callProc,'".'
      PRINT *, ' No timer action specified.'
      PRINT *, ' Valid actions are:'
      PRINT *, ' "START", "STOP", "PRINT" and "PRINTALL".'
      PRINT *, '*******************************************************'
      GOTO 1000
  903 CONTINUE
      PRINT *, '                                                       '
      PRINT *, '*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      PRINT *, 'procedure: "',callProc,'".'
      PRINT *, 'Timer action"',name(ISTART:IEND),'" is invalid.'
      PRINT *, ' Valid actions are:'
      PRINT *, ' "START", "STOP", "PRINT" and "PRINTALL".'
      PRINT *, '*******************************************************'
      GOTO 1000
  904 CONTINUE
      PRINT *, '                                                       '
      PRINT *, '*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      PRINT *, 'procedure: "',callProc,'".'
      PRINT *, 'Timer "',name(ISTART:IEND),'" cannot be created.'
      PRINT *, ' Only ',maxTimers,' timers are allowed.'
      PRINT *, '*******************************************************'
      GOTO 1000
  905 CONTINUE
      PRINT *, '                                                       '
      PRINT *, '*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      PRINT *, 'procedure: "',callProc,'".'
      PRINT *, 'Timer name is blank.'
      PRINT *, ' A name must be used with "START", "STOP" or  "PRINT".'
      PRINT *, '*******************************************************'
      GOTO 1000
      END


C***********************************************************************
      SUBROUTINE TIMER_PRINTALL
      IMPLICIT NONE
C     Print out value for every timer.
C
      CALL TIMER_CONTROL( ' ', 'PRINTALL', 'TIMER_PRINTALL' )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_START ( string )
      IMPLICIT NONE
C     Return start timer named "string".
      CHARACTER*(*) string
C
      CALL TIMER_CONTROL( string, 'START', 'TIMER_START' )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE TIMER_STOP  ( string )
      IMPLICIT NONE
C     Return start timer named "string".
      CHARACTER*(*) string
C
      CALL TIMER_CONTROL( string, 'STOP', 'TIMER_STOP' )
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE UCASE ( string )
      IMPLICIT NONE
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
