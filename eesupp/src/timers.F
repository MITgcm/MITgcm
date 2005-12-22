C $Header: /u/gcmpack/MITgcm/eesupp/src/timers.F,v 1.20 2005/12/22 01:03:29 ce107 Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#ifdef USE_LIBHPM
# include "f_hpm.h"
#endif

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

CGG   Modified following A. Biastoch for use with SP3. Is backwards
CGG   compatible. G. Gebbie, gebbie@mit.edu, 20 Oct 2001, Scripps.

CBOP
C     !ROUTINE: TIMER_INDEX

C     !INTERFACE:
      INTEGER FUNCTION TIMER_INDEX ( 
     I        name,timerNames,maxTimers,nTimers )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION TIMER\_INDEX                                      
C     | o Timing support routine.                                 
C     *==========================================================*
C     | Return index in timer data structure of timer named       
C     | by the function argument "name".                          
C     *==========================================================*

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguements ==
C     maxTimers  :: Total number of timers allowed
C     nTimers    :: Current number of timers
C     name       :: Name of timer to find
C     timerNames :: List of valid timer names
      INTEGER maxTimers
      INTEGER nTimers
      CHARACTER*(*) name
      CHARACTER*(*) timerNames(maxTimers)

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I :: Index variable
      INTEGER I
CEOP
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

CBOP
C     !ROUTINE: TIMER_CONTROL

C     !INTERFACE:
      SUBROUTINE TIMER_CONTROL ( name , action , callProc , myThreadId )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE TIMER\_CONTROL                                 |
C     | o Timing routine.                                        |
C     *==========================================================*
C     | User callable interface to timing routines. Timers are   |
C     | created, stopped, started and queried only through this  |
C     | rtouine.                                                 |
C     *==========================================================*

C     !USES:
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
      INTEGER  TIMER_INDEX
      INTEGER  IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL TIMER_INDEX
      EXTERNAL IFNBLNK
      EXTERNAL ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     name       :: name of the timer
C     action     :: operation to perform with this timer
C     callProc   :: procedure calling this routine
C     myThreadId :: instance number of this thread
      CHARACTER*(*) name
      CHARACTER*(*) action
      CHARACTER*(*) callProc
      INTEGER myThreadId
C
C     !LOCAL VARIABLES:
C     maxTimers :: Total numer of timer allowed
C     maxString :: Max length of a timer name
      INTEGER maxTimers
      INTEGER maxString
      PARAMETER ( maxTimers = 40 )
      PARAMETER ( maxString = 80 )
C     timerStarts :: Timer counters for each timer and each thread
C     timerStops 
C     timerUser
C     timerWall
C     timerSys
C     timerT0User
C     timerT0Wall
C     timerT0Sys 
C     timerStatus  :: START/STOP/RUNNING Status of the timer
C     timerNameLen :: Length of timer name
C     timerNames   :: Table of timer names
C     nTimers      :: Number of active timers
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
#ifdef USE_PAPI
#include <fpapi.h>
      INTEGER PAPIF_num_counters
      EXTERNAL PAPIF_num_counters
      CHARACTER*(*) INIT
      CHARACTER(13) EventName
      PARAMETER ( INIT = 'INIT' )
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 18)
      INTEGER EventCode(nmaxevents)
      INTEGER*8 values(nmaxevents, maxTimers , MAX_NO_THREADS),
     $     values1(nmaxevents, maxTimers, MAX_NO_THREADS),
     $     values2(nmaxevents, maxTimers, MAX_NO_THREADS)
      COMMON /papivalues/ values, values1, values2
      INTEGER neventsmax, nevents, Check, EventSet
      INTEGER papiunit
      SAVE EventCode, neventsmax, nevents, EventSet
#endif
      INTEGER I, J
      Real*8 userTime
      Real*8 systemTime
      Real*8 wallClockTime
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
      DATA nTimers  /MAX_NO_THREADS*0/
      SAVE nTimers
CEOP
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
#ifdef USE_PAPI
CCE107 - Read event counts
          call PAPIF_read(EventSet, values1(1,iTimer,myThreadId), Check)
#endif
        ENDIF
#ifdef USE_LIBHPM
        CALL f_hpmtstart((myThreadId-1)*100+iTimer,tmpName)
#endif
      ELSEIF ( tmpAction .EQ. STOP ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
#ifdef USE_LIBHPM
        CALL f_hpmtstop((myThreadId-1)*100+iTimer)
#endif
        IF ( timerStatus(iTimer,myThreadId) .EQ. RUNNING ) THEN
#ifdef USE_PAPI
CCE107 PAPI - Read event counts
          call PAPIF_read(EventSet, values2(1,iTimer,myThreadId), Check)
#endif
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
#ifdef USE_PAPI
          do i=1,nevents
             values(i,iTimer,myThreadId) = values(i,iTimer,myThreadId) +
     $       values2(i,iTimer,myThreadId) - values1(i,iTimer,myThreadId)
          enddo
#endif
          timerStatus(iTimer,myThreadId)  = STOPPED
          timerStops (iTimer,myThreadId)  = 
     &    timerStops (iTimer,myThreadId)+1
        ENDIF
#ifdef USE_PAPI
      ELSEIF ( tmpAction .EQ. INIT ) THEN
CCE107 PAPI - Check PAPI version, find the maximum number of events and 
C      initialize the library, read the suggested events and create 
C      EventSet, prepare counter for use
         Check = PAPI_VER_CURRENT
         call PAPIF_library_init(Check)
         if (Check .NE. PAPI_VER_CURRENT) then
            WRITE(msgBuffer,*) "PAPI Library Version is out of Date"
            CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &           SQUEEZE_RIGHT,myThreadId)
            CALL ABORT
         endif
         neventsmax = PAPIF_num_counters(check)
         if (neventsmax .GT. nmaxevents) then
            WRITE(msgBuffer,*) "Fix the nmaxevents in the code to ",
     $           neventsmax 
            CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &           SQUEEZE_RIGHT,myThreadId)
            CALL ABORT
         endif
         _BEGIN_MASTER(myThreadId)
         CALL mdsFindUnit (papiunit, myThreadId)
         OPEN(UNIT=papiunit,FILE='data.papi',STATUS='OLD')
        read(papiunit,*) nevents
C       reset to reasonable values
        if (nevents .gt. neventsmax) then
           nevents = neventsmax
           WRITE(msgBuffer,*)
     $          "resetting the number of PAPI events to the maximum" 
           CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &           SQUEEZE_RIGHT,myThreadId)
        endif
        do i = 1,nevents
          read(papiunit,*) EventName
          call PAPIF_event_name_to_code(EventName, EventCode(i), Check)
        end do
        close(papiunit)
        _END_MASTER(myThid)
        EventSet = PAPI_NULL
        call PAPIF_create_eventset(EventSet, Check)
        do i = 1,nevents
           call PAPIF_add_event(EventSet, EventCode(i), Check)
           if (Check .NE. PAPI_OK) then
              CALL PAPIF_event_code_to_name(EventCode(i), EventName,
     $             Check)
              WRITE(msgBuffer,*) "Abort After PAPIF_add_event: ",
     $             EventName
              CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &             SQUEEZE_RIGHT,myThreadId)
              CALL ABORT
           endif
        enddo
CCE107 - Start counting events
        call PAPIF_start(EventSet, Check)
#endif
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
#ifdef USE_PAPI
        do i = 1,nevents
           call PAPIF_event_code_to_name(EventCode(i), EventName, Check)
           if ((EventName .ne. 'PAPI_FLOPS   ') .and.
     $         (EventName .ne. 'PAPI_IPS     ')) then
               WRITE(msgBuffer,71) Eventname,
     $         values(i,iTimer,myThreadId)/timerUser(iTimer,myThreadId),
     $         values(i,iTimer,myThreadId)/timerWall(iTimer,myThreadId), 
     $         1.D0*values(i,iTimer,myThreadId)
           else
               WRITE(msgBuffer,72) Eventname,1.D0*values(j,I,myThreadId)
           endif
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#endif
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
#ifdef USE_PAPI
        do j = 1,nevents
           call PAPIF_event_code_to_name(EventCode(j), EventName, Check)
           if ((EventName .ne. 'PAPI_FLOPS   ') .and.
     $         (EventName .ne. 'PAPI_IPS     ')) then
               WRITE(msgBuffer,71) Eventname,
     $             values(j,I,myThreadId)/timerUser(I,myThreadId),
     $             values(j,I,myThreadId)/timerWall(I,myThreadId),
     $             1.D0*values(j,I,myThreadId) 
           else
               WRITE(msgBuffer,72) Eventname,1.D0*values(j,I,myThreadId)
           endif
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#endif
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

 71   FORMAT(A,' per sec ',D13.7,' ',D13.7,', number ', D13.7)
 72   FORMAT(A,D13.7)
      END

CBOP
C     !ROUTINE: TIMER_GET_TIME

C     !INTERFACE:
      SUBROUTINE TIMER_GET_TIME( 
     O                           userTime, 
     O                           systemTime, 
     O                           wallClockTime )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE TIMER\_GET\_TIME                                 
C     | o Query system timer routines.                            
C     *==========================================================*
C     | Routine returns total elapsed time for program so far.    
C     | Three times are returned that conventionally are used as  
C     | user time, system time and wall-clock time. Not all these 
C     | numbers are available on all machines.                    
C     *==========================================================*

C     !INPUT/OUTPUT PARAMETERS:
C     userTime      :: User time returned
C     systemTime    :: System time returned
C     wallClockTime :: Wall clock time returned
      Real*8 userTime
      Real*8 systemTime
      Real*8 wallClockTime

C     !USES:
CEH3 This needs to be further cleaned up using a HAVE_CLOC define
CEH3 that is diagnosed by genmake
CEH3 #ifndef HAVE_FDATE
      Real*8 system_time, user_time, timenow
CEH3 #else
#ifdef TARGET_AIX
      Real*4 ETIME_
      EXTERNAL ETIME_
#else
      Real*4 ETIME
      EXTERNAL ETIME
#endif
CEH3 #endif

C     !LOCAL VARIABLES:
C     ACTUAL, TARRAY, :: Temps. to hold times
C     wTime
      Real*4 ACTUAL, TARRAY(2)
      Real*8 wtime
CEOP

C     Real*8 MPI_Wtime
C     EXTERNAL MPI_Wtime

#ifndef IGNORE_TIME

CCE107 Fixed for AIX and UNICOS
#ifdef TARGET_AIX
      ACTUAL = ETIME_(TARRAY)
      userTime      = TARRAY(1)
      systemTime    = TARRAY(2)
      wallClockTime = timenow()
#elif (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
      userTime      = SECOND()
      systemTime    = 0.
      wallClockTime = SECONDR()
#else
#ifdef HAVE_ETIME
      ACTUAL = ETIME(TARRAY)
#else
      TARRAY(1) = user_time()
      TARRAY(2) = system_time()
#endif
      userTime      = TARRAY(1)
      systemTime    = TARRAY(2)
#ifdef HAVE_CLOC
      CALL CLOC(wTime)
#else 
      wtime = timenow()
#endif /* HAVE_CLOC */
      wallClockTime = wtime
#endif /* CRAY defines */

CCE107 Sometimes MPI_Wtime has better resolution...
#if (defined (ALLOW_USE_MPI) && defined (USE_MPI_WTIME))
      wtime         = MPI_Wtime()
      wallClockTime = wtime
#endif /* ALLOW_USE_MPI && USE_MPI_WTIME */
      
#else   /* IGNORE_TIME */
      wtime = 0.0
#endif  /* IGNORE_TIME */

      RETURN
      END

CBOP

C     !ROUTINE: TIMER_PRINTALL

C     !INTERFACE:
      SUBROUTINE TIMER_PRINTALL( myThreadId )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE TIMER\_PRINTALL                                 
C     | o Print timer information                                 
C     *==========================================================*
C     | Request print out of table of timing from all timers.     
C     *==========================================================*

C     !INPUT PARAMETERS:
C     myThreadId :: This threads number
      INTEGER myThreadId
CEOP

      CALL TIMER_CONTROL( ' ', 'PRINTALL', 'TIMER_PRINTALL' , 
     &                   myThreadId )
C
      RETURN
      END

CBOP
C     !ROUTINE: TIMER_START

C     !INTERFACE:
      SUBROUTINE TIMER_START ( string , myThreadId )
      IMPLICIT NONE

C     !DESCRIPTION:
C     Start timer named "string".

C     !INPUT PARAMETERS:
C     string     :: Name of timer
C     myThreadId :: My thread number
      CHARACTER*(*) string
      INTEGER myThreadId
CEOP
C
      CALL TIMER_CONTROL( string, 'START', 'TIMER_START' , myThreadId)
C
      RETURN
      END
CBOP
C     !ROUTINE: TIMER_STOP

C     !INTERFACE:
      SUBROUTINE TIMER_STOP  ( string , myThreadId )
      IMPLICIT NONE

C     !DESCRIPTION:
C     Stop timer named "string".

C     !INPUT PARAMETERS:
C     string     :: Name of timer
C     myThreadId :: My thread number
      CHARACTER*(*) string
      INTEGER myThreadId
CEOP
C
      CALL TIMER_CONTROL( string, 'STOP', 'TIMER_STOP' , myThreadId )
C
      RETURN
      END
C***********************************************************************

#ifdef USE_PAPI
CCE107 Initialization of common block for PAPI timers
      BLOCK DATA setpapivalues
#include "EEPARAMS.h"
      INTEGER maxTimers
      PARAMETER (maxTimers = 40)
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 18)
      INTEGER EventCode(nmaxevents)
      INTEGER size
      PARAMETER (size = 3*nmaxevents*maxTimers*MAX_NO_THREADS)
      INTEGER*8 values(nmaxevents, maxTimers , MAX_NO_THREADS),
     $     values1(nmaxevents, maxTimers, MAX_NO_THREADS),
     $     values2(nmaxevents, maxTimers, MAX_NO_THREADS)
      COMMON /papivalues/ values, values1, values2
      DATA values, values1, values2 /size*0/ 
      END
#endif
