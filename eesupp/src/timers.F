C $Header: /u/gcmpack/MITgcm/eesupp/src/timers.F,v 1.30 2014/01/19 14:33:43 jmc Exp $
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
      PARAMETER ( maxTimers = 50 )
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
#if defined(USE_PAPI) || defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined (USE_PCL)
      CHARACTER*(*) INIT
      PARAMETER ( INIT = 'INIT' )
#ifdef USE_PAPI
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 18)
      INTEGER neventsmax, nevents
      SAVE neventsmax, nevents
      INTEGER*8 values(nmaxevents, maxTimers , MAX_NO_THREADS),
     $     values1(nmaxevents, maxTimers, MAX_NO_THREADS),
     $     values2(nmaxevents, maxTimers, MAX_NO_THREADS)
      COMMON /papivalues/ values, values1, values2
#include <fpapi.h>
      CHARACTER(13) EventName
      INTEGER EventCode(nmaxevents)
      INTEGER Check, EventSet
      INTEGER papiunit
      SAVE EventCode, EventSet
      INTEGER j
#else
#if defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
#include <pclh.f>
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      INTEGER flags, res, nevents
      INTEGER*8 descr
      CHARACTER*22 pcl_counter_name(0:nmaxevents-1)
#ifdef USE_PCL
      INTEGER pcl_counter_list(nmaxevents)
      INTEGER*8 i_result(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER*8 i_result1(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER*8 i_result2(nmaxevents, maxTimers, MAX_NO_THREADS)
      REAL*8 fp_result(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER j
#else
      INTEGER pcl_counter_list(5), alt_counter_list(5)
      INTEGER*8 i_result(5)
      REAL*8 fp_result(5)
      SAVE alt_counter_list
      DATA alt_counter_list /PCL_MFLOPS, PCL_IPC, PCL_L1DCACHE_MISSRATE,
     $     PCL_L2DCACHE_MISSRATE, PCL_MEM_FP_RATIO/
#endif
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents
      COMMON /pclnames/ pcl_counter_name
      INTEGER pclunit
#endif
#endif
#endif
      INTEGER I
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
CCE107 PAPI - Read event counts
          call PAPIF_read(EventSet, values1(1,iTimer,myThreadId), Check)
#else
#ifdef USE_PCL
CCE107 PCL - Read event counts
          res = PCLread(descr, i_result1(1,iTimer,myThreadId),
     $         fp_result(1,iTimer,myThreadId), nevents)
#endif
#endif
        ENDIF
#ifdef USE_LIBHPM
#ifdef TARGET_BGL
        CALL f_hpmstart((myThreadId-1)*100+iTimer,tmpName)
#else
        CALL f_hpmtstart((myThreadId-1)*100+iTimer,tmpName)
#endif
#endif
      ELSEIF ( tmpAction .EQ. STOP ) THEN
        IF ( iTimer .EQ. 0 ) GOTO 905
#ifdef USE_LIBHPM
#ifdef TARGET_BGL
        CALL f_hpmstop((myThreadId-1)*100+iTimer)
#else
        CALL f_hpmtstop((myThreadId-1)*100+iTimer)
#endif
#endif
        IF ( timerStatus(iTimer,myThreadId) .EQ. RUNNING ) THEN
#ifdef USE_PAPI
CCE107 PAPI - Read event counts
          call PAPIF_read(EventSet, values2(1,iTimer,myThreadId), Check)
#else
#ifdef USE_PCL
CCE107 PCL - Read event counts
          res = PCLread(descr, i_result2(1,iTimer,myThreadId),
     $         fp_result(1,iTimer,myThreadId), nevents)
#endif
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
#else
#ifdef USE_PCL
          do i=1,nevents
             i_result(i,iTimer,myThreadId) = i_result(i,iTimer
     $            ,myThreadId) + i_result2(i,iTimer,myThreadId) -
     $            i_result1(i,iTimer,myThreadId)
          enddo
#endif
#endif
          timerStatus(iTimer,myThreadId)  = STOPPED
          timerStops (iTimer,myThreadId)  =
     &    timerStops (iTimer,myThreadId)+1
        ENDIF
#if defined (USE_PAPI) || defined (USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
      ELSEIF ( tmpAction .EQ. INIT ) THEN
#ifdef USE_PAPI
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
         call PAPIF_num_counters(neventsmax)
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
     &          SQUEEZE_RIGHT,myThreadId)
        endif
        do i = 1,nevents
           read(papiunit,*) EventName
           if ((EventName .eq. 'PAPI_FLOPS') .or.
     $          (EventName .eq. 'PAPI_IPS')) then
              WRITE(msgBuffer,*) "Abort! Rate events are not supported:"
     $             ,EventName
              CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &             SQUEEZE_RIGHT,myThreadId)
              CALL ABORT
           endif

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
#else
#if defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
CCE107 PCL - initialize the library, read the suggested events
C      and check them
        res = PCLinit(descr)

#ifdef USE_PCL
        _BEGIN_MASTER(myThreadId)
        CALL mdsFindUnit (pclunit, myThreadId)
        OPEN(UNIT=pclunit,FILE='data.pcl',STATUS='OLD')
        read(pclunit,*) nevents
C     reset to reasonable values
        if (nevents .gt. nmaxevents) then
           nevents = nmaxevents
           WRITE(msgBuffer,*)
     $          "resetting the number of PCL events to the maximum"
           CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        endif
        do i = 1,nevents
           read(pclunit,*) pcl_counter_list(i)
           if ((pcl_counter_list(i) .ge. PCL_MFLOPS) .or.
     $          (pcl_counter_list(i) .lt. 1)) then
              if ((pcl_counter_list(i) .ge. PCL_MFLOPS) .and.
     $             (pcl_counter_list(i) .le. nmaxevents)) then
                 WRITE(msgBuffer,*)
     $                "Abort! Rate events are not relevant:",
     $                pcl_counter_name(pcl_counter_list(i))
              else
                 WRITE(msgBuffer,*)
     $                "Abort! Events are not defined:",
     $                pcl_counter_list(i)
              endif
              CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &             SQUEEZE_RIGHT,myThreadId)
              CALL ABORT
           endif
        enddo
        close(pclunit)
        _END_MASTER(myThid)

        do i = 1,nevents
CCE107 check to see that event are supported in the order asked
           res = PCLquery(descr, pcl_counter_list, i, flags)
           IF(res .NE. PCL_SUCCESS) THEN
              WRITE(msgBuffer,*) "Abort! No support when adding event: "
     $             , pcl_counter_name(pcl_counter_list(i))
              CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &             SQUEEZE_RIGHT,myThreadId)
              CALL ABORT
           endif
        enddo
#else
        do i = 1,5
CCE107 check to see which rate events are supported.
           res = PCLquery(descr, pcl_counter_list, nevents+1, flags)
           if ((res .ne. PCL_SUCCESS) .and. (i .lt. 5)) then
              pcl_counter_list(nevents+1) = alt_counter_list(i+1)
           else
              if (i .lt. 5) then
                 nevents = nevents + 1
              endif
           endif
        enddo
        if (nevents .eq. 0) then
           WRITE(msgBuffer,*)
     $          "No PCL rate events supported: Please recompile!"
           CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
           CALL ABORT
        endif
#endif

CCE107 - Start counting events
        res = PCLstart(descr, pcl_counter_list, nevents, flags)
        IF(res .NE. PCL_SUCCESS) THEN
           WRITE(msgBuffer,*) "PCL counting failed - please recompile!"
           CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
           CALL ABORT
        ENDIF
#endif
#endif
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
           WRITE(msgBuffer,71) Eventname,
     $          values(i,iTimer,myThreadId)/timerUser(iTimer,myThreadId)
     $          ,values(i,iTimer,myThreadId)/timerWall(iTimer,myThreadId
     $          ),1.D0*values(i,iTimer,myThreadId)
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#else
#ifdef USE_PCL
        do i = 1,nevents
           WRITE(msgBuffer,71) pcl_counter_name(pcl_counter_list(i)),
     $          i_result(i,iTimer,myThreadId)/timerUser(iTimer
     $          ,myThreadId),i_result(i,iTimer,myThreadId)
     $          /timerWall(iTimer,myThreadId),1.D0*i_result(i,iTimer
     $          ,myThreadId)
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#endif
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
           WRITE(msgBuffer,71) Eventname,
     $          values(j,I,myThreadId)/timerUser(I,myThreadId),
     $          values(j,I,myThreadId)/timerWall(I,myThreadId),
     $          1.D0*values(j,I,myThreadId)
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#else
#ifdef USE_PCL
        do j = 1,nevents
           WRITE(msgBuffer,71) pcl_counter_name(pcl_counter_list(j)),
     $          i_result(j,I,myThreadId)/timerUser(I,myThreadId)
     $          ,i_result(j,I,myThreadId)/timerWall(I,myThreadId),1.D0
     $          *i_result(j,I,myThreadId)
           CALL PRINT_MESSAGE(msgBuffer,standardMessageUnit,
     &          SQUEEZE_RIGHT,myThreadId)
        enddo
#endif
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
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name "',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Names must have fewer than',maxString+1,' characters.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  902 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' No timer action specified.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  903 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer action"',name(ISTART:IEND),'" is invalid.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Valid actions are:'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' "START", "STOP", "PRINT" and "PRINTALL".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  904 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer "',name(ISTART:IEND),'" cannot be created.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' Only ',maxTimers,' timers are allowed.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000
  905 CONTINUE
      WRITE(msgBuffer,*)
     &'                                                       '
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*** WARNING WARNING WARNING WARNING WARNING WARNING ***'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'procedure: "',callProc,'".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'Timer name is blank.'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &' A name must be used with "START", "STOP" or  "PRINT".'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      WRITE(msgBuffer,*)
     &'*******************************************************'
      CALL PRINT_MESSAGE(msgBuffer,errorMessageUnit,
     &  SQUEEZE_RIGHT,myThreadId)
      GOTO 1000

#if (defined USE_PAPI) || (defined USE_PCL)
 71   FORMAT(A,' per sec ',D13.7,' ',D13.7,', number ', D13.7)
#endif
c72   FORMAT(A,D13.7)
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C     The following was seriously hacked around by Mark Hadfield
C     October 2006

#ifdef IGNORE_TIME

      userTime = 0.
      systemTime = 0.
      wallClockTime = 0.

#else

C     Declarations follow the same preprocessor structure as the
C     executable code below.

# if defined (TARGET_AIX) || defined (TARGET_BGL)
      Real*4 etime_
      Real*8 timenow
      external etime_, timenow
      Real*4 actual, tarray(2)
# elif (defined TARGET_T3E || defined TARGET_CRAY_VECTOR)
      real second, secondr
      external second, secondr
# else
#  ifdef HAVE_ETIME
      Real*4 etime
      EXTERNAL etime
      Real*4 actual, tarray(2)
#  else
      Real*8 csystemtime, cusertime
      external csystemtime, cusertime
#  endif
#  if defined HAVE_CLOC
      Real*8 wtime
#  elif (defined (ALLOW_USE_MPI) && defined (USE_MPI_WTIME))
C     No declarations necessary
#  else
      Real*8 timenow
      external timenow
#  endif /* HAVE_CLOC */
# endif
CEOP

C     Executable code

CCE107 Fixed for AIX and UNICOS
# if defined(TARGET_AIX) || defined(TARGET_BGL)
      ACTUAL = ETIME_(TARRAY)
      userTime      = TARRAY(1)
      systemTime    = TARRAY(2)
      wallClockTime = timenow()
# elif (defined (TARGET_T3E) || defined (TARGET_CRAY_VECTOR))
      userTime      = SECOND()
      systemTime    = 0.
      wallClockTime = SECONDR()
# else
#  ifdef HAVE_ETIME
      actual = etime(tarray)
      userTime = tarray(1)
      systemTime = tarray(2)
#  else
      userTime   = cusertime()
      systemTime = csystemtime()
#  endif
#  if defined HAVE_CLOC
      CALL cloc(wTime)
      wallClockTime = wtime
#  elif (defined (ALLOW_USE_MPI) && defined (USE_MPI_WTIME))
      wallClockTime = MPI_Wtime()
#  else
      wallClockTime = timenow()
#  endif
# endif
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef USE_PAPI
CCE107 Initialization of common block for PAPI timers
      BLOCK DATA setpapivalues
#include "EEPARAMS.h"
      INTEGER maxTimers
      PARAMETER (maxTimers = 50)
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 18)
      INTEGER size
      PARAMETER (size = 3*nmaxevents*maxTimers*MAX_NO_THREADS)
      INTEGER*8 values(nmaxevents, maxTimers , MAX_NO_THREADS),
     $     values1(nmaxevents, maxTimers, MAX_NO_THREADS),
     $     values2(nmaxevents, maxTimers, MAX_NO_THREADS)
      COMMON /papivalues/ values, values1, values2
      DATA values, values1, values2 /size*0/
      END
#endif
#if defined(USE_PCL_FLOPS_SFP) || defined(USE_PCL_FLOPS) || defined(USE_PCL)
CCE107 Initialization of common block for PCL event names
      BLOCK DATA setpclnames
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      CHARACTER*22 pcl_counter_name(0:nmaxevents-1)
      COMMON /pclnames/ pcl_counter_name
      DATA pcl_counter_name(0) /'PCL_L1CACHE_READ'/
      DATA pcl_counter_name(1) /'PCL_L1CACHE_WRITE'/
      DATA pcl_counter_name(2) /'PCL_L1CACHE_READWRITE'/
      DATA pcl_counter_name(3) /'PCL_L1CACHE_HIT'/
      DATA pcl_counter_name(4) /'PCL_L1CACHE_MISS'/
      DATA pcl_counter_name(5) /'PCL_L1DCACHE_READ'/
      DATA pcl_counter_name(6) /'PCL_L1DCACHE_WRITE'/
      DATA pcl_counter_name(7) /'PCL_L1DCACHE_READWRITE'/
      DATA pcl_counter_name(8) /'PCL_L1DCACHE_HIT'/
      DATA pcl_counter_name(9) /'PCL_L1DCACHE_MISS'/
      DATA pcl_counter_name(10) /'PCL_L1ICACHE_READ'/
      DATA pcl_counter_name(11) /'PCL_L1ICACHE_WRITE'/
      DATA pcl_counter_name(12) /'PCL_L1ICACHE_READWRITE'/
      DATA pcl_counter_name(13) /'PCL_L1ICACHE_HIT'/
      DATA pcl_counter_name(14) /'PCL_L1ICACHE_MISS'/
      DATA pcl_counter_name(15) /'PCL_L2CACHE_READ'/
      DATA pcl_counter_name(16) /'PCL_L2CACHE_WRITE'/
      DATA pcl_counter_name(17) /'PCL_L2CACHE_READWRITE'/
      DATA pcl_counter_name(18) /'PCL_L2CACHE_HIT'/
      DATA pcl_counter_name(19) /'PCL_L2CACHE_MISS'/
      DATA pcl_counter_name(20) /'PCL_L2DCACHE_READ'/
      DATA pcl_counter_name(21) /'PCL_L2DCACHE_WRITE'/
      DATA pcl_counter_name(22) /'PCL_L2DCACHE_READWRITE'/
      DATA pcl_counter_name(23) /'PCL_L2DCACHE_HIT'/
      DATA pcl_counter_name(24) /'PCL_L2DCACHE_MISS'/
      DATA pcl_counter_name(25) /'PCL_L2ICACHE_READ'/
      DATA pcl_counter_name(26) /'PCL_L2ICACHE_WRITE'/
      DATA pcl_counter_name(27) /'PCL_L2ICACHE_READWRITE'/
      DATA pcl_counter_name(28) /'PCL_L2ICACHE_HIT'/
      DATA pcl_counter_name(29) /'PCL_L2ICACHE_MISS'/
      DATA pcl_counter_name(30) /'PCL_TLB_HIT'/
      DATA pcl_counter_name(31) /'PCL_TLB_MISS'/
      DATA pcl_counter_name(32) /'PCL_ITLB_HIT'/
      DATA pcl_counter_name(33) /'PCL_ITLB_MISS'/
      DATA pcl_counter_name(34) /'PCL_DTLB_HIT'/
      DATA pcl_counter_name(35) /'PCL_DTLB_MISS'/
      DATA pcl_counter_name(36) /'PCL_CYCLES'/
      DATA pcl_counter_name(37) /'PCL_ELAPSED_CYCLES'/
      DATA pcl_counter_name(38) /'PCL_INTEGER_INSTR'/
      DATA pcl_counter_name(39) /'PCL_FP_INSTR'/
      DATA pcl_counter_name(40) /'PCL_LOAD_INSTR'/
      DATA pcl_counter_name(41) /'PCL_STORE_INSTR'/
      DATA pcl_counter_name(42) /'PCL_LOADSTORE_INSTR'/
      DATA pcl_counter_name(43) /'PCL_INSTR'/
      DATA pcl_counter_name(44) /'PCL_JUMP_SUCCESS'/
      DATA pcl_counter_name(45) /'PCL_JUMP_UNSUCCESS'/
      DATA pcl_counter_name(46) /'PCL_JUMP'/
      DATA pcl_counter_name(47) /'PCL_ATOMIC_SUCCESS'/
      DATA pcl_counter_name(48) /'PCL_ATOMIC_UNSUCCESS'/
      DATA pcl_counter_name(49) /'PCL_ATOMIC'/
      DATA pcl_counter_name(50) /'PCL_STALL_INTEGER'/
      DATA pcl_counter_name(51) /'PCL_STALL_FP'/
      DATA pcl_counter_name(52) /'PCL_STALL_JUMP'/
      DATA pcl_counter_name(53) /'PCL_STALL_LOAD'/
      DATA pcl_counter_name(54) /'PCL_STALL_STORE'/
      DATA pcl_counter_name(55) /'PCL_STALL'/
      DATA pcl_counter_name(56) /'PCL_MFLOPS'/
      DATA pcl_counter_name(57) /'PCL_IPC'/
      DATA pcl_counter_name(58) /'PCL_L1DCACHE_MISSRATE'/
      DATA pcl_counter_name(59) /'PCL_L2DCACHE_MISSRATE'/
      DATA pcl_counter_name(60) /'PCL_MEM_FP_RATIO'/
      END

#ifdef USE_PCL
CCE107 Initialization of common block for PCL summary performance
      BLOCK DATA setpcls
#include "EEPARAMS.h"
      INTEGER maxTimers
      PARAMETER (maxTimers = 50)
      INTEGER nmaxevents
      PARAMETER (nmaxevents = 61)
      INTEGER size
      PARAMETER (size = nmaxevents*maxTimers*MAX_NO_THREADS)
      INTEGER PCL_CYCLES, PCL_MODE_USER_SYSTEM
      PARAMETER (PCL_CYCLES=36, PCL_MODE_USER_SYSTEM=3)
      INTEGER pcl_counter_list(nmaxevents)
      INTEGER flags, nevents
      INTEGER*8 i_result(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER*8 i_result1(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER*8 i_result2(nmaxevents, maxTimers, MAX_NO_THREADS)
      INTEGER*8 descr
      REAL*8 fp_result(nmaxevents, maxTimers, MAX_NO_THREADS)
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents
      DATA fp_result /size*0.0D0/
      DATA i_result /size*0/
      DATA i_result1 /size*0/
      DATA i_result2 /size*0/
      DATA descr /0/
      DATA nevents /nmaxevents/
      DATA pcl_counter_list /nmaxevents*PCL_CYCLES/
      DATA flags /PCL_MODE_USER_SYSTEM/
      END
#else
CCE107 Initialization of common block for PCL summary performance
      BLOCK DATA setpcls
      INTEGER PCL_MFLOPS, PCL_IPC, PCL_L1DCACHE_MISSRATE,
     $     PCL_L2DCACHE_MISSRATE, PCL_MEM_FP_RATIO
      PARAMETER (PCL_MFLOPS=56, PCL_IPC=57, PCL_L1DCACHE_MISSRATE=58,
     $     PCL_L2DCACHE_MISSRATE=59, PCL_MEM_FP_RATIO=60)
      INTEGER PCL_MODE_USER_SYSTEM
      PARAMETER (PCL_MODE_USER_SYSTEM=3)
      INTEGER pcl_counter_list(5), flags, nevents
      INTEGER*8 i_result(5), descr
      REAL*8 fp_result(5)
      COMMON /pclvars/ i_result, descr, fp_result, pcl_counter_list,
     $     flags, nevents
      DATA fp_result /5*0.0D0/
      DATA i_result /5*0/
      DATA descr /0/
      DATA nevents /0/
      DATA pcl_counter_list /PCL_MFLOPS, PCL_IPC, PCL_L1DCACHE_MISSRATE,
     $     PCL_L2DCACHE_MISSRATE, PCL_MEM_FP_RATIO/
      DATA flags /PCL_MODE_USER_SYSTEM/
      END
#endif
#endif
