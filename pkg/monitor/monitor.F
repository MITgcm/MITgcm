C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor.F,v 1.8 2001/11/08 16:47:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MONITOR(
     I                myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE MONITOR                                       |
C     | o Calculate bare statistics of global array "arr"        |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "MONITOR.h"

C     === Routine arguments ===
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Local variables ====
      CHARACTER*(MAX_LEN_MBUF) msgBuf   
      _RL dT
      INTEGER time_as_int

      dT=deltaTclock

      IF (DIFFERENT_MULTIPLE(monitorFreq,myTime,myTime-dT)) THEN

C      Dynamics field monitor start 
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// Begin MONITOR dynamic field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
       _END_MASTER(myThid)

C      Print the time to make grepping the stdout easier
       CALL MON_SET_PREF('time',myThid)
       CALL MON_OUT_I( '_tsnumber', myIter,mon_string_none,myThid)
       CALL MON_OUT_RL('_secondsf', myTime,mon_string_none,myThid)
       time_as_int = NINT( myTime )
       CALL MON_OUT_I( '_secondsi', time_as_int,mon_string_none,myThid)
C      Print the basic statistics of model state variables
       CALL MON_SET_PREF('dynstat',myThid)
       CALL MON_PRINTSTATS_RL(1,EtaN,'_eta',
     &                        maskH,maskH,rA ,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,uVel,'_uvel',
     &                        maskW,hFacW,rAw,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,vVel,'_vvel',
     &                        maskS,hFacS,rAs,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,wVel,'_wvel',
     &                        maskC,maskC,rA ,drC,myThid)
       CALL MON_PRINTSTATS_RL(Nr,theta,'_theta',
     &                        maskC,hFacC,rA ,drF,myThid)
       CALL MON_PRINTSTATS_RL(Nr,salt,'_salt',
     &                        maskC,hFacC,rA ,drF,myThid)

C      Print the numerical stablility parameters for current state
       CALL MON_SET_PREF('advcfl',myThid)
       dT=max(deltaTtracer,deltaTmom)
       CALL MON_ADVCFL('_uvel',uVel,recip_dxc,dT,myThid)
       CALL MON_ADVCFL('_vvel',vVel,recip_dyc,dT,myThid)
       CALL MON_ADVCFLW('_wvel',wVel,recip_drc,dT,myThid)
       CALL MON_ADVCFLW2('_W_hf',wVel,recip_hFacC,
     &                               recip_drC,dT,myThid)

C      Print stats for KE
       CALL MON_SET_PREF('ke',myThid)
       CALL MON_KE(myThid)

C      Dynamics field monitor finish
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)') '// End MONITOR dynamic field statistics'
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit,
     &                    SQUEEZE_RIGHT , 1)
        WRITE(msgBuf,'(A)')
     &'// ======================================================='
        CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
       _END_MASTER(myThid)

      ENDIF

      RETURN
      END
