C $Header: /u/gcmpack/MITgcm/pkg/monitor/monitor.F,v 1.2 2001/06/04 13:25:35 adcroft Exp $
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

C     === Routine arguments ===
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     === Local variables ====
      _RL dT

      dT=deltaTclock

      IF (DIFFERENT_MULTIPLE(monitorFreq,myTime,myTime-dT)) THEN
C      Print the time to make grepping the stdout easier
       WRITE(*,'(A,A10,I10,A10,1PE14.4)')
     &   'MON_TIME:','iter=',myIter,'time=',myTime
C      Print the basic statistics of model state variables
       CALL MON_PRINTSTATS_RL(1,EtaN,'Eta',myThid)
       CALL MON_PRINTSTATS_RL(Nr,uVel,'Uvel',myThid)
       CALL MON_PRINTSTATS_RL(Nr,vVel,'Vvel',myThid)
       CALL MON_PRINTSTATS_RL(Nr,wVel,'Wvel',myThid)
       CALL MON_PRINTSTATS_RL(Nr,theta,'Theta',myThid)
       CALL MON_PRINTSTATS_RL(Nr,salt,'Salt',myThid)

C      Print the numerical stablility parameters for current state
       dT=max(deltaTtracer,deltaTmom)
       CALL MON_ADVCFL('U',uVel,recip_dxc,dT,myThid)
       CALL MON_ADVCFL('V',vVel,recip_dyc,dT,myThid)
       CALL MON_ADVCFL('W',wVel,recip_drc,dT,myThid)

C      Print stats for KE
       CALL MON_KE(myThid)

      ENDIF

      RETURN
      END
