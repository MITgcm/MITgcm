C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_solution.F,v 1.2 2003/12/15 02:13:13 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

      SUBROUTINE MON_SOLUTION(
     I                myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE MON_SOLUTION                                  |
C     | o Checks that the solutions is within bounds             |
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

C     === Local variables ====
      CHARACTER*(MAX_LEN_MBUF) msgBuf   
      _RL tMin,tMax,tMean,tSD,tDel2,tVol

C     Statistics for T
      CALL MON_STATS_RL(
     I                Nr, theta, maskC,hFacC,rA,drF,
     O                tMin,tMax,tMean,tSD,tDel2,tVol,
     I                myThid )
      
      IF (tMax-tMin.GT.1.e3) THEN
        _BEGIN_MASTER(myThid)
         WRITE(msgBuf,'(A,1P2E12.3)')
     &    'SOLUTION IS HEADING OUT OF BOUNDS: tMin,tMax=',tMin,tMax
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A)') 'STOPPING CALCULATION'
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        _END_MASTER(myThid)

C jmc: add this "if not ..." to avoid beeing stuck when using coupler; 
       IF ( .NOT. useCoupler )
     &  CALL EEDIE
        STOP 'MON_SOLUTION: STOPPED DUE TO EXTREME VALUES OF SOLUTION'
      ENDIF

      RETURN
      END
