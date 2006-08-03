C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_solution.F,v 1.6 2006/08/03 19:06:34 molod Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_SOLUTION

C     !INTERFACE:
      SUBROUTINE MON_SOLUTION(
     I     statsTemp,
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Checks that the solutions is within bounds             

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      _RL statsTemp(*)
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf   
      _RL tMin,tMax,tMean,tSD,tDel2,tVol

      IF ( statsTemp(1) .LE. statsTemp(2) ) THEN
C     take statistics from the input argument "statsTemp":
        tMin = statsTemp(1)
        tMax = statsTemp(2)
      ELSE
C     Statistics for T
      CALL MON_STATS_RL(
     I                Nr, theta, maskC,hFacC,rA,drF,
     O                tMin,tMax,tMean,tSD,tDel2,tVol,
     I                myThid )
      ENDIF
      
      IF ( (tMax-tMin.GT.10.e3) .or. 
     .            (fluidIsWater .and. (tMax-tMin.GT.1.e3) )  ) THEN
        _BEGIN_MASTER(myThid)
         WRITE(msgBuf,'(A,1P2E12.3)')
     &    'SOLUTION IS HEADING OUT OF BOUNDS: tMin,tMax=',tMin,tMax
         CALL PRINT_MESSAGE(msgBuf,errorMessageUnit,SQUEEZE_RIGHT, 1)
         WRITE(msgBuf,'(A)') 'MON_SOLUTION: STOPPING CALCULATION'
         CALL PRINT_MESSAGE(msgBuf,errorMessageUnit,SQUEEZE_RIGHT, 1)
        _END_MASTER(myThid)

C jmc: add this "if not ..." to avoid beeing stuck when using coupler; 
        IF ( .NOT. useCoupler ) CALL EEDIE
        STOP 'MON_SOLUTION: STOPPED DUE TO EXTREME VALUES OF SOLUTION'
      ENDIF

      RETURN
      END
