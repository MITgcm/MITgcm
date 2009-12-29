C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_solution.F,v 1.10 2009/12/29 19:51:43 jmc Exp $
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
C     Take statistics from the input argument "statsTemp":
        tMin = statsTemp(1)
        tMax = statsTemp(2)
      ELSE
C     Statistics for T
        CALL MON_CALC_STATS_RL(
     I                Nr, theta, hFacC, maskInC, rA, drF,
     O                tMin, tMax, tMean, tSD, tDel2, tVol,
     I                myThid )
      ENDIF

      IF ( (tMax-tMin).GT.monSolutionMaxRange ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A,1P2E11.3)')
     &    'SOLUTION IS HEADING OUT OF BOUNDS: tMin,tMax=',tMin,tMax
        CALL PRINT_MESSAGE(msgBuf,errorMessageUnit,SQUEEZE_RIGHT,myThid)
        WRITE(msgBuf,'(2A,1PE11.3,A)') '  exceeds allowed range ',
     &             '(monSolutionMaxRange=', monSolutionMaxRange,')'
        CALL PRINT_MESSAGE(msgBuf,errorMessageUnit,SQUEEZE_RIGHT,myThid)
        WRITE(msgBuf,'(A,I10)')
     &    'MON_SOLUTION: STOPPING CALCULATION at Iter=', myIter
        CALL PRINT_MESSAGE(msgBuf,errorMessageUnit,SQUEEZE_RIGHT,myThid)
        _END_MASTER(myThid)

        CALL ALL_PROC_DIE( myThid )
        STOP
     &  'ABNORMAL END: S/R MON_SOLUTION, stops due to EXTREME Pot.Temp'
      ENDIF

      RETURN
      END
