C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rl.F,v 1.3 2001/06/18 17:39:59 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_PRINTSTATS_RL(
     I                myNr, arr, arrName,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_PRINTSTATS_RL                             |
C     | o Prints to STDOUT the bare statistics of global array   |
C     |   "_RL arr" with label "arrName"                         |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      CHARACTER*(*) arrName
      INTEGER myThid

C     === Local variables ====
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD

      CALL MON_STATS_RL(
     I                myNr, arr,
     O                theMin,theMax,theMean,theSD,
     I                myThid )

      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max  )
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min  )
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean )
      CALL MON_OUT_RL( arrName, theSD,   mon_foot_sd   )

      RETURN
      END
