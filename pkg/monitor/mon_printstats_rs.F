C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rs.F,v 1.6 2003/05/13 18:18:05 adcroft Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

      SUBROUTINE MON_PRINTSTATS_RS(
     I                myNr, arr, arrName,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_PRINTSTATS_RS                             |
C     | o Prints to STDOUT the bare statistics of global array   |
C     |   "_RS arr" with label "arrName"                         |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
      INTEGER myNr
      _RS arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      CHARACTER*(*) arrName
      INTEGER myThid

C     === Local variables ====
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD

      CALL MON_STATS_RS(
     I                myNr, arr,
     O                theMin,theMax,theMean,theSD,
     I                myThid )

      CALL MON_OUT_RS( arrName, theMax,  mon_foot_max  ,myThid)
      CALL MON_OUT_RS( arrName, theMin,  mon_foot_min  ,myThid)
      CALL MON_OUT_RS( arrName, theMean, mon_foot_mean ,myThid)
      CALL MON_OUT_RS( arrName, theSD,   mon_foot_sd   ,myThid)

      RETURN
      END
