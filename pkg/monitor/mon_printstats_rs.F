C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rs.F,v 1.8 2004/04/03 21:17:10 edhill Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_PRINTSTATS_RS

C     !INTERFACE:
      SUBROUTINE MON_PRINTSTATS_RS(
     I     myNr, arr, arrName,
     I     myThid )

C     !DESCRIPTION:
C     Prints to STDOUT the bare statistics of global array "\_RS arr"
C     with label "arrName".

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      INTEGER myNr
      _RS arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      CHARACTER*(*) arrName
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD

      CALL MON_STATS_RS(
     I     myNr, arr,
     O     theMin,theMax,theMean,theSD,
     I     myThid )

      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max  ,myThid)
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min  ,myThid)
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean ,myThid)
      CALL MON_OUT_RL( arrName, theSD,   mon_foot_sd   ,myThid)

      RETURN
      END
