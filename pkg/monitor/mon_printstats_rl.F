C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rl.F,v 1.5 2001/11/08 16:47:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_PRINTSTATS_RL(
     I                myNr, arr, arrName, 
     I                arrMask,arrhFac,arrArea,arrDr,
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
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      INTEGER myThid

C     === Local variables ====
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD
      _RL theDel2

      CALL MON_STATS_RL(
     I                myNr, arr, arrMask,arrhFac,arrArea,arrDr,
     O                theMin,theMax,theMean,theSD,theDel2,
     I                myThid )

      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max  ,myThid)
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min  ,myThid)
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean ,myThid)
      CALL MON_OUT_RL( arrName, theSD,   mon_foot_sd   ,myThid)
      CALL MON_OUT_RL( arrName, theDel2, mon_foot_del2 ,myThid)

      RETURN
      END
