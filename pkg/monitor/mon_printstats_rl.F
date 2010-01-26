C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rl.F,v 1.11 2010/01/26 01:09:02 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_PRINTSTATS_RL

C     !INTERFACE:
      SUBROUTINE MON_PRINTSTATS_RL(
     I     myNr, arr, arrName,
     I     arrMask,arrhFac,arrArea,arrDr,
     I     myThid )

C     !DESCRIPTION:
C     Prints to STDOUT the bare statistics of global array "\_RL arr"
C     with label "arrName".

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      _RL theMin, theMax, theMean, theSD, theDel2, theVol

C     Since 2009/12/21 MON_WRITESTATS_RL replaces MON_PRINTSTATS_RL
C     which is now disabled
      STOP 'ABNORMAL END: S/R MON_PRINTSTATS_RL no longer maintained'

      CALL MON_STATS_RL(
     I     myNr, arr, arrMask, arrhFac, arrArea, arrDr,
     O     theMin, theMax, theMean, theSD, theDel2, theVol,
     I     myThid )

      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max  ,myThid)
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min  ,myThid)
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean ,myThid)
      CALL MON_OUT_RL( arrName, theSD,   mon_foot_sd   ,myThid)
      CALL MON_OUT_RL( arrName, theDel2, mon_foot_del2 ,myThid)
c     CALL MON_OUT_RL( arrName, theVol,  mon_foot_vol  ,myThid)

      RETURN
      END
