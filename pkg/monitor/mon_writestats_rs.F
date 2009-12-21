C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_writestats_rs.F,v 1.1 2009/12/21 00:03:40 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_WRITESTATS_RS

C     !INTERFACE:
      SUBROUTINE MON_WRITESTATS_RS(
     I     myNr, arr, arrName,
     I     arrhFac, arrMask, arrArea, arrDr,
     O     arrStats,
     I     myThid )

C     !DESCRIPTION:
C     Compute the statistics of global array "\_RS arr" (account for
C     volume and mask) and write them to STDOUT with label "arrName".

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      INTEGER myNr
      _RS arr    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     arrStats :: statistics of the global array (min, max ...)
      _RL arrStats(*)
CEOP

C     !LOCAL VARIABLES:
      _RL theMin, theMax, theMean, theSD, theDel2, theVol

      CALL MON_CALC_STATS_RS(
     I     myNr, arr, arrhFac, arrMask, arrArea, arrDr,
     O     theMin, theMax, theMean, theSD, theDel2, theVol,
     I     myThid )

      arrStats(1) = theMin
      arrStats(2) = theMax
      arrStats(3) = theMean
      arrStats(4) = theSD
      arrStats(5) = theDel2
      arrStats(6) = theVol

      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max,  myThid )
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min,  myThid )
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean, myThid )
      CALL MON_OUT_RL( arrName, theSD,   mon_foot_sd,   myThid )
      CALL MON_OUT_RL( arrName, theDel2, mon_foot_del2, myThid )
c     CALL MON_OUT_RL( arrName, theVol,  mon_foot_vol,  myThid )

      RETURN
      END
