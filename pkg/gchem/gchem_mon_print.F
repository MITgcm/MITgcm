C $Header: /u/gcmpack/MITgcm/pkg/gchem/Attic/gchem_mon_print.F,v 1.1 2003/06/25 21:00:37 stephd Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

      SUBROUTINE GCHEM_PRINT(
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
      _RL theVol
      _RL theTot

      CALL MON_STATS_RL(
     I                myNr, arr, arrMask,arrhFac,arrArea,arrDr,
     O                theMin,theMax,theMean,theSD,theDel2,theVol,
     I                myThid )

      theTot=theMean*theVol
      CALL MON_OUT_RL( arrName, theMax,  mon_foot_max  ,myThid)
      CALL MON_OUT_RL( arrName, theMin,  mon_foot_min  ,myThid)
      CALL MON_OUT_RL( arrName, theMean, mon_foot_mean ,myThid)

      RETURN
      END
