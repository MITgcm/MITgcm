C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rl.F,v 1.2 2001/06/06 14:49:51 adcroft Exp $
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

      _BEGIN_MASTER( myThid )
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS_RL: ',arrName,'  min=',theMin
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS_RL: ',arrName,'  max=',theMax
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS_RL: ',arrName,' mean=',theMean
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS_RL: ',arrName,' S.D.=',theSD
      _END_MASTER( )

      RETURN
      END
