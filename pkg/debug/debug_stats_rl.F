C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_stats_rl.F,v 1.1 2001/06/06 14:55:46 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE DEBUG_STATS_RL(
     I                myNr, arr, arrName,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_STATS_RL                                |
C     | o Prints to STDOUT the bare statistics of global array   |
C     |   "_RL arr" with label "arrName"                         |
C     | o This is an exact copy of MON_PRINTSTATS_RL but with    |
C     |   a different label at left of screen                    |
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
      WRITE(*,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RL: ',arrName,'  min=',theMin
      WRITE(*,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RL: ',arrName,'  max=',theMax
      WRITE(*,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RL: ',arrName,' mean=',theMean
      WRITE(*,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RL: ',arrName,' S.D.=',theSD
      _END_MASTER( )

      RETURN
      END
