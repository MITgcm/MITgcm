C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_stats_rs.F,v 1.5 2005/02/09 21:15:03 heimbach Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_STATS_RS(
     I                myNr, arr, arrName,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_STATS_RS                                |
C     | o Prints to STDOUT the bare statistics of global array   |
C     |   "_RS arr" with label "arrName"                         |
C     | o This is an exact copy of MON_PRINTSTATS_RS but with    |
C     |   a different label at left of screen                    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

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

      CALL DEBUG_FLD_STATS_RS(
     I                myNr, arr,
     O                theMin,theMax,theMean,theSD,
     I                myThid )

      _BEGIN_MASTER( myThid )
      WRITE(standardmessageunit,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RS: ',arrName,'  min=',theMin
      WRITE(standardmessageunit,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RS: ',arrName,'  max=',theMax
      WRITE(standardmessageunit,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RS: ',arrName,' mean=',theMean
      WRITE(standardmessageunit,'(A,A30,A,1PE22.14)')
     &      'DEBUG_STATS_RS: ',arrName,' S.D.=',theSD
      _END_MASTER( myThid )

      RETURN
      END
