C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_printstats_rs.F,v 1.1 2001/05/30 19:33:18 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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

C     === Routine arguments ===
      INTEGER myNr
      _RS arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      CHARACTER*(*) arrName
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      INTEGER numPnts
      LOGICAL noPnts
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
     &      'MON_PRINTSTATS: ',arrName,'  min=',theMin
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS: ',arrName,'  max=',theMax
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS: ',arrName,' mean=',theMean
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_PRINTSTATS: ',arrName,' S.D.=',theSD
      _END_MASTER( )

      RETURN
      END
