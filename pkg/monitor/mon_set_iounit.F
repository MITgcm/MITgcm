C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_set_iounit.F,v 1.1 2003/05/13 18:18:05 adcroft Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

      SUBROUTINE MON_SET_IOUNIT(unit, myThid )
C     /==========================================================\
C     | SUBROUTINE MON_SET_IOUNIT                                |
C     | o Set default monitor unit for I/O.                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
C     unit   - Unit number to use for monitor output
C     myThid - Instance number of this call to monitor
      INTEGER unit
      INTEGER myThid

C     Dont change before everyone is ready
      CALL BAR2( myThid )

C     Set monitor I/O unit
      mon_ioUnit = standardMessageUnit

C     Make sure everyone sees the change
      CALL BAR2( myThid )

      RETURN
      END
