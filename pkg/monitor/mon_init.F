C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_init.F,v 1.2 2003/05/13 18:18:05 adcroft Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

      SUBROUTINE MON_INIT(myThid )
C     /==========================================================\
C     | SUBROUTINE MON_INIT                                      |
C     | o Set default monitor internal setup.                    |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
C     myThid - Instance number of this call to monitor
      INTEGER myThid

C     Set monitor I/O to standard output by default
C     and prefix monitor "variables" with mon by default.
      CALL MON_SET_IOUNIT( standardMessageUnit, myThid )
      CALL MON_SET_PREF  ( mon_string_none    , myThid )

C     Internal parameter for continuing when monitor_solution would
C     otherwise decide to stop the code.
      mon_overrideStop=.FALSE.

      RETURN
      END
