C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_init.F,v 1.1 2001/06/18 17:39:59 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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

      RETURN
      END
