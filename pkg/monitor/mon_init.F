C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_init.F,v 1.6 2004/09/07 23:51:22 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_INIT

C     !INTERFACE:
      SUBROUTINE MON_INIT( myThid )

C     !DESCRIPTION:
C     Set default monitor internal setup.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i
      character*(MAX_LEN_MBUF) msgbuf

C     set those 2 flags to write monitor of the grid (S/R ini_grid) 
C     and coriolis (S/R ini_cori) to standard output
      mon_write_stdout = .TRUE.
      mon_write_mnc    = .FALSE.

      IF ((monitorFreq .GT. 0.0) .AND. (mon_iotype .LT. 1)) THEN
        write(msgbuf,'(3a)') 'MONITOR ERROR: mon_freq>0 and',
     &       'mon_iotype<1 are incompatible'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MON_INIT'
      ENDIF

      IF ((mon_iotype .LT. 2) .AND. mnc_use_for_mon) THEN
        mon_iotype = 2
      ENDIF
      IF ((mon_iotype .GE. 2) .AND. (.NOT. mnc_use_for_mon)) THEN
        mon_iotype = 1
      ENDIF

      IF (useMNC .AND. (mon_iotype .GT. 1)) THEN
        DO i = 1,MAX_LEN_MBUF
          mon_fname(i:i) = ' '
        ENDDO
      ENDIF

C     Set monitor I/O to standard output by default
C     and prefix monitor "variables" with mon by default.
      CALL MON_SET_IOUNIT( standardMessageUnit, myThid )
      CALL MON_SET_PREF  ( mon_string_none    , myThid )

C     Internal parameter for continuing when monitor_solution would
C     otherwise decide to stop the code.
      mon_overrideStop=.FALSE.

      RETURN
      END
