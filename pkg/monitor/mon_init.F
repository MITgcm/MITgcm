C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_init.F,v 1.4 2004/04/05 06:01:07 edhill Exp $
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

      IF (useMNC .AND. mnc_use_for_mon) THEN
        DO i = 1,MAX_LEN_MBUF
          mon_fname(i:i) = ' '
        ENDDO
        mon_do_fwrite = .FALSE.
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
