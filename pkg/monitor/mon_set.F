C $Header: /u/gcmpack/MITgcm/pkg/monitor/Attic/mon_set.F,v 1.3 2001/09/27 18:11:59 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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

      SUBROUTINE MON_SET_PREF(prefString, myThid )
C     /==========================================================\
C     | SUBROUTINE MON_SET_PREF                                  |
C     | o Set default monitor prefix string.                     |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

      EXTERNAL IFNBLNK, ILNBLNK
      INTEGER  IFNBLNK, ILNBLNK

C     === Routine arguments ===
C     prefString - String to use for prefixing monitor output
C     myThid     - Instance number of this call to monitor
      CHARACTER*(*) prefString
      INTEGER myThid

C     === Local variables ===
C     I0, I1 - String first and last indices
C     IL     - String length
      INTEGER I0, I1
      INTEGER IL

C     Dont change before everyone is ready
      CALL BAR2( myThid )

C     Set monitor I/O unit
      I0 = IFNBLNK( prefString )
      I1 = ILNBLNK( prefString )
      IL = I1-I0+1
      IF ( IL .LE. MAX_LEN_MBUF ) THEN
       mon_pref = ' '
       mon_prefL = IL
       mon_pref(1:IL) = prefString(I0:I1)
      ENDIF

C     Make sure everyone sees the change
      CALL BAR2( myThid )

      RETURN
      END
