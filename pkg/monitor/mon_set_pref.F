C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_set_pref.F,v 1.3 2006/10/17 19:01:49 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_SET_PREF

C     !INTERFACE:
      SUBROUTINE MON_SET_PREF( prefString, myThid )

C     !DESCRIPTION:
C     Set default monitor prefix string.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"
      EXTERNAL IFNBLNK, ILNBLNK
      INTEGER  IFNBLNK, ILNBLNK

C     !INPUT PARAMETERS:
C     prefString - String to use for prefixing monitor output
C     myThid     - Instance number of this call to monitor
      CHARACTER*(*) prefString
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     I0, I1 - String first and last indices
C     IL     - String length
      INTEGER I0, I1
      INTEGER IL

C     Dont change before everyone is ready
      CALL BAR2( myThid )

C     Set monitor I/O unit
      _BEGIN_MASTER(myThid)
      I0 = IFNBLNK( prefString )
      I1 = ILNBLNK( prefString )
      IL = I1-I0+1
      IF ( IL .LE. MAX_LEN_MBUF ) THEN
       mon_pref = ' '
       mon_prefL = IL
       mon_pref(1:IL) = prefString(I0:I1)
      ENDIF
      _END_MASTER(myThid)

C     Make sure everyone sees the change
      CALL BAR2( myThid )

      RETURN
      END
