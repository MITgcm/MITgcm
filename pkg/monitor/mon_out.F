C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_out.F,v 1.2 2001/06/25 20:35:23 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_OUT_I(pref, value, foot, myThid )
C.sh  /==========================================================\
C     | SUBROUTINE MON_OUT_I                                     |
C     | o Formatted integer I/O for monitor print out.           |
C.    \==========================================================/
      IMPLICIT NONE

C.gd  === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK
      INTEGER  myThid
C.

C.ra  === Routine arguments ===
C.d   pref   - Field prefix ( ignored if == mon_string_none )
C.d   value  - Value to print
C.d   foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      INTEGER       value
      CHARACTER*(*) foot
C.

C.lv  === Local variables ===
C.d   msgBuf - Buffer for building output string
C.d   lBuf   - Buffer for length
C.d.  I0     - Temps used in calculating string length
C     I1
C.    IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  lBuf
      INTEGER  I0, I1, IL
C.

      msgBuf = ' '
      lBuf   = 0

      I0 = IFNBLNK(mon_head)
      I1 = ILNBLNK(mon_head)
      IL = I1-I0+1
      IF ( IL .GT. 0 .AND. lBuf+IL+1 .LE. MAX_LEN_MBUF ) THEN
       msgBuf(1:IL) = mon_head
       lBuf = IL+1
       msgBuf(lBuf:lBuf) = ' '
      ENDIF

      IF ( mon_pref(1:mon_prefL) .NE. mon_string_none .AND.
     &     lBuf+mon_prefL+1      .LE. MAX_LEN_MBUF ) THEN
       lBuf = lBuf+1
       msgBuf(lBuf:lBuf+mon_prefL-1) = mon_pref(1:mon_prefL)
       lBuf = lBuf+mon_prefL-1
      ENDIF

      I0 = IFNBLNK(pref)
      I1 = ILNBLNK(pref)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( pref(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = pref(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      I0 = IFNBLNK(foot)
      I1 = ILNBLNK(foot)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( foot(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = foot(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      msgBuf(35:35) = '='

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf(36:57),'(1X,I21)') value
      CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1 )
      _END_MASTER()

      RETURN
      END
      SUBROUTINE MON_OUT_RS(pref, value, foot, myThid )
C.sh  /==========================================================\
C     | SUBROUTINE MON_OUT_RS                                    |
C     | o Formatted RS I/O for monitor print out.                |
C.    \==========================================================/
      IMPLICIT NONE

C.gd  === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK
      INTEGER  myThid
C.

C.ra  === Routine arguments ===
C.d   pref   - Field prefix ( ignored if == mon_string_none )
C.d   value  - Value to print
C.d   foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      _RS           value
      CHARACTER*(*) foot
C.

C.lv  === Local variables ===
C.d   msgBuf - Buffer for building output string
C.d   lBuf   - Buffer for length
C.d.  I0     - Temps used in calculating string length
C     I1
C.    IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  lBuf
      INTEGER  I0, I1, IL
C.

      msgBuf = ' '
      lBuf   = 0

      I0 = IFNBLNK(mon_head)
      I1 = ILNBLNK(mon_head)
      IL = I1-I0+1
      IF ( IL .GT. 0 .AND. lBuf+IL+1 .LE. MAX_LEN_MBUF ) THEN
       msgBuf(1:IL) = mon_head
       lBuf = IL+1
       msgBuf(lBuf:lBuf) = ' '
      ENDIF

      IF ( mon_pref(1:mon_prefL) .NE. mon_string_none .AND.
     &     lBuf+mon_prefL+1      .LE. MAX_LEN_MBUF ) THEN
       lBuf = lBuf+1
       msgBuf(lBuf:lBuf+mon_prefL-1) = mon_pref(1:mon_prefL)
       lBuf = lBuf+mon_prefL-1
      ENDIF

      I0 = IFNBLNK(pref)
      I1 = ILNBLNK(pref)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( pref(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = pref(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      I0 = IFNBLNK(foot)
      I1 = ILNBLNK(foot)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( foot(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = foot(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      msgBuf(35:35) = '='

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf(36:57),'(1X,1P1E21.13)') value
      CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1 )
      _END_MASTER()

      RETURN
      END
      SUBROUTINE MON_OUT_RL(pref, value, foot, myThid )
C.sh  /==========================================================\
C     | SUBROUTINE MON_OUT_RL                                    |
C     | o Formatted RL I/O for monitor print out.                |
C.    \==========================================================/
      IMPLICIT NONE

C.gd  === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"
      EXTERNAL IFNBLNK
      INTEGER  IFNBLNK
      EXTERNAL ILNBLNK
      INTEGER  ILNBLNK
C.

C.ra  === Routine arguments ===
C.d   pref   - Field prefix ( ignored if == mon_string_none )
C.d   value  - Value to print
C.d   foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      _RL           value
      CHARACTER*(*) foot
      INTEGER  myThid
C.

C.lv  === Local variables ===
C.d   msgBuf - Buffer for building output string
C.d   lBuf   - Buffer for length
C.d.  I0     - Temps used in calculating string length
C     I1
C.    IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  lBuf
      INTEGER  I0, I1, IL
C.

      msgBuf = ' '
      lBuf   = 0

      I0 = IFNBLNK(mon_head)
      I1 = ILNBLNK(mon_head)
      IL = I1-I0+1
      IF ( IL .GT. 0 .AND. lBuf+IL+1 .LE. MAX_LEN_MBUF ) THEN
       msgBuf(1:IL) = mon_head
       lBuf = IL+1
       msgBuf(lBuf:lBuf) = ' '
      ENDIF

      IF ( mon_pref(1:mon_prefL) .NE. mon_string_none .AND.
     &     lBuf+mon_prefL+1      .LE. MAX_LEN_MBUF ) THEN
       lBuf = lBuf+1
       msgBuf(lBuf:lBuf+mon_prefL-1) = mon_pref(1:mon_prefL)
       lBuf = lBuf+mon_prefL-1
      ENDIF

      I0 = IFNBLNK(pref)
      I1 = ILNBLNK(pref)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( pref(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = pref(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      I0 = IFNBLNK(foot)
      I1 = ILNBLNK(foot)
      IL = I1-I0+1
      IF ( IL .GT. 0 ) THEN
       IF ( foot(I0:I1) .NE. mon_string_none .AND. 
     &      lBuf+IL+1   .LE. MAX_LEN_MBUF ) THEN
        lBuf = lBuf+1
        msgBuf(lBuf:lBuf+IL-1) = foot(I0:I1)
        lBuf = lBuf+IL-1
       ENDIF
      ENDIF

      msgBuf(35:35) = '='

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf(36:57),'(1X,1P1E21.13)') value
      CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1 )
      _END_MASTER()

      RETURN
      END
