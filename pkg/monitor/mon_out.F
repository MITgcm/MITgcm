C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_out.F,v 1.6 2004/04/03 21:17:10 edhill Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_OUT_I

C     !INTERFACE:
      SUBROUTINE MON_OUT_I( pref, value, foot, myThid )

C     !DESCRIPTION:
C     Formatted integer I/O for monitor print out.           

C     !INPUT PARAMETERS:
C     pref   - Field prefix ( ignored if == mon_string_none )
C     value  - Value to print
C     foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      INTEGER       value
      CHARACTER*(*) foot
      INTEGER  myThid
CEOP

      CALL MON_OUT_ALL(pref, foot, 1, value, 0.0, 0.0d0, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_OUT_RS

C     !INTERFACE:
      SUBROUTINE MON_OUT_RS( pref, value, foot, myThid )

C     !DESCRIPTION:
C     Formatted RS I/O for monitor print out.

C     !INPUT PARAMETERS:
C     pref   - Field prefix ( ignored if == mon_string_none )
C     value  - Value to print
C     foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      _RS           value
      CHARACTER*(*) foot
      INTEGER  myThid
CEOP

      CALL MON_OUT_ALL(pref, foot, 2, 0, value, 0.0d0, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_OUT_RL

C     !INTERFACE:
      SUBROUTINE MON_OUT_RL(pref, value, foot, myThid )

C     !DESCRIPTION:
C     Formatted RL I/O for monitor print out.

C     !INPUT PARAMETERS:
C     pref   - Field prefix ( ignored if == mon_string_none )
C     value  - Value to print
C     foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref
      _RL           value
      CHARACTER*(*) foot
      INTEGER  myThid
CEOP

      CALL MON_OUT_ALL(pref, foot, 3, 0, 0.0, value, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MON_OUT_ALL

C     !INTERFACE:
      SUBROUTINE MON_OUT_ALL(
     I     pref, foot, 
     I     itype, ival, rval, dval,
     I     myThid )

C     !DESCRIPTION:
C     Formatted I/O for monitor output.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"
#include "MONITOR.h"
      INTEGER IFNBLNK
      INTEGER ILNBLNK

C     !INPUT PARAMETERS:
C     pref   - Field prefix ( ignored if == mon_string_none )
C     foot   - Field suffix ( ignored if == mon_string_none )
      CHARACTER*(*) pref, foot
      INTEGER itype
      INTEGER ival
      _RS     rval
      _RL     dval
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf - Buffer for building output string
C     lBuf   - Buffer for length
C     I0     - Temps used in calculating string length
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER  lBuf
      INTEGER  I0, I1, IL

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
#ifdef ALLOW_USE_MPI
        IF ( .NOT. useSingleCPUIO .OR. mpiMyId .EQ. 0 ) THEN
#endif /* ALLOW_USE_MPI */
          IF (itype .EQ. 1)
     &         WRITE(msgBuf(36:57),'(1X,I21)')       ival
          IF (itype .EQ. 2)
     &         WRITE(msgBuf(36:57),'(1X,1P1E21.13)') rval
          IF (itype .EQ. 3)
     &         WRITE(msgBuf(36:57),'(1X,1P1E21.13)') dval
          CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1 )
#ifdef ALLOW_USE_MPI
        ENDIF
#endif /* ALLOW_USE_MPI */
      _END_MASTER()

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
