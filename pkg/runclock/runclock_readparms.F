C $Header: /u/gcmpack/MITgcm/pkg/runclock/runclock_readparms.F,v 1.3 2014/05/28 00:13:16 jmc Exp $
C $Name:  $

#include "RUNCLOCK_OPTIONS.h"

      SUBROUTINE RUNCLOCK_READPARMS( myThid )
C     *==========================================================*
C     | SUBROUTINE RUNCLOCK_READPARMS                            |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "RUNCLOCK.h"

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_RUNCLOCK
C     === Local variables ===
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER tSecs

C--   RUNCLOCK parameters
      NAMELIST /RUNCLOCK/
     &    RC_maxtime_hr,
     &    RC_maxtime_mi,
     &    RC_maxtime_sc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useRunClock ) THEN
C-    pkg RUNCLOCK is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         RUNCLOCKIsOn = .FALSE.
C     print a (weak) warning if data.runclock is found
         CALL PACKAGES_UNUSED_MSG( 'useRunClock', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      RUNCLOCKIsOn = .TRUE.

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: opening data.runclock'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.runclock', 'RUNCLOCK_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values for RUNCLOCK
      RC_maxtime_hr=0
      RC_maxtime_mi=0
      RC_maxtime_sc=0

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=RUNCLOCK)

      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: read data.runclock done'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)

      IF (RC_maxtime_hr.LT.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: ERR! RC_maxtime_hr<0'
      CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (RC_maxtime_mi.LT.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: ERR! RC_maxtime_mi<0'
      CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (RC_maxtime_sc.LT.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: ERR! RC_maxtime_sc<0'
      CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (RC_maxtime_sc.GT.59 .AND. RC_maxtime_mi.NE.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: ERR! RC_maxtime_sc>59'
      CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF (RC_maxtime_mi.GT.59 .AND. RC_maxtime_hr.NE.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: ERR! RC_maxtime_mi>59'
      CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      tSecs=(RC_maxtime_hr*60+RC_maxtime_mi)*60+RC_maxtime_sc
      IF (tSecs.EQ.0) THEN
      WRITE(msgBuf,'(A)') ' RUNCLOCK_READPARMS: no Wall Clock limit set'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
c     write(0,*) ' RUNCLOCK_READPARMS: no Wall Clock limit set'
      ELSE
      WRITE(msgBuf,'(A,I7)')
     &  ' RUNCLOCK_READPARMS: Wall Clock limit set to ',tSecs
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
c     write(0,*) ' RUNCLOCK_READPARMS: Wall Clock limit set to ',tSecs
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_RUNCLOCK */

      RETURN
      END
