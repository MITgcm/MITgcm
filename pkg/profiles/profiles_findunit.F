#include "CPP_EEOPTIONS.h"

      SUBROUTINE PROFILES_FINDUNIT( ioUnit, myThid )
C OUT:
C     ioUnit  (integer) :: unit number
C
C PROFILES_FINDUNIT returns a valid, unused unit number for f77 I/O
C The routine stops the program is an error occurs in the process
C of searching the I/O channels.
C
C Created: 03/20/99 adcroft@mit.edu

      IMPLICIT NONE

#include "EEPARAMS.h"

C Arguments
      INTEGER ioUnit
      INTEGER myThid
C Local
      INTEGER ii
      LOGICAL op
      INTEGER ios
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C Sweep through a valid range of unit numbers
      ioUnit=-1
      DO ii = 2000, 9999
       IF ( ioUnit.EQ.-1 ) THEN
C- skip reserved unit numbers
        IF (       ii.NE.errorMessageUnit
     &       .AND. ii.NE.standardMessageUnit
     &       .AND. ii.NE.scrUnit1   .AND. ii.NE.scrUnit2
     &       .AND. ii.NE.eeDataUnit .AND. ii.NE.modelDataUnit
     &     ) THEN
          INQUIRE(unit=ii,iostat=ios,opened=op)
          IF ( ios.NE.0 ) THEN
            WRITE(msgBuf,'(A,I4)')
     &        ' PROFILES_FINDUNIT: inquiring unit number =', ii
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A)')
     &        ' PROFILES_FINDUNIT: inquire statement failed!'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R PROFILES_FINDUNIT'
          ENDIF
          IF ( .NOT.op ) THEN
            ioUnit=ii
          ENDIF
        ENDIF
       ENDIF
      ENDDO

C Was there an available unit number
      IF ( ioUnit.EQ.-1 ) THEN
        WRITE(msgBuf,'(A)')
     &    ' PROFILES_FINDUNIT: could not find an available unit number!'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_FINDUNIT'
      ENDIF

      RETURN
      END
