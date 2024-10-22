#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: OBSFIT_FINDUNIT

C     !INTERFACE: 
      SUBROUTINE OBSFIT_FINDUNIT( ioUnit, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Returns a valid, unused unit number for f77 I/O
C     | The routine stops the program is an error occurs in the process
C     | of searching the I/O channels
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     ioUnit    :: unit number
C     myThid    :: my Thread Id number
      INTEGER ioUnit
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER ii
      LOGICAL op
      INTEGER ios
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C Sweep through a valid range of unit numbers
      ioUnit=-1
      DO ii=2000,9999
       IF ( ioUnit.EQ.-1 ) THEN
C Skip reserved unit numbers
        IF (       ii.NE.errorMessageUnit
     &       .AND. ii.NE.standardMessageUnit
     &       .AND. ii.NE.scrUnit1   .AND. ii.NE.scrUnit2
     &       .AND. ii.NE.eeDataUnit .AND. ii.NE.modelDataUnit
     &     ) THEN
          INQUIRE(unit=ii,iostat=ios,opened=op)
          IF ( ios.NE.0 ) THEN
            WRITE(msgBuf,'(A,I4)')
     &        ' OBSFIT_FINDUNIT: inquiring unit number =', ii
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A)')
     &        ' OBSFIT_FINDUNIT: inquire statement failed!'
            CALL PRINT_ERROR( msgBuf, myThid )
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R OBSFIT_FINDUNIT'
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
     &    ' OBSFIT_FINDUNIT: could not find an available unit number!'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R OBSFIT_FINDUNIT'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|