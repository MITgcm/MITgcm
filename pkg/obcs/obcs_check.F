C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check.F,v 1.2 2001/02/02 21:36:30 adcroft Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CHECK                                    |
C     | o Check dependances with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OBCS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

#ifdef ALLOW_OBCS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#ifdef ALLOW_ORLANSKI
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_ORLANSKI'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)
#else
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: #undef OBCS_RADIATE_ORLANSKI  and'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: one of useOrlanski* logicals is true'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF 
#endif /* ALLOW_ORLANSKI */

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#endif /* ALLOW_OBCS */
      return
      end
