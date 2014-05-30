C $Header: /u/gcmpack/MITgcm/pkg/sbo/sbo_check.F,v 1.6 2014/05/30 17:10:15 jmc Exp $
C $Name:  $

#include "SBO_OPTIONS.h"

      SUBROUTINE SBO_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE SBO_CHECK                                     |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     *==========================================================*

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Routine arguments ===
C     myThid   :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_SBO

C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'SBO_CHECK: #define ALLOW_SBO'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C     SBO assumes spherical polar coordinates
      IF ( usingCartesianGrid ) THEN
        WRITE(msgBuf,'(A)') 'SBO not implemented for Cartesian Grid'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SBO_CHECK'
      ENDIF
      IF ( usingCylindricalGrid ) THEN
        WRITE(msgBuf,'(A)') 'SBO not implemented for Cylindrical Grid'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SBO_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_SBO */

      RETURN
      END
