C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_check.F,v 1.4 2001/02/04 14:38:49 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE GMREDI_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_CHECK                                  |
C     | o Check dependances with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_GMREDI
      IF (GMRediIsOn) THEN

       WRITE(msgBuf,'(A)') 'GMREDI_CHECK: #define GMREDI'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C      GM/Redi needs implicit diffusion (will be packaged later)
       IF (.NOT.implicitDiffusion) THEN
        WRITE(msgBuf,'(A)') 'GM/Redi needs implicitDiffusion=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R GMREDI_CHECK'
       ENDIF

      ENDIF

#endif
      RETURN
      END
