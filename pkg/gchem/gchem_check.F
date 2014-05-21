C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_check.F,v 1.7 2014/05/21 19:31:23 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

      SUBROUTINE GCHEM_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE GCHEM_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"

C     === Routine arguments ===
C     myThid   ::  Number of this instance
      INTEGER myThid

#ifdef ALLOW_GCHEM

C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'GCHEM_CHECK: #define ALLOW_GCHEM'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

#ifdef GCHEM_SEPARATE_FORCING
      IF ( useCFC ) THEN
        WRITE(msgBuf,'(A)') 'GCHEM_SEPARATE_FORCING not coded for CFC'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF
#else  /* GCHEM_SEPARATE_FORCING */
      IF ( useDIC ) THEN
        WRITE(msgBuf,'(A)') 'GCHEM_SEPARATE_FORCING needed to use DIC'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF
      IF ( useDARWIN ) THEN
        WRITE(msgBuf,'(A)') 'GCHEM_SEPARATE_FORCING needed for DARWIN'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF
#endif /* GCHEM_SEPARATE_FORCING */

C     GCHEM needs passive tracer package
      IF ( .NOT.usePTRACERS) THEN
        WRITE(msgBuf,'(A)') 'usePTRACERS has to be .TRUE. for GCHEM'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

C     GCHEM cannot (yet!) run cfc and dic together
      IF ( useDIC .AND. useCFC ) THEN
        WRITE(msgBuf,'(A)') 'useDIC and useCFC cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

C     GCHEM cannot run darwin and dic together
      IF ( useDARWIN .AND. useDIC ) THEN
        WRITE(msgBuf,'(A)') 'useDARWIN and useDIC cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

C     GCHEM cannot run darwin and cfc together
      IF ( useDARWIN .AND. useCFC ) THEN
        WRITE(msgBuf,'(A)') 'useDARWIN and useCFC cannot both be .TRUE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R GCHEM_CHECK'
      ENDIF

      _END_MASTER(myThid)

#ifdef ALLOW_CFC
C     run checks specific to CFC
      IF ( useCFC ) THEN
        CALL CFC_CHECK( myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
C     run checks specific to darwin
      IF ( useDARWIN ) THEN
        CALL DARWIN_CHECK( myThid )
      ENDIF
#endif

#endif /* ALLOW_GCHEM */

      RETURN
      END
