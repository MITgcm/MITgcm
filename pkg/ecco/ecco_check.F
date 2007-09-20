
#include "CPP_OPTIONS.h"

      SUBROUTINE ECCO_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE ECCO_CHECK                                    |
C     | o Check runtime activated packages have been built in.   |
C     |==========================================================|
C     | All packages can be selected/deselected at build time    |
C     | ( when code is compiled ) and activated/deactivated at   |
C     | runtime. This routine does a quick check to trap packages|
C     | that were activated at runtime but that were not compiled|
C     | in at build time.                                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
#include "profiles.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
      IF ( ndaysrec .GT. maxNumDays ) THEN
       WRITE(msgBuf,'(2A,2I)') 
     &        'ECCO_CHECK: for ALLOW_TRANSPORT_COST_CONTRIBUTION: ',
     &        'ndaysrec > maxNumDays in ecco_cost.h ',
     &        ndaysrec, maxNumDays
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid )
       CALL PRINT_ERROR( msgBuf , myThid )
       STOP 'ABNORMAL END: S/R ECCO_CHECK'
      ENDIF
#endif

      _END_MASTER(myThid)

      RETURN
      END
