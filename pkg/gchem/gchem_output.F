#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_DARWIN
#include "DARWIN_OPTIONS.h"
#endif

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_OUTPUT( myTime, myIter, myThid )

C !DESCRIPTION:
C calls subroutine that calculate diagnostic specific to
C any tracer epxperiment
C also calls gchem monitor

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myThid   :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

cccccccccccccccccccccc
c diagnostics        c
cccccccccccccccccccccc

#ifdef ALLOW_DARWIN
#ifdef ALLOW_TIMEAVE
      IF ( useDARWIN ) THEN
       CALL DARWIN_DIAGS( myTime, myIter, myThid )
#ifdef ALLOW_CARBON
       CALL DIC_DIAGS( myTime, myIter, myThid )
#endif
      ENDIF
#endif
#endif

      RETURN
      END
