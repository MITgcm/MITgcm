#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  calls routines which read in fields needed for any tracer experiment
C !USES: ===============================================================
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current iteration
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
CEOP

#ifdef ALLOW_GCHEM

cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
       CALL DIC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_BLING
      IF ( useBLING ) THEN
       CALL BLING_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
       CALL CFC_FIELDS_LOAD( myTime, myIter, myThid )
      ENDIF
#endif

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
       CALl DARWIN_FIELDS_LOAD(myIter,myTime,myThid)
      ENDIF
#endif

#endif /* ALLOW_GCHEM */

      RETURN
      END

