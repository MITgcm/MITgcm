#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_DIAGS(myTime,myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid, myIter
      _RL myTime

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM
cccccccccccccccccccccc
c diagnostics        c
cccccccccccccccccccccc

#ifdef DIC_BIOTIC
#ifdef ALLOW_TIMEAVE
       call dic_biotic_diags( myTime, myIter, myThid )
#endif
#endif

ccccccccccccccccccccccccc
c  monitor              c
ccccccccccccccccccccccccc
        call gchem_monitor(myIter, myTime, myThid )

#endif /* ALLOW_GCHEM */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
