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
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_ABIOTIC.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid, myIter
      _RL myTime

#ifdef ALLOW_PTRACERS
cccccccccccccccccccccc
c diagnostics        c
cccccccccccccccccccccc
#ifdef DIC_BIOTIC
#ifdef ALLOW_TIMEAVE
       call dic_biotic_diags( myTime, myIter, myThid )
#endif
cswdcost -- add --
       call dic_diags( myTime, myIter, myThid )
cswdcost -- end add ---
#endif


ccccccccccccccccccccccccc
c  monitor              c
ccccccccccccccccccccccccc
        call gchem_monitor(myIter, myTime, myThid )


#endif /* ALLOW_PTRACERS */

      RETURN
      END
