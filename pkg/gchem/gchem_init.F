#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT(myTime, myIter, myThid )

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

         _BEGIN_MASTER( mythid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
          print*,'QQQQQ load dic parameters, myiter', myiter
          call DIC_ABIOTIC_PARAM( myThid )
          call DIC_BIOTIC_PARAM( myThid )
          call DIC_BIOTIC_DIAGS(myTime, myIter, myThid )
#endif
          _END_MASTER( mythid )


#endif /* ALLOW_PTRACERS */

      RETURN
      END
