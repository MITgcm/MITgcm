#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_FIXED(myThid )

C !DESCRIPTION:
C calls subroutines that initialized fixed variables for any tracer
C experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM

         _BEGIN_MASTER( mythid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
          print*,'QQ load dic parameters, initial fixed' 
          call DIC_ABIOTIC_PARAM( myThid )
          call DIC_BIOTIC_PARAM( myThid )
#endif
#ifdef ALLOW_CFC
          print*,'QQ start cfc params'
          call CFC_PARAM( myThid )
          print*,'QQ read cfc atmos data'
          call CFC_ATMOS( myThid )
#endif
          _END_MASTER( mythid )


#endif /* ALLOW_GCHEM */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
