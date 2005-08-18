C$Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_fixed.F,v 1.6 2005/08/18 19:29:51 stephd Exp $
C$Name:  $

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

#ifdef ALLOW_GCHEM

C     !LOCAL VARIABLES:

         _BEGIN_MASTER( mythid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_DIC
          print*,'QQ load dic parameters, initial fixed' 
          call DIC_ABIOTIC_PARAM( myThid )
          call DIC_BIOTIC_PARAM( myThid )
#endif /* ALLOW_DIC */
#ifdef ALLOW_CFC
          print*,'QQ start cfc params'
          call CFC_PARAM( myThid )
          print*,'QQ read cfc atmos data'
          call CFC_ATMOS( myThid )
#endif /* ALLOW_CFC */
          _END_MASTER( mythid )

#ifdef ALLOW_DIAGNOSTICS
C     Define diagnostics Names :
      IF ( useDiagnostics ) THEN
        CALL GCHEM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GCHEM */

      RETURN
      END
