C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_fixed.F,v 1.9 2008/04/06 20:51:10 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_FIXED(myThid )

C !DESCRIPTION:
C calls subroutines that initialized fixed variables for any tracer
C experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM

C     !LOCAL VARIABLES:

C jmc: simpler to put MASTER thread directive within individual S/R :
C      that leave the choice, for each S/R, to do either single (master)
C      thread tasks or Multi-threaded tasks.
c        _BEGIN_MASTER( mythid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
          print*,'QQ load dic parameters, initial fixed'
          CALL DIC_ABIOTIC_PARAM( myThid )
          CALL DIC_BIOTIC_PARAM( myThid )
          CALL DIC_INIT_FIXED( myThid )
      ENDIF
#endif /* ALLOW_DIC */
#ifdef ALLOW_CFC
      IF ( useCFC ) THEN
          print*,'QQ start cfc params'
          CALL CFC_PARAM( myThid )
          print*,'QQ read cfc atmos data'
          CALL CFC_ATMOS( myThid )
      ENDIF
#endif /* ALLOW_CFC */
c         _END_MASTER( mythid )

#ifdef ALLOW_DIAGNOSTICS
C     Define diagnostics Names :
      IF ( useDiagnostics ) THEN
        CALL GCHEM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GCHEM */

      RETURN
      END
