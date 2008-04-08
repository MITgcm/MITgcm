C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_vari.F,v 1.10 2008/04/08 21:00:17 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_INIT_VARI(myThid )

C !DESCRIPTION:
C calls subroutines that initialize any time dependent variables
C for any tracer experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "GRID.h"
# include "DYNVARS.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM

ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_DIC
      IF ( useDIC ) THEN
          print*,'QQ load dic parameters, initial vari' 
          CALL DIC_INI_FORCING(myThid)
          CALL DIC_SURFFORCING_INIT(myThid)
          CALL DIC_BIOTIC_INIT(myThid )
      ENDIF
#endif /* ALLOW_DIC */

#endif /* ALLOW_GCHEM */

      RETURN
      END
