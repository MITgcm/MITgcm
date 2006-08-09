C$Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_vari.F,v 1.7 2006/08/09 01:24:51 jmc Exp $
C$Name:  $

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
          print*,'QQ load dic parameters, initial vari' 
#ifdef ALLOW_AUTODIFF_TAMC
c these two needed to be recalled for adjoint
          call DIC_BIOTIC_PARAM(myThid)
          call DIC_ABIOTIC_PARAM(myThid)
#endif /* ALLOW_AUTODIFF_TAMC */
c -- 
          CALL DIC_INI_FORCING(myThid)
          call DIC_SURFFORCING_INIT(myThid)
          call DIC_BIOTIC_INIT(myThid )
#endif /* ALLOW_DIC */


#endif /* ALLOW_GCHEM */

      RETURN
      END
