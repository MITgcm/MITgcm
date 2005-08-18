C$Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_vari.F,v 1.6 2005/08/18 19:29:51 stephd Exp $
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
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM

         _BEGIN_MASTER( mythid )
ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_DIC
          print*,'QQ load dic parameters, initial vari' 
c these two needed to be recalled for adjoint
          call DIC_BIOTIC_PARAM(myThid)
          call DIC_ABIOTIC_PARAM(myThid)
c -- 
          CALL DIC_INI_FORCING(myThid)
          call DIC_SURFFORCING_INIT(myThid)
          call DIC_BIOTIC_INIT(myThid )
#endif /* ALLOW_DIC */
          _END_MASTER( mythid )


#endif /* ALLOW_GCHEM */

      RETURN
      END
