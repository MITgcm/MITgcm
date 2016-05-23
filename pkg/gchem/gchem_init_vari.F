C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_init_vari.F,v 1.15 2016/05/23 13:09:31 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif
#ifdef ALLOW_DARWIN
# include "DARWIN_OPTIONS.h"
#endif

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
#ifdef ALLOW_AUTODIFF
# include "GRID.h"
# include "DYNVARS.h"
#endif /* ALLOW_AUTODIFF */

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM

ccccccccccccccccccccccccccc
c initialize parameters   c
ccccccccccccccccccccccccccc
#ifdef ALLOW_DIC
# ifdef ALLOW_AUTODIFF
      IF ( .NOT.useDIC ) STOP 'ABNORMAL END: S/R GCHEM_INIT_VARI'
# else /* ALLOW_AUTODIFF */
      IF ( useDIC ) THEN
# endif /* ALLOW_AUTODIFF */
         CALL DIC_INIT_VARIA(myThid)
         CALL DIC_INI_FORCING(myThid)
         CALL DIC_SURFFORCING_INIT(myThid)
         CALL DIC_BIOTIC_INIT(myThid)
# ifdef ALLOW_CTRL
         CALL DIC_SET_CONTROL(myThid)
# endif
# ifndef ALLOW_AUTODIFF
      ENDIF
# endif
#endif /* ALLOW_DIC */

#ifdef ALLOW_BLING
# ifndef ALLOW_AUTODIFF_TAMC
      IF ( useBLING ) THEN
# endif
         CALL BLING_INIT_VARIA(myThid)
         CALL BLING_INI_FORCING(myThid)
         CALL BLING_CARBONATE_INIT(myThid)
# ifndef ALLOW_AUTODIFF_TAMC
      ENDIF
# endif
#endif /* ALLOW_BLING */

#ifdef ALLOW_DARWIN
      IF ( useDARWIN ) THEN
         CALL DARWIN_INIT_VARI(myThid )
#ifdef ALLOW_CARBON
         CALL DIC_SURFFORCING_INIT(myThid)
         CALL DIC_DIAGS_INIT(myThid)
#endif
      ENDIF
#endif
#endif /* ALLOW_GCHEM */

      RETURN
      END
