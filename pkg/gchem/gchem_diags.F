C $Header: /u/gcmpack/MITgcm/pkg/gchem/Attic/gchem_diags.F,v 1.6 2004/11/19 20:17:48 mlosch Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_DIAGS(myTime,myIter, myThid )

C !DESCRIPTION:
C calls subroutine that calculate diagnostic specific to
C any tracer epxperiment
C also calls gchem monitor 

C !USES: ===============================================================
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
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM
cccccccccccccccccccccc
c diagnostics        c
cccccccccccccccccccccc

#ifdef DIC_BIOTIC
#ifdef ALLOW_TIMEAVE
       IF (myIter.NE.nIter0)
     & call dic_biotic_diags( myTime, myIter, myThid )
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
