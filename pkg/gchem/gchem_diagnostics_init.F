C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_diagnostics_init.F,v 1.4 2010/05/14 21:52:34 jahn Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_DIAGNOSTICS_INIT(myThid )

C !DESCRIPTION:
C define diagnostics for gchem package
C experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_GCHEM
#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Define diagnostics Names :

C     diagnostics should be initialized in each package called by gchem

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_GCHEM */

      RETURN
      END
