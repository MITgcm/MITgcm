C $Header: /u/gcmpack/MITgcm/pkg/atm_common/atm_diagnostics_init.F,v 1.4 2007/07/31 23:01:58 ce107 Exp $
C $Name:  $

#include "ATM_COMMON_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE ATM_DIAGNOSTICS_INIT(myThid )

C !DESCRIPTION:
C define diagnostics for atmosheric physics packages

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_ATM_COMMON
#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Define diagnostics Names :

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ATM_COMMON */

      RETURN
      END
