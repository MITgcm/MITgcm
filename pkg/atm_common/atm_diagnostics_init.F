C $Header: /u/gcmpack/MITgcm/pkg/atm_common/atm_diagnostics_init.F,v 1.5 2010/01/21 01:47:32 jmc Exp $
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
c     INTEGER       diagNum
c     INTEGER       diagMate
c     CHARACTER*8   diagName
c     CHARACTER*16  diagCode
c     CHARACTER*16  diagUnits
c     CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Define diagnostics Names :

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ATM_COMMON */

      RETURN
      END
