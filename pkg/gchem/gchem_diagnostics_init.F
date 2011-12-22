C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_diagnostics_init.F,v 1.5 2011/12/22 19:05:27 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_DIAGNOSTICS_INIT( myThid )

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
c     INTEGER       diagNum
c     CHARACTER*8   diagName
c     CHARACTER*16  diagCode
c     CHARACTER*16  diagUnits
c     CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Define diagnostics Names :

C     diagnostics should be initialized in each package called by gchem

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_GCHEM */

      RETURN
      END
