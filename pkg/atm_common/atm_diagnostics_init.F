C$Header: /u/gcmpack/MITgcm/pkg/atm_common/atm_diagnostics_init.F,v 1.2 2005/01/28 01:07:15 jmc Exp $
C$Name:  $

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

      diagName  = 'DTCONV  '
      diagTitle = 'Temp Change due to Convection (K/sec)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQCONV  '
      diagTitle = 'Humidity Change due to Convection (g/kg/sec)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity (fraction)'
      diagUnits = 'Fraction        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECLS  '
      diagTitle = 'Large Scale Precipitation (g/m2/sec)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ENPREC  '
      diagTitle = 'Energy of Precipitation (snow, rain Temp) in (J/g)'
      diagUnits = 'J/g             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ATM_COMMON */

      RETURN
      END
