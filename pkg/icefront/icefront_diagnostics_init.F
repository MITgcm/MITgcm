C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_diagnostics_init.F,v 1.2 2010/01/22 00:51:54 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEFRONT_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE ICEFRONT_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "ICEFRONT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'ICFfwFlx'
      diagTitle = 'Ice shelf fresh water flux (positive upward)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ICFhtFlx'
      diagTitle = 'Ice shelf heat flux  (positive upward)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ICFForcT'
      diagTitle = 'Ice shelf forcing for theta, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ICFForcS'
      diagTitle = 'Ice shelf forcing for salt, >0 increases salt'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
