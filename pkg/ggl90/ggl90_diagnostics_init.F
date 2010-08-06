C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_diagnostics_init.F,v 1.3 2010/08/06 18:37:05 gforget Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GGL90_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE GGL90_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagMate  :: diag. mate number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
c     INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        diagName  = 'GGL90TKE'
        diagTitle = 'GGL90 sub-grid turbulent kinetic energy'
        diagUnits = 'm^2/s^2         '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90Lmx'
        diagTitle = 'Mixing length scale              '
        diagUnits = 'm               '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90Prl'
        diagTitle = 'Prandtl number used in GGL90'
        diagUnits = '1               '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90ArU'
        diagTitle = 'GGL90 eddy viscosity at U-point'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90ArV'
        diagTitle = 'GGL90 eddy viscosity at V-point'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90Kr'
        diagTitle = 'GGL90 diffusion coefficient for temperature'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
