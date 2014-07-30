C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_diagnostics_init.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

CBOP
C     !ROUTINE: KL10_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE KL10_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KL10_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for KL10 package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES :
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

        diagName  = 'KLviscAr'
        diagTitle = 'KL10 vertical eddy viscosity coefficient'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KLdiffKr'
        diagTitle = 'KL10 Vertical diffusion coefficient for tracers'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KLeps   '
        diagTitle = 'KL10 Turbulent dissipation rate'
        diagUnits = 'm^2/s^3           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
