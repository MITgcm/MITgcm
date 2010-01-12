C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_diagnostics_init.F,v 1.3 2010/01/12 21:34:09 jmc Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

CBOP
C     !ROUTINE: MY82_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE MY82_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for MY82 package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"

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

        diagName  = 'MYVISCAR'
        diagTitle = 'MY vertical eddy viscosity coefficient'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'MYDIFFKR'
        diagTitle = 'Vertical diffusion coefficient for salt & tracers'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'MYHBL   '
        diagTitle = 'MY82 boundary layer depth'
        diagUnits = 'm               '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )


#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
