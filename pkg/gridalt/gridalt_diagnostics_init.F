C $Header: /u/gcmpack/MITgcm/pkg/gridalt/gridalt_diagnostics_init.F,v 1.3 2010/01/12 21:31:15 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

CBOP
C     !ROUTINE: GRIDALT_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE GRIDALT_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GRIDALT_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for GRIDALT package
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

      diagName  = 'DPPHYS  '
      diagTitle = 'Pressure Thickness of Layers on Fizhi Grid'
      diagUnits = 'Pascal          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
