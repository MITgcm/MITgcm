C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_diagnostics_init.F,v 1.2 2007/11/28 17:55:36 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
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

      diagName  = 'PLUMEKB '
      diagTitle = 'fractional plume: [0-1] (unitless)'
      diagUnits = '                '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'oceSPflx'
      diagTitle = 'net surface Salt flux rejected into the ocean during'
     &          //' freezing, (+=down),'
     &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'oceSPDep'
      diagTitle = 'Salt plume depth based on density criterion (>0)'
      diagUnits = 'm               '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
