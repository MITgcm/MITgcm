C $Header: /u/gcmpack/MITgcm/pkg/land/land_diagnostics_init.F,v 1.4 2010/01/12 21:31:15 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE LAND_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAND_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for LAND package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
c #include "SIZE.h"
c #include "PARAMS.h"
c #include "LAND_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my thread Id number
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

c     IF ( useDiagnostics ) THEN

C--    Land state variables:
        diagName  = 'GrdSurfT'
        diagTitle = 'Surface Temperature over land'
        diagUnits = 'degC            '
        diagCode  = 'SM      Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GrdTemp '
        diagTitle = 'Ground Temperature at each level'
        diagUnits = 'degC            '
        diagCode  = 'SM      MG      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GrdEnth '
        diagTitle = 'Ground Enthalpy at each level'
        diagUnits = 'J/m3            '
        diagCode  = 'SM      MG      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GrdWater'
        diagTitle =
     &       'Ground Water (vs Field Capacity) Fraction at each level'
        diagUnits = '0-1             '
        diagCode  = 'SM P    MG      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c       cdiag(160) = 'SNOW    '
c       tdiag(160) = 'Snow Depth (mm water equivalent)'
c       udiag(160) = 'mm water equiv  '
c       gdiag(160) = 'SM      L1      '
c---
        diagName  = 'LdSnowH '
        diagTitle = 'Snow Thickness over land'
        diagUnits = 'm               '
        diagCode  = 'SM P    Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'LdSnwAge'
        diagTitle = 'Snow Age over land'
        diagUnits = 's               '
        diagCode  = 'SM P    Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--    Fluxes:

        diagName  = 'RUNOFF  '
        diagTitle = 'Run-Off per surface unit'
        diagUnits = 'kg/m2/s         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'EnRunOff'
        diagTitle = 'Energy flux associated with run-Off'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'landHFlx'
        diagTitle = 'net surface downward Heat flux over land'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'landPmE '
        diagTitle = 'Precipitation minus Evaporation over land'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'ldEnFxPr'
        diagTitle =
     &    'Energy flux (over land) associated with Precip (snow,rain)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      Lg      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
