#include "KPP_OPTIONS.h"
#ifdef ALLOW_SALT_PLUME
#include "SALT_PLUME_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: KPP_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE KPP_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for KPP package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
      CHARACTER*4   diagSufx
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        diagName  = 'KPPviscA'
        diagTitle = 'KPP vertical eddy viscosity coefficient'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPdiffS'
        diagTitle = 'Vertical diffusion coefficient for salt & tracers'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPdiffT'
        diagTitle = 'Vertical diffusion coefficient for heat'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPghatK'
        diagTitle = 'ratio of KPP non-local (salt) flux'
     &            //' relative to surface-flux'
        diagUnits = '0-1             '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPhbl  '
        diagTitle = 'KPP boundary layer depth, bulk Ri criterion'
        diagUnits = 'm               '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPfrac '
        diagTitle = 'Short-wave flux fraction penetrating mixing layer'
        diagUnits = '                '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPdbsfc'
        diagTitle = 'Buoyancy difference with respect to surface'
        diagUnits = 'm/s^2           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPbfsfc'
        diagTitle = 'Bo+radiation absorbed to d=hbf*hbl + plume'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPRi   '
        diagTitle = 'Bulk Richardson number'
        diagUnits = 'non-dimensional '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPbo   '
        diagTitle = 'Surface turbulent buoyancy forcing'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPbosol'
        diagTitle = 'surface radiative buoyancy forcing'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPdbloc'
        diagTitle = 'Local delta buoyancy across interfaces'
        diagUnits = 'm/s^2           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPshsq '
        diagTitle = 'Local shear (V(k-1)-V(k))^2'
        diagUnits = 'm^2/s^2         '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPdVsq '
        diagTitle = 'Shear relative to surface (V(ksrf)-V(k))**2'
        diagUnits = 'm^2/s^2         '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifndef EXCLUDE_KPP_DOUBLEDIFF
        diagName  = 'KPPnuddt'
        diagTitle = 'Vertical double diffusion coefficient for heat'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPnudds'
        diagTitle = 'Vertical double diffusion coefficient for salt'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ndef EXCLUDE_KPP_DOUBLEDIFF */

        diagSufx  = 'aaaa'
#ifdef ALLOW_GENERIC_ADVDIFF
        diagSufx  = GAD_DIAG_SUFX( GAD_TEMPERATURE, myThid )
#endif
        diagName  = 'KPPg'//diagSufx
        diagTitle = 'KPP non-local Flux of Pot.Temperature'
        diagUnits = 'degC.m^3/s      '
        diagCode  = 'WM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_GENERIC_ADVDIFF
        diagSufx  = GAD_DIAG_SUFX( GAD_SALINITY, myThid )
#endif
        diagName  = 'KPPg'//diagSufx
        diagTitle = 'KPP non-local Flux of Salinity'
        diagUnits = 'g/kg.m^3/s      '
        diagCode  = 'WM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_SALT_PLUME
        diagName  = 'KPPpfrac'
        diagTitle = 'Salt plume flux fraction penetrating mixing layer'
        diagUnits = '                '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'KPPboplm'
        diagTitle = 'Surface haline buoyancy forcing'
        diagUnits = 'm^2/s^3         '
#ifdef SALT_PLUME_VOLUME
        diagCode  = 'SM P    UR      '
#else /* SALT_PLUME_VOLUME */
        diagCode  = 'SM P     1      '
#endif /* SALT_PLUME_VOLUME */
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_SALT_PLUME */

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
