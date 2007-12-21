C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_diagnostics_init.F,v 1.3 2007/12/21 02:54:34 atn Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

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

        diagName  = 'KPPviscA'
        diagTitle = 'KPP vertical eddy viscosity coefficient'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPdiffS'
        diagTitle = 'Vertical diffusion coefficient for salt & tracers'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPdiffT'
        diagTitle = 'Vertical diffusion coefficient for heat'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPghat '
        diagTitle = 'Nonlocal transport coefficient'
        diagUnits = 's/m^2           '
        diagCode  = 'SM P    LR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPhbl  '
        diagTitle = 'KPP boundary layer depth, bulk Ri criterion'
        diagUnits = 'm               '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPmld  '
        diagTitle = 'Mixed layer depth, dT=.8degC density criterion'
        diagUnits = 'm               '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPfrac '
        diagTitle = 'Short-wave flux fraction penetrating mixing layer'
        diagUnits = '                '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPdbsfc'
        diagTitle = 'Buoyancy difference with respect to surface'
        diagUnits = 'm/s^2           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPbfsfc'
        diagTitle = 'Bo+radiation absorbed to d=hbf*hbl + plume'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPbo   '
        diagTitle = 'Surface turbulent buoyancy forcing'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPbosol'
        diagTitle = 'surface radiative buoyancy forcing'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPdbloc'
        diagTitle = 'Local delta buoyancy across interfaces'
        diagUnits = 'm/s^2           '
        diagCode  = 'SM P    UR      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#ifdef ALLOW_SALT_PLUME
        diagName  = 'KPPpfrac'
        diagTitle = 'Salt plume flux fraction penetrating mixing layer'
        diagUnits = '                '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'KPPboplm'
        diagTitle = 'Surface haline buoyancy forcing'
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM P     1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )
#endif /* ALLOW_SALT_PLUME */

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
