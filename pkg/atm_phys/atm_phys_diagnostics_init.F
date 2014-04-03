C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/atm_phys_diagnostics_init.F,v 1.2 2014/04/03 00:28:21 jmc Exp $
C $Name:  $

#include "ATM_PHYS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ATM_PHYS_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE ATM_PHYS_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics for pkg AtmPhys

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
c#include "ATM_PHYS_PARAMS.h"

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
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'AtPh_SST'
        diagTitle = 'Sea Surface Temperature in Atm_Phys package'
        diagUnits = 'K               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhdTdt'
        diagTitle = ' Pot.Temp. tendency from package ATM_PHYS'
        diagUnits = 'K/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhdQdt'
        diagTitle = ' Spec.Humid tendency from package ATM_PHYS'
        diagUnits = '1/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhdUdt'
        diagTitle = 'u-Velocity tendency from package ATM_PHYS'
        diagUnits = 'm/s^2           '
        diagCode  = 'UMR     MR      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'AtPhdVdt'
        diagTitle = 'v-Velocity tendency from package ATM_PHYS'
        diagUnits = 'm/s^2           '
        diagCode  = 'VMR     MR      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'AtPhDifT'
        diagTitle = 'ATM_PHYS: Vertical Diffusion for Temp & Q'
        diagUnits = 'unit            '
        diagCode  = 'WM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhDifM'
        diagTitle = 'ATM_PHYS: Vertical Diffusion for Momentum'
        diagUnits = 'unit            '
        diagCode  = 'WM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhCnvP'
        diagTitle = 'ATM_PHYS: Convective Precip'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhLscP'
        diagTitle = 'ATM_PHYS: Large-Scale Precip'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhInSR'
        diagTitle = 'ATM_PHYS: Incoming Shortwave Radiation (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    U1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhNTSR'
        diagTitle = 'ATM_PHYS: Net Top Shortwave Radiation (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    U1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhOLR '
        diagTitle = 'ATM_PHYS: Outgoing Longwave  Radiation (+=up)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    U1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhDSSR'
        diagTitle = 'ATM_PHYS: Downward Surface Shortwave Rad. (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    U1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhNSSR'
        diagTitle = 'ATM_PHYS: Net Surface Shortwave Radiation (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhDSLR'
        diagTitle = 'ATM_PHYS: Downward Surface Longwave Rad. (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhUSLR'
        diagTitle = 'ATM_PHYS: Upward Surface Longwave Rad. (+=up)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM P    L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhSens'
        diagTitle = 'ATM_PHYS: surface sensible heat flux (+=up)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhEvap'
        diagTitle = 'ATM_PHYS: surface evaporation'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhTauX'
        diagTitle = 'ATM_PHYS: Zonal comp of surface stress'
        diagUnits = 'unit            '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhTauY'
        diagTitle = 'ATM_PHYS: Meridional comp of surface stress'
        diagUnits = 'unit            '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--     Dargan BettsMiller Convection:
        diagName  = 'AtPhCAPE'
        diagTitle = 'ATM_PHYS: Convective Avail. Pot. Energy: CAPE'
        diagUnits = 'J/kg            '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhCnIn'
        diagTitle = 'ATM_PHYS: Convection inhibition'
        diagUnits = '1               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhKlzb'
        diagTitle = 'ATM_PHYS: Level of zero buoyancy (=klzb, Convect)'
        diagUnits = '1               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhConv'
        diagTitle = 'ATM_PHYS: B.M. Convection Flag '
     &            //'(0:no, 1:shallow, 2:Deep)'
        diagUnits = '1               '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhRlxT'
        diagTitle = 'ATM_PHYS: Temp. Relax. inv time-scale (Convection)'
        diagUnits = '1/s             '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhRlxQ'
        diagTitle = 'ATM_PHYS: Humid Relax. inv time-scale (Convection)'
        diagUnits = '1/s             '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPh_Trf'
        diagTitle = 'ATM_PHYS: Ref Temp for convection (Moist Adiabat)'
        diagUnits = 'K               '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPh_Qrf'
        diagTitle = 'ATM_PHYS: Ref Humid for convection (Saturated)'
        diagUnits = 'kg/kg           '
        diagCode  = 'SM      MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'AtPhdtTg'
        diagTitle = 'ATM_PHYS: vertical diff. tendency of temp.'
        diagUnits = 'K/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhdtQg'
        diagTitle = 'ATM_PHYS: vertical diff. tendency of spec.humid'
        diagUnits = '1/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        diagName  = 'AtPhDisH'
        diagTitle = 'ATM_PHYS: heating from dissipation'
        diagUnits = 'K/s             '
        diagCode  = 'SMR     MR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
