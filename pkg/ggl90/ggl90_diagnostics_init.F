#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GGL90_DIAGNOSTICS_INIT
C !INTERFACE:
      SUBROUTINE GGL90_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | Initialize list of all available diagnostics
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
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
CEOP
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

        diagName  = 'GGL90Kr '
        diagTitle = 'GGL90 diffusion coefficient for temperature'
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'GGL90flx'
        diagTitle = 'Surface flux of TKE                       '
        diagUnits = 'm^3/s^3         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )


        diagName  = 'GGL90tau'
        diagTitle = 'Work done by the wind                     '
        diagUnits = 'm^3/s^3         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_GGL90_IDEMIX
        diagName  = 'IDEMIX_E'
        diagTitle = 'IDEMIX internal wave energy            '
        diagUnits = 'm^2/s^2         '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEMIX_c'
        diagTitle = 'IDEMIX vertical group velocity             '
        diagUnits = 'm/s             '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEMIX_v'
        diagTitle = 'IDEMIX horizontal group velocity           '
        diagUnits = 'm/s             '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEMIX_t'  ! m^2/s^3 /(m^4/s^4)
        diagTitle = 'IDEMIX dissipation constant                '
        diagUnits = 's/m^2           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEMIX_K'
        diagTitle = 'IDEMIX vertical diffusivity                '
        diagUnits = 'm^2/s           '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEMIX_F'
        diagTitle = 'IDEMIX Forcing by gm                       '
        diagUnits = 'm^2/s^3         '
        diagCode  = 'SM      LR      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEM_F_b'
        diagTitle = 'Tidal forcing at bottom                    '
        diagUnits = 'm^3/s^3         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEM_F_s'
        diagTitle = 'Wind forcing at surface                   '
        diagUnits = 'm^3/s^3         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'IDEM_F_g'
        diagTitle = 'Integrated GM forcing                     '
        diagUnits = 'm^3/s^3         '
        diagCode  = 'SM      L1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif

#ifdef ALLOW_AUTODIFF
      diagName  = 'ADJtke90'
      diagTitle = 'dJ/dTKE: Sensitivity to GGL90 TKE'
      diagUnits = 'dJ/(m^2/s^2)   '
      diagCode  = 'SMRA    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
