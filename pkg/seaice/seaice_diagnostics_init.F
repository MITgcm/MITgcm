C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diagnostics_init.F,v 1.2 2006/10/23 16:29:48 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE SEAICE_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for SEAICE package
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

      diagName  = 'SIarea  '
      diagTitle = 'SEAICE fractional ice-covered area [0 to 1]'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'SIheff  '
      diagTitle = 'SEAICE effective ice thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIuice  '
      diagTitle = 'SEAICE zonal ice velocity, >0 from West to East'
      diagUnits = 'm/s             '
      diagCode  = 'UU      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'SIvice  '
      diagTitle = 'SEAICE merid. ice velocity, >0 from South to North'
      diagUnits = 'm/s             '
      diagCode  = 'VV      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIhsnow '
      diagTitle = 'SEAICE snow thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, myThid )



      diagName  = 'SIuwind '
      diagTitle = 'SEAICE zonal 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIvwind '
      diagTitle = 'SEAICE meridional 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIfu    '
      diagTitle = 'SEAICE zonal surface wind stress, >0 increases uVel '
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIfv    '
      diagTitle = 'SEAICE merid. surface wind stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIempmr '
      diagTitle = 'SEAICE upward freshwater flux, > 0 increases salt'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SIqnet  '
      diagTitle = 'SEAICE upward heatflux, turb+rad, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      
      diagName  = 'SIqsw   '
      diagTitle = 'SEAICE upward shortwave radiat., >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, myThid )
      



#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
