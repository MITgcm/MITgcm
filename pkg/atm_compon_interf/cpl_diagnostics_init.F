#include "ATM_CPL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: CPL_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE CPL_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of available diagnostics
C     for Coupled Atmospheric Component

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "CPL_PARAMS.h"

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
c     INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#ifdef ALLOW_THSICE
      INTEGER       numFract, numThick
#endif /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'CPL_MxlD'
        diagTitle = 'Ocean mixed-layer depth [m] (recv from Coupler)'
        diagUnits = 'm               '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_SST '
        diagTitle = 'Ocean surface temperature (recv from Coupler)'
        diagUnits = 'degC            '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_SSS '
        diagTitle = 'Ocean surface salinity    (recv from Coupler)'
        diagUnits = 'g/kg            '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_vSq '
        diagName  = 'CPL_Vel2'
        diagTitle = 'Ocean surface velocity square (recv from Coupler)'
        diagUnits = 'm^2/s^2         '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_AIM
        diagName  = 'CPL_fCO2'
        diagTitle ='Ocean air-sea flux of CO2 (+=down)(recv fr Coupler)'
        diagUnits = 'mol/m^2/s       '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_THSICE
        diagName  = 'CPL_icFr'
        diagTitle = 'Seaice fraction (Ocean, recv from Coupler)'
        diagUnits = '0-1             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        numFract  = diagNum

        diagName  = 'CPL_iceH'
        diagTitle = 'Seaice thickness (Ocean, recv from Coupler)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numFract, myThid )
        numThick  = diagNum

        diagName  = 'CPL_snwH'
        diagTitle = 'Snow thickness over seaice (OCN, recv fr Coupler)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'CPL_Qic1'
        diagTitle ='Enthalpy of seaice layer 1 (Ocean, recv fr Coupler)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

        diagName  = 'CPL_Qic2'
        diagTitle ='Enthalpy of seaice layer 2 (Ocean, recv fr Coupler)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numThick, myThid )
#endif /* ALLOW_THSICE */

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
