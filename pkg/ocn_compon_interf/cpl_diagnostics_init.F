#include "OCN_CPL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: CPL_DIAGNOSTICS_INIT

C !INTERFACE:
      SUBROUTINE CPL_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of available diagnostics
C     for Coupled Oceanic Component

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
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
#ifdef ALLOW_THSICE
      INTEGER       numFract, numThick
#endif /* ALLOW_THSICE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'CPL_SLPr'
        diagTitle = 'Atmos Sea-Level pressure anomaly (recv fr Coupler)'
        diagUnits = 'Pa              '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_Qnet'
        diagTitle = 'Net surface heat flux (+=up) (recv from Coupler)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_Qsw '
        diagTitle = 'Net shortwave heat flux (+=up)(recv from Coupler)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_tauX'
        diagTitle = 'Zonal  surface wind-stress (recv from Coupler)'
        diagUnits = 'N/m^2           '
        diagCode  = 'UM      M1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'CPL_tauY'
        diagTitle = 'Merid. surface wind-stress (recv from Coupler)'
        diagUnits = 'N/m^2           '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

        diagName  = 'CPL_FWat'
        diagTitle = 'Surface fresh-water flux (+=up)(recv from Coupler)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_sFlx'
        diagTitle = 'Salt flux from seaice comp (+=up)(recv fr Coupler)'
        diagUnits = 'g/m^2/s         '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_iceM'
        diagTitle = 'Seaice mass       (recv from Coupler)'
        diagUnits = 'kg/m^2          '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_SALT_PLUME
        diagName  = 'CPL_sPlm'
        diagTitle = 'Salt-plume flux (+=down) (recv from Coupler)'
        diagUnits = 'g/m^2/s         '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIC
        diagName  = 'CPL_RnOf'
        diagTitle = 'river Run-Off (for DIC, +=down)(recv from Coupler)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_aCO2'
        diagTitle = 'Atmospheric CO2 (parts by vol)(recv from Coupler)'
        diagUnits = 'ppm             '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'CPL_wSpd'
        diagTitle = 'Atmospheric surf. wind speed (recv from Coupler)'
        diagUnits = 'm/s             '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_DIC */

#if (defined ALLOW_DIC) || (defined ALLOW_THSICE)
        diagName  = 'CPL_icFr'
        diagTitle = 'Seaice fraction (Atmos, recv from Coupler)'
        diagUnits = '0-1             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_DIC or ALLOW_THSICE */
#ifdef ALLOW_THSICE
        numFract  = diagNum

        diagName  = 'CPL_iceH'
        diagTitle = 'Seaice thickness (Atmos, recv from Coupler)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numFract, myThid )
        numThick  = diagNum

        diagName  = 'CPL_snwH'
        diagTitle = 'Snow thickness over seaice (ATM, recv fr Coupler)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'CPL_Qic1'
        diagTitle ='Enthalpy of seaice layer 1 (Atmos, recv fr Coupler)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

        diagName  = 'CPL_Qic2'
        diagTitle ='Enthalpy of seaice layer 2 (Atmos, recv fr Coupler)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, numThick, myThid )
#endif /* ALLOW_THSICE */

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
