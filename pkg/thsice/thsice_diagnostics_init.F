#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE THSICE_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for THSICE package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
c #include "SIZE.h"
c #include "PARAMS.h"
#include "THSICE_SIZE.h"
c #include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  My Thread Id. number
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
      INTEGER       numFract, numThick, numFrcFx
      INTEGER       n
      LOGICAL       flag
      CHARACTER*16  locName
      CHARACTER*4   THSICE_DIAG_SUFX, diagSufx
      EXTERNAL      THSICE_DIAG_SUFX

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'SI_Fract'
        diagTitle = 'Sea-Ice fraction  [0-1]'
        diagUnits = '0-1             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        numFract  = diagNum
        numFrcFx  = diagNum

        diagName  = 'SI_Thick'
        diagTitle = 'Sea-Ice thickness (area weighted average)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )
        numThick  = diagNum

        diagName  = 'SI_SnowH'
        diagTitle = 'Snow thickness over Sea-Ice (area weighted)'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'SI_Tsrf '
        diagTitle = 'Surface Temperature over Sea-Ice (area weighted)'
        diagUnits = 'degC            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'SI_Tice1'
        diagTitle = 'Sea-Ice Temperature, 1srt layer (area weighted)'
        diagUnits = 'degC            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'SI_Tice2'
        diagTitle = 'Sea-Ice Temperature, 2nd  layer (area weighted)'
        diagUnits = 'degC            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'SI_Qice1'
        diagTitle = 'Sea-Ice enthalpy, 1srt layer (mass weighted)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

        diagName  = 'SI_Qice2'
        diagTitle = 'Sea-Ice enthalpy, 2nd  layer (mass weighted)'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

        diagName  = 'SIsnwAge'
        diagTitle = 'snow age over Sea-Ice'
        diagUnits = 's               '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIalbedo'
        diagTitle = 'Sea-Ice Albedo [0-1] (area weighted average)'
        diagUnits = '0-1             '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFrcFx, myThid )

        diagName  = 'SIsnwPrc'
        diagTitle = 'snow precip. (+=dw) over Sea-Ice (area weighted)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFrcFx, myThid )

        diagName  = 'SIflxAtm'
        diagTitle = 'net heat flux from the Atmosphere (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIfrwAtm'
        diagTitle = 'fresh-water flux to the Atmosphere (+=up)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIflx2oc'
        diagTitle = 'heat flux out of the ocean (+=up)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIfrw2oc'
        diagTitle = 'fresh-water flux out of the ocean (+=up)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIsaltFx'
        diagTitle = 'salt flux out of the ocean (+=up)'
        diagUnits = 'g/m^2/s         '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SItOcMxL'
        diagTitle = 'ocean mixed layer temperature'
        diagUnits = 'degC            '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

        diagName  = 'SIsOcMxL'
        diagTitle = 'ocean mixed layer salinity'
        diagUnits = 'g/kg            '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       DO n=-5,-9,-1
        flag = .TRUE.
        IF     ( n .EQ. GAD_SI_FRAC  ) THEN
          locName   = 'SeaIce Fraction '
          diagUnits = 'm^2/s           '
        ELSEIF ( n .EQ. GAD_SI_HSNOW ) THEN
          locName   = 'Snow Thickness  '
          diagUnits = 'm^3/s           '
        ELSEIF ( n .EQ. GAD_SI_HICE  ) THEN
          locName   = 'SeaIce Thickness'
          diagUnits = 'm^3/s           '
        ELSEIF ( n .EQ. GAD_SI_QICE1 ) THEN
          locName   = 'Ice enthalpy l=1'
          diagUnits = 'J/kg.m^3/s      '
c         diagUnits = 'W.m^3/kg        '
        ELSEIF ( n .EQ. GAD_SI_QICE2 ) THEN
          locName   = 'Ice enthalpy l=2'
          diagUnits = 'J/kg.m^3/s      '
        ELSE
          flag = .FALSE.
        ENDIF
        IF ( flag ) THEN
C-     Advective flux:
        diagSufx = THSICE_DIAG_SUFX( n, myThid )
        diagName  = 'ADVx'//diagSufx
        diagTitle = 'Zonal      Advective Flux of '//locName
        diagCode  = 'UU      M1      '
        diagMate  = diagNum + 2
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        diagName  = 'ADVy'//diagSufx
        diagTitle = 'Meridional Advective Flux of '//locName
        diagCode  = 'VV      M1      '
        diagMate  = diagNum
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
        ENDIF
      ENDDO

C--   Intermediate State, just before advection:
        locName   = 'before advection'
        diagName  = 'SI_AdvFr'
        diagTitle = 'Sea-Ice fraction  [0-1] ('//locName//')'
        diagUnits = '0-1             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )
        numFract  = diagNum

        diagName  = 'SI_AdvHi'
        diagTitle = 'Sea-Ice thickness ('//locName//')'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )
        numThick  = diagNum

        diagName  = 'SI_AdvHs'
        diagTitle = 'Snow thickness over Sea-Ice ('//locName//')'
        diagUnits = 'm               '
        diagCode  = 'SM PC   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numFract, myThid )

        diagName  = 'SI_AdvQ1'
        diagTitle = 'Sea-Ice enthalpy, 1srt layer ('//locName//')'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

        diagName  = 'SI_AdvQ2'
        diagTitle = 'Sea-Ice enthalpy, 2nd  layer ('//locName//')'
        diagUnits = 'J/kg            '
        diagCode  = 'SM  C   M1      '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numThick, myThid )

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: THSICE_DIAG_SUFX

C     !INTERFACE:
      CHARACTER*4 FUNCTION THSICE_DIAG_SUFX( tracerId, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION THSICE_DIAG_SUFX
C     | o Return diagnostic suffix (4 character long) for the
C     |   "tracerId" tracer (used to build diagnostic names).
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "THSICE_SIZE.h"

C     !INPUT PARAMETERS:
C     tracerId   ::  tracer identifier
C     myThid     ::  my thread Id number
      INTEGER      tracerId
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:

C--   Set diagnostic suffix (4 character long) for the "tracerId" tracer
      IF    ( tracerId.EQ.GAD_SI_FRAC  ) THEN
        THSICE_DIAG_SUFX = 'SIfr'
      ELSEIF( tracerId.EQ.GAD_SI_HSNOW ) THEN
        THSICE_DIAG_SUFX = 'SIhs'
      ELSEIF( tracerId.EQ.GAD_SI_HICE  ) THEN
        THSICE_DIAG_SUFX = 'SIhi'
      ELSEIF( tracerId.EQ.GAD_SI_QICE1 ) THEN
        THSICE_DIAG_SUFX = 'SIq1'
      ELSEIF( tracerId.EQ.GAD_SI_QICE2 ) THEN
        THSICE_DIAG_SUFX = 'SIq2'
      ELSE
        THSICE_DIAG_SUFX = 'iiii'
      ENDIF

      RETURN
      END
