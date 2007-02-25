C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_diagnostics_init.F,v 1.3 2007/02/25 20:00:30 jmc Exp $
C $Name:  $

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
c #include "THSICE_PARAMS.h"

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
      INTEGER       numFract, numThick

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c     IF ( useDiagnotics ) THEN

        diagName  = 'SI_Fract'
        diagTitle = 'Sea-Ice fraction  [0-1]'
        diagUnits = '0-1             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )
        numFract  = diagNum

        diagName  = 'SI_Thick'
        diagTitle = 'Sea-Ice thickness (area weighted average)'
        diagUnits = 'm               '
        WRITE(diagCode,'(A,I3.3,A)') 'SM PC', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )
        numThick  = diagNum

        diagName  = 'SI_SnowH'
        diagTitle = 'Snow thickness over Sea-Ice (area weighted)'
        diagUnits = 'm               '
        WRITE(diagCode,'(A,I3.3,A)') 'SM PC', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SI_Tsrf '
        diagTitle = 'Surface Temperature over Sea-Ice (area weighted)'
        diagUnits = 'degC            '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SI_Tice1'
        diagTitle = 'Sea-Ice Temperature, 1srt layer (area weighted)'
        diagUnits = 'degC            '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SI_Tice2'
        diagTitle = 'Sea-Ice Temperature, 2nd  layer (area weighted)'
        diagUnits = 'degC            '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SI_Qice1'
        diagTitle = 'Sea-Ice enthalpy, 1srt layer (mass weighted)'
        diagUnits = 'J/kg            '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numThick, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SI_Qice2'
        diagTitle = 'Sea-Ice enthalpy, 2nd  layer (mass weighted)'
        diagUnits = 'J/kg            '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numThick, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIalbedo'
        diagTitle = 'Sea-Ice Albedo [0-1] (area weighted average)'
        diagUnits = '0-1             '
        WRITE(diagCode,'(A,I3.3,A)') 'SM PC', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIsnwAge'
        diagTitle = 'snow age over Sea-Ice'
        diagUnits = 's               '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIsnwPrc'
        diagTitle = 'snow precip. (+=dw) over Sea-Ice (area weighted)'
        diagUnits = 'kg/m^2/s        '
        WRITE(diagCode,'(A,I3.3,A)') 'SM  C', numFract, 'M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIflxAtm'
        diagTitle = 'net heat flux from the Atmosphere (+=dw)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIfrwAtm'
        diagTitle = 'fresh-water flux to the Atmosphere (+=up)'
        diagUnits = 'kg/m^2/s        '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIflx2oc'
        diagTitle = 'heat flux out of the ocean (+=up)'
        diagUnits = 'W/m^2           '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIfrw2oc'
        diagTitle = 'fresh-water flux out of the ocean (+=up)'
        diagUnits = 'm/s             '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIsaltFx'
        diagTitle = 'salt flux out of the ocean (+=up)'
        diagUnits = 'psu.kg/m^2/s    '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SItOcMxL'
        diagTitle = 'ocean mixed layer temperature'
        diagUnits = 'degC            '
        diagCode  = 'SM      M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

        diagName  = 'SIsOcMxL'
        diagTitle = 'ocean mixed layer salinity'
        diagUnits = 'psu             '
        diagCode  = 'SM P    M1      '
        CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

c     ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
