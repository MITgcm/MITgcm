#include "EXF_OPTIONS.h"

C--  File exf_check.F: Routines to check EXF settings
C--   Contents
C--   o EXF_CHECK
C--   o EXF_CHECK_INTERP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_CHECK
C     !INTERFACE:

      SUBROUTINE EXF_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXF_CHECK
C     | o Check parameters and other package dependences
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
CEOP

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(A)') 'EXF_CHECK: #define ALLOW_EXF'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Check for consistency, main parameters
      IF (.NOT.
     &     (exf_iprec.EQ.precFloat32 .OR. exf_iprec.EQ.precFloat64)
     &     ) THEN
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: value of exf_iprec not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( repeatPeriod.LT.0. ) THEN
C- Note: should check all {fld}RepCycle (not just common defaut repeatPeriod)
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: repeatPeriod must be positive'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( useExfYearlyFields ) THEN
       IF ( .NOT.useCAL ) THEN
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: ',
     &       'useExfYearlyFields requires to use pkg/cal (useCAL=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( repeatPeriod.NE.0. ) THEN
C- Note: should check all obcs{N,S,E,W}repCycle (not just default repeatPeriod)
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: The use of ',
     &       'useExfYearlyFields AND repeatPeriod is not implemented'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( useOBCS .AND. useOBCSYearlyFields ) THEN
       IF ( .NOT.useCAL ) THEN
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: ',
     &       'useOBCSYearlyFields requires to use pkg/cal (useCAL=T)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( repeatPeriod.NE.0. .AND. (OBCSNperiod.GE.0 .OR.
     &      OBCSSperiod.GE.0 .OR. OBCSWperiod.GE.0 .OR.
     &      OBCSEperiod.GE.0)) THEN
C- Note: should check all siob{N,S,E,W}repCycle (not just default repeatPeriod)
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: ',
     &       'useOBCSYearlyFields not implemented for repeatPeriod <> 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   For each field, check for parameter consistency:
C     a) stop when file is specified but not read-in;
C     b) print warning when file is read-in but not used within pkg/exf

C-    Check wind parameters:
      IF ( useAtmWind ) THEN
       IF ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     &   'EXF_CHECK: use u,v_wind components but not wind-stress'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

      IF ( .NOT.useAtmWind ) THEN
       IF ( uwindfile .NE. ' ' .OR. vwindfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     &   'EXF_CHECK: read-in wind-stress but not u,v_wind components'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

#if !(defined ALLOW_ATM_TEMP) || !(defined ALLOW_BULKFORMULAE)
      IF ( wspeedfile.NE.' ' .AND. .NOT.useAtmWind ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** EXF_CHECK: "wspeed" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

C-    Check other field parameters:

#ifdef ALLOW_ATM_TEMP
      IF ( hfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** EXF_CHECK: "hflux" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( sfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** EXF_CHECK: "sflux" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

# ifndef ALLOW_BULKFORMULAE
C-    atemp might be used (outside Bulk-Formulae), e.g. to make snow
c     IF ( atempfile.NE.' ' ) THEN
c       WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "atemp" ',
c    &    'field is loaded from file but not used within pkg/exf'
c       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
c    &                      SQUEEZE_RIGHT, myThid )
c     ENDIF
      IF ( aqhfile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "aqh" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
# endif /* ndef ALLOW_BULKFORMULAE */
#else /* ALLOW_ATM_TEMP */
      IF ( atempfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"atemp" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( aqhfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"aqh" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_ATM_TEMP */

#if (defined ALLOW_ATM_TEMP) && (defined ALLOW_READ_TURBFLUXES)
# ifdef ALLOW_BULKFORMULAE
      IF ( hs_file.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "hs" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( hl_file.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "hl" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
# endif /* ALLOW_BULKFORMULAE */
#else /* ALLOW_ATM_TEMP and ALLOW_READ_TURBFLUXES */
      IF ( hs_file.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"hs" without ALLOW_ATM_TEMP and ALLOW_READ_TURBFLUXES'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( hl_file.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"hl" without ALLOW_ATM_TEMP and ALLOW_READ_TURBFLUXES'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_ATM_TEMP and ALLOW_READ_TURBFLUXES */

#if !(defined ALLOW_ATM_TEMP) || !(defined EXF_READ_EVAP)
      IF ( evapfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"evap" without ALLOW_ATM_TEMP and EXF_READ_EVAP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef ALLOW_ATM_TEMP or ndef EXF_READ_EVAP */

#ifndef ALLOW_ATM_TEMP
      IF ( precipfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"precip" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( snowprecipfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"snowprecip" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
# ifndef SHORTWAVE_HEATING
      IF ( swfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"swflux" without ALLOW_ATM_TEMP or SHORTWAVE_HEATING'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( swdownfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"swdown" without ALLOW_ATM_TEMP or SHORTWAVE_HEATING'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
# endif /* ndef SHORTWAVE_HEATING */
      IF ( lwfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"lwflux" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( lwdownfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"lwdown" with #undef ALLOW_ATM_TEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef ALLOW_ATM_TEMP */

#ifdef ALLOW_DOWNWARD_RADIATION
# if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swdownfile.NE.' ' .AND. swfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "swdown" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
# endif /* ALLOW_ATM_TEMP or SHORTWAVE_HEATING */
# ifdef ALLOW_ATM_TEMP
      IF ( lwdownfile.NE.' ' .AND. lwfluxfile.NE.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "lwdown" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
# endif /* ALLOW_ATM_TEMP or SHORTWAVE_HEATING */
#else /* ALLOW_DOWNWARD_RADIATION */
      IF ( swdownfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"swdown" with #undef ALLOW_DOWNWARD_RADIATION'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( lwdownfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"lwdown" with #undef ALLOW_DOWNWARD_RADIATION'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifndef ATMOSPHERIC_LOADING
      IF ( apressurefile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"apressure" with #undef ATMOSPHERIC_LOADING'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef ATMOSPHERIC_LOADING */

#ifndef EXF_ALLOW_TIDES
      IF ( tidePotFile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"tidePot" with #undef EXF_ALLOW_TIDES'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef EXF_SEAICE_FRACTION */

#ifndef EXF_SEAICE_FRACTION
      IF ( areamaskfile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"areamask" with #undef EXF_SEAICE_FRACTION'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef EXF_SEAICE_FRACTION */

#ifndef ALLOW_RUNOFF
      IF ( runofffile.NE.' ' ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"runoff" with #undef ALLOW_RUNOFF'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ndef ALLOW_RUNOFF */

      IF ( runoftempfile.NE.' ' ) THEN
#ifndef ALLOW_RUNOFTEMP
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"runoftemp" with #undef ALLOW_RUNOFTEMP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* ndef ALLOW_RUNOFTEMP */
       IF ( runofffile.EQ.' ' ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: "runoftemp" ',
     &    'field is loaded from file but not used within pkg/exf'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

      IF ( saltflxfile.NE.' ' ) THEN
#ifndef ALLOW_SALTFLX
        WRITE(msgBuf,'(3A)') 'EXF_CHECK: Cannot read-in field ',
     &    '"saltflx" with #undef ALLOW_SALTFLX'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif /* ndef ALLOW_SALTFLX */
       IF ( useSEAICE .OR. useThSIce )  THEN
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: exf salt flux is not',
     &       ' allowed when using either pkg/seaice or pkg/thsice'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_ZENITHANGLE
      IF ( ( useExfZenIncoming .OR. select_ZenAlbedo.NE.0 ) .AND.
     &     ( usingCartesianGrid .OR. usingCylindricalGrid ) ) THEN
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ZENITHANGLE code ',
     &      'does not work for cartesian and cylindrical grids'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( select_ZenAlbedo.LT.0 .OR. select_ZenAlbedo.GT.3 ) THEN
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: unsupported ',
     &      'select_ZenAlbedo choice'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( select_ZenAlbedo.EQ.2 ) THEN
       WRITE(msgBuf,'(A,A)')
     &      '** WARNING ** EXF_CHECK: for daily mean albedo, ',
     &      'it is advised to use select_ZenAlbedo.EQ.1 instead of 2'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( select_ZenAlbedo.EQ.3 .AND. swdownperiod.GT.21600. ) THEN
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: using diurnal albedo ',
     &        'formula requires diurnal downward shortwave forcing'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( select_ZenAlbedo.EQ.3 .AND. swdownperiod.GT.3600. ) THEN
       WRITE(msgBuf,'(3A)') '** WARNING ** EXF_CHECK: ',
     &      'the diurnal albedo formula is likely not safe for such ',
     &      'coarse temporal resolution downward shortwave forcing'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#else /* ALLOW_ZENITHANGLE */
      IF ( useExfZenIncoming .OR. select_ZenAlbedo.NE.0 ) THEN
        WRITE(msgBuf,'(A,A)') 'EXF_CHECK: unsupported option',
     &        ' when ALLOW_ZENITHANGLE is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* ALLOW_ZENITHANGLE */

#ifdef USE_EXF_INTERPOLATION
      IF ( usingCartesianGrid ) THEN
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &      'USE_EXF_INTERPOLATION assumes latitude/longitude'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &      'input and output coordinates. Trivial to extend to'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &      'cartesian coordinates, but has not yet been done.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      CALL EXF_CHECK_INTERP( 'ustress', ustressfile,
     &         ustress_interpMethod, ustress_nlat,
     &         ustress_nlon, ustress_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'vstress', vstressfile,
     &         vstress_interpMethod, vstress_nlat,
     &         vstress_nlon, vstress_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'hflux', hfluxfile, hflux_interpMethod,
     &     hflux_nlat, hflux_nlon, hflux_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'sflux', sfluxfile, sflux_interpMethod,
     &     sflux_nlat, sflux_nlon, sflux_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'swflux', swfluxfile, swflux_interpMethod,
     &     swflux_nlat, swflux_nlon, swflux_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'runoff', runofffile, runoff_interpMethod,
     &     runoff_nlat, runoff_nlon, runoff_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'saltflx', saltflxfile,
     &         saltflx_interpMethod, saltflx_nlat,
     &         saltflx_nlon, saltflx_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'atemp', atempfile, atemp_interpMethod,
     &     atemp_nlat, atemp_nlon, atemp_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'aqh', aqhfile, aqh_interpMethod,
     &     aqh_nlat, aqh_nlon, aqh_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'hs', hs_file, hs_interpMethod,
     &     hs_nlat, hs_nlon, hs_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'hl', hl_file, hl_interpMethod,
     &     hl_nlat, hl_nlon, hl_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'evap', evapfile, evap_interpMethod,
     &     evap_nlat, evap_nlon, evap_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'precip', precipfile, precip_interpMethod,
     &     precip_nlat, precip_nlon, precip_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'snowprecip', snowprecipfile,
     &         snowprecip_interpMethod, snowprecip_nlat,
     &         snowprecip_nlon, snowprecip_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'uwind', uwindfile, uwind_interpMethod,
     &     uwind_nlat, uwind_nlon, uwind_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'vwind', vwindfile, vwind_interpMethod,
     &     vwind_nlat, vwind_nlon, vwind_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'wspeed', wspeedfile, wspeed_interpMethod,
     &     wspeed_nlat, wspeed_nlon, wspeed_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'lwflux', lwfluxfile, lwflux_interpMethod,
     &     lwflux_nlat, lwflux_nlon, lwflux_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'swdown', swdownfile, swdown_interpMethod,
     &     swdown_nlat, swdown_nlon, swdown_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'lwdown', lwdownfile, lwdown_interpMethod,
     &     lwdown_nlat, lwdown_nlon, lwdown_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'apressure', apressurefile,
     &         apressure_interpMethod, apressure_nlat,
     &         apressure_nlon, apressure_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'tidePot', tidePotFile,
     &         tidePot_interpMethod, tidePot_nlat,
     &         tidePot_nlon, tidePot_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'areamask', areamaskfile,
     &         areamask_interpMethod, areamask_nlat,
     &         areamask_nlon, areamask_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'climsst', climsstfile,
     &         climsst_interpMethod,  climsst_nlat,
     &         climsst_nlon, climsst_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'climsss', climsssfile,
     &         climsss_interpMethod, climsss_nlat,
     &         climsss_nlon, climsss_lon_inc, errCount, myThid )
      CALL EXF_CHECK_INTERP( 'climustr', climustrfile,
     &         climustr_interpMethod, climustr_nlat,
     &         climustr_nlon, climustr_lon_inc, errCount,myThid )
      CALL EXF_CHECK_INTERP( 'climvstr', climvstrfile,
     &         climvstr_interpMethod, climvstr_nlat,
     &         climvstr_nlon, climvstr_lon_inc,  errCount, myThid )

C-    some restrictions on 2-component vector field (might be relaxed later on)
      IF ( ( uwind_interpMethod.GE.1 .AND. uwindfile.NE.' ' ) .OR.
     &     ( vwind_interpMethod.GE.1 .AND. vwindfile.NE.' ' ) ) THEN
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        IF ( uwind_interpMethod.EQ.0 .OR. uwindfile.EQ.' ' .OR.
     &       vwind_interpMethod.EQ.0 .OR. vwindfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
         WRITE(msgBuf,'(A)')
     &        'EXF_CHECK: interp. needs 2 components (wind)'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
        IF ( uwindStartTime .NE. vwindStartTime .OR.
     &       uwindperiod    .NE. vwindperiod    .OR.
     &       uwindRepCycle  .NE. vwindRepCycle  ) THEN
         WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &        'For CurvilinearGrid/RotatedGrid, the u and v wind '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,A,A)') 'EXF_CHECK: ',
     &        'files have to have the same StartTime,period & Cycle ',
     &        'because S/R EXF_SET_UV assumes that.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDIF
      IF ( (ustress_interpMethod.GE.1 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.GE.1 .AND. vstressfile.NE.' ') ) THEN
       IF ( readStressOnCgrid ) THEN
        WRITE(msgBuf,'(A,A)')
     &       'EXF_CHECK: readStressOnCgrid=.TRUE. ',
     &       'and interp wind-stress (=A-grid) are not compatible'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        IF ( ustress_interpMethod.EQ.0 .OR. ustressfile.EQ.' ' .OR.
     &       vstress_interpMethod.EQ.0 .OR. vstressfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
         WRITE(msgBuf,'(A)')
     &        'EXF_CHECK: interp. needs 2 components (wind-stress)'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
        IF ( ustressStartTime .NE. vstressStartTime .OR.
     &       ustressperiod    .NE. vstressperiod    .OR.
     &       ustressRepCycle  .NE. vstressRepCycle ) THEN
         WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &    'For CurvilinearGrid/RotatedGrid, the u and v wind stress '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,A,A)') 'EXF_CHECK: ',
     &        'have to have the same StartTime,period & Cycle ',
     &        'because S/R EXF_SET_UV assumes that.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
      ENDIF

      IF ( (ustress_interpMethod.EQ.0 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.EQ.0 .AND. vstressfile.NE.' ') ) THEN
#else /* ndef USE_EXF_INTERPOLATION */
      IF     ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
#endif /* USE_EXF_INTERPOLATION */
       IF (     (readStressOnAgrid.AND.readStressOnCgrid) .OR.
     &      .NOT.(readStressOnAgrid.OR.readStressOnCgrid) ) THEN
        WRITE(msgBuf,'(A)')
     & 'EXF_CHECK: Select 1 wind-stress position: A or C-grid'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF (rotateStressOnAgrid.AND..NOT.readStressOnAgrid) THEN
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: rotateStressOnAgrid ',
     &       'only applies to cases readStressOnAgrid is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( rotateStressOnAgrid .AND.
     &      ( ustressfile .EQ. ' ' .OR. ustressperiod .EQ. 0. .OR.
     &        vstressfile .EQ. ' ' .OR. vstressperiod .EQ. 0. ) ) THEN
        WRITE(msgBuf,'(2A)') 'EXF_CHECK: rotateStressOnAgrid ',
     &       'requires to update u/vstress'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

      ELSE
       IF ( readStressOnAgrid .OR. readStressOnCgrid .OR.
     &      rotateStressOnAgrid ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** EXF_CHECK: ',
     &       'wind-stress position irrelevant'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDIF

#ifdef USE_NO_INTERP_RUNOFF
      WRITE(msgBuf,'(A)')
     &     'EXF_CHECK: USE_NO_INTERP_RUNOFF code has been removed;'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A,A)')
     &     'EXF_CHECK: use instead "runoff_interpMethod=0"',
     &      ' in "data.exf" (EXF_NML_04)'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif /* USE_NO_INTERP_RUNOFF */

#ifdef ALLOW_CLIMTEMP_RELAXATION
      WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &     'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif /* ALLOW_CLIMTEMP_RELAXATION */

#ifdef ALLOW_CLIMSALT_RELAXATION
      WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ',
     &     'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif /* ALLOW_CLIMSALT_RELAXATION */

      IF ( climsstTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSST_RELAXATION
       WRITE(msgBuf,'(A)') 'EXF_CHECK: climsstTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'EXF_CHECK: but ALLOW_CLIMSST_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* ndef ALLOW_CLIMSST_RELAXATION */
       IF ( climsstfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsstTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsstfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIf
      ENDIf

      IF ( climsssTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSSS_RELAXATION
       WRITE(msgBuf,'(A)') 'EXF_CHECK: climsssTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'EXF_CHECK: but ALLOW_CLIMSSS_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif /* ALLOW_CLIMSSS_RELAXATION */
       IF ( climsssfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsssTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsssfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'EXF_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_CHECK_INTERP
C     !INTERFACE:

      SUBROUTINE EXF_CHECK_INTERP(
     I               loc_name, loc_file, loc_interpMethod,
     I               loc_nlat, loc_nlon, loc_lon_inc,
     U               errCount,
     I               myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXF_CHECK_INTERP
C     | o Check parameters for one of the pkg/exf variable
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "EXF_INTERP_SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     fldName          :: field short name (to print mesg)
C     fldFile          :: file-name for this field
C     loc_interpMethod :: select interpolation method
C     loc_nlat         :: size in y direction of original input grid
C     loc_nlon         :: size in x direction of original input grid
C     fld_lon_inc      :: scalar x-grid increment
C     errCount         :: error counter
C     myThid           :: my Thread Id number
      CHARACTER*(*) loc_name
      CHARACTER*(*) loc_file
      INTEGER loc_interpMethod
      INTEGER loc_nlat
      INTEGER loc_nlon
      _RL     loc_lon_inc
      INTEGER errCount
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF ( loc_interpMethod.GE.1 .AND. loc_file.NE.' ' ) THEN
       IF ( loc_nlat .GT. (MAX_LAT_INC+1) ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK_INTERP: ',loc_name,
     &                      '_nlat > (MAX_LAT_INC+1)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
#ifndef EXF_INTERP_USE_DYNALLOC
C-     Check buffer size:
       IF ( loc_nlon.GT.exf_max_nLon ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK_INTERP: ',loc_name,
     &                      '_nlon > exf_max_nLon'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( loc_nlat.GT.exf_max_nLat ) THEN
        WRITE(msgBuf,'(3A)') 'EXF_CHECK_INTERP: ',loc_name,
     &                      '_nlat > exf_max_nLat'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( (loc_nlon+4)*(loc_nlat+4).GT.exf_interp_bufferSize ) THEN
        WRITE(msgBuf,'(6A)') 'EXF_CHECK_INTERP: ',
     &    'exf_interp_bufferSize too small for ',
     &     loc_name, '_nlon & ', loc_name, '_nlat'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
#endif /* ndef EXF_INTERP_USE_DYNALLOC */
       IF ( loc_lon_inc.GT.500. ) THEN
        WRITE(msgBuf,'(4A,1PE16.8)') 'EXF_CHECK_INTERP: ',
     &    'Invalid value for: ',loc_name,'_lon_inc =', loc_lon_inc
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(4A)') 'EXF_CHECK_INTERP: Fix it ',
     &    'or Turn off ',loc_name,'-interp (interpMethod=0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

      RETURN
      END
