#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE EXF_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_INIT_FIXED
C     | o Routine to initialize EXF variables
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_INTERP_SIZE.h"
#include "EXF_INTERP_PARAM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
C     errCount  :: error counter
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
#ifdef USE_EXF_INTERPOLATION
      INTEGER j
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('EXF_INIT_FIXED',myThid)
#endif

      _BEGIN_MASTER( myThid )
      errCount = 0

C--   Set mask for each input field
C     ' ' = no masking; 'c' = centered mask; 'w' = western mask; 's' = southern
      hfluxmask    = 'c'
      sfluxmask    = 'c'
      atempmask    = 'c'
      aqhmask      = 'c'
      hs_mask      = 'c'
      hl_mask      = 'c'
      evapmask     = 'c'
      precipmask   = 'c'
      snowprecipmask='c'
      runoffmask   = 'c'
      saltflxmask  = 'c'
      IF ( stressIsOnCgrid ) THEN
       ustressmask = 'w'
       vstressmask = 's'
      ELSE
       ustressmask = 'c'
       vstressmask = 'c'
      ENDIF
      uwindmask    = 'c'
      vwindmask    = 'c'
      wspeedmask   = 'c'
      swfluxmask   = 'c'
      lwfluxmask   = 'c'
      swdownmask   = 'c'
      lwdownmask   = 'c'
      apressuremask= 'c'
      tidePotmask  = 'c'
      areamaskmask = 'c'
      climsstmask  = 'c'
      climsssmask  = 'c'
      climustrmask = 'w'
      climvstrmask = 's'

      IF ( useSEAICE ) THEN
C     Avoid masking of vector fields with pkg/seaice (for B/C-grid interp.)
C     but keep it for fields that might involve calculation using SST on land
       hfluxmask    = ' '
       sfluxmask    = ' '
c      atempmask    = ' '
c      aqhmask      = ' '
c      hs_mask      = ' '
c      hl_mask      = ' '
c      evapmask     = ' '
c      precipmask   = ' '
c      snowprecipmask=' '
c      runoffmask   = ' '
c      saltflxmask  = ' '
       ustressmask  = ' '
       vstressmask  = ' '
       uwindmask    = ' '
       vwindmask    = ' '
       wspeedmask   = ' '
       swfluxmask   = ' '
       swdownmask   = ' '
c      lwfluxmask   = ' '
c      lwdownmask   = ' '
       apressuremask= ' '
c      tidePotmask  = ' '
c      areamaskmask = ' '
c      climsstmask  = ' '
c      climsssmask  = ' '
       climustrmask = ' '
       climvstrmask = ' '
      ENDIF

C--   Complete the start date specifications for the forcing
C     fields to get a complete calendar date array.
C     FLDStartTime, for FLD = { uwind, vwind, wspeed, etc. },
C     returns time in seconds of first FLD record from the
C     beginning of the model integration or, if useYearlyFields,
C     from the beginning of the year.

      IF ( useAtmWind ) THEN
       IF ( uwindfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START uwind',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                     'exf', 'uwind', uwindperiod,
     I                     uwindstartdate1, uwindstartdate2,
     U                     uwindStartTime, errCount,
     I                     myThid )
       ENDIF
       IF ( vwindfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START vwind',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                     'exf', 'vwind', vwindperiod,
     I                     vwindstartdate1, vwindstartdate2,
     U                     vwindStartTime, errCount,
     I                     myThid )
       ENDIF
      ENDIF

      IF ( wspeedfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START wspeed',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'wspeed', wspeedperiod,
     I                    wspeedstartdate1, wspeedstartdate2,
     U                    wspeedStartTime, errCount,
     I                    myThid )
      ENDIF

      IF ( .NOT.useAtmWind ) THEN
       IF ( ustressfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode)
     &     CALL DEBUG_CALL('GETFFIELD_START ustress',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                     'exf', 'ustress', ustressperiod,
     I                     ustressstartdate1, ustressstartdate2,
     U                     ustressStartTime, errCount,
     I                     myThid )
       ENDIF
       IF ( vstressfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode)
     &     CALL DEBUG_CALL('GETFFIELD_START vstress',myThid)
# endif
        CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                     'exf', 'vstress', vstressperiod,
     I                     vstressstartdate1, vstressstartdate2,
     U                     vstressStartTime, errCount,
     I                     myThid )
       ENDIF
      ENDIF

      IF ( hfluxfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START hflux',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'hflux', hfluxperiod,
     I                    hfluxstartdate1, hfluxstartdate2,
     U                    hfluxStartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( sfluxfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START sflux',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'sflux', sfluxperiod,
     I                    sfluxstartdate1, sfluxstartdate2,
     U                    sfluxStartTime, errCount,
     I                    myThid )
      ENDIF

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .NE. ' '  ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START swflux',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'swflux', swfluxperiod,
     I                    swfluxstartdate1, swfluxstartdate2,
     U                    swfluxStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING) */

#ifdef ALLOW_ATM_TEMP
      IF ( atempfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START atemp',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'atemp', atempperiod,
     I                    atempstartdate1, atempstartdate2,
     U                    atempStartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( aqhfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START aqh',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'aqh', aqhperiod,
     I                    aqhstartdate1, aqhstartdate2,
     U                    aqhStartTime, errCount,
     I                    myThid )
      ENDIF
# ifdef ALLOW_READ_TURBFLUXES
      IF ( hs_file .NE. ' ' ) THEN
#  ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START hs ',myThid)
#  endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'hs_', hs_period,
     I                    hs_startdate1, hs_startdate2,
     U                    hs_StartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( hl_file .NE. ' ' ) THEN
#  ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START hl ',myThid)
#  endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'hl_', hl_period,
     I                    hl_startdate1, hl_startdate2,
     U                    hl_StartTime, errCount,
     I                    myThid )
      ENDIF
# endif /* ALLOW_READ_TURBFLUXES */
      IF ( lwfluxfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START lwflux',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'lwflux', lwfluxperiod,
     I                    lwfluxstartdate1, lwfluxstartdate2,
     U                    lwfluxStartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( precipfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START precip',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'precip', precipperiod,
     I                    precipstartdate1, precipstartdate2,
     U                    precipStartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( snowprecipfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START snowprecip',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'snowprecip', snowprecipperiod,
     I                    snowprecipstartdate1, snowprecipstartdate2,
     U                    snowprecipStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_ATM_TEMP */

#ifdef EXF_READ_EVAP
      IF ( evapfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START evap',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'evap', evapperiod,
     I                    evapstartdate1, evapstartdate2,
     U                    evapStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* EXF_READ_EVAP */

#ifdef ALLOW_RUNOFF
      IF ( runofffile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START runoff',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'runoff', runoffperiod,
     I                    runoffstartdate1, runoffstartdate2,
     U                    runoffStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_SALTFLX
      IF ( saltflxfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START saltflx',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'saltflx', saltflxperiod,
     I                    saltflxstartdate1, saltflxstartdate2,
     U                    saltflxStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_SALTFLX */

#ifdef ALLOW_DOWNWARD_RADIATION
      IF ( swdownfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START swdown',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'swdown', swdownperiod,
     I                    swdownstartdate1, swdownstartdate2,
     U                    swdownStartTime, errCount,
     I                    myThid )
      ENDIF
      IF ( lwdownfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START lwdown',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'lwdown', lwdownperiod,
     I                    lwdownstartdate1, lwdownstartdate2,
     U                    lwdownStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
      IF ( apressurefile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START apressure',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'apressure', apressureperiod,
     I                    apressurestartdate1, apressurestartdate2,
     U                    apressureStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

#ifdef EXF_ALLOW_TIDES
      IF ( tidePotFile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START tidePot',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'tidePot', tidePotPeriod,
     I                    tidePotStartdate1, tidePotStartdate2,
     U                    tidePotStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* EXF_ALLOW_TIDES */

#ifdef EXF_SEAICE_FRACTION
      IF ( areamaskfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START areamask',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'areamask', areamaskperiod,
     I                    areamaskstartdate1, areamaskstartdate2,
     U                    areamaskStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* EXF_SEAICE_FRACTION */

#ifdef ALLOW_OBCS
      IF ( useOBCS ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START obcsN',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'obcsN', obcsNperiod,
     I                    obcsNstartdate1, obcsNstartdate2,
     U                    obcsNstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START obcsS',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'obcsS', obcsSperiod,
     I                    obcsSstartdate1, obcsSstartdate2,
     U                    obcsSstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START obcsE',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'obcsE', obcsEperiod,
     I                    obcsEstartdate1, obcsEstartdate2,
     U                    obcsEstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START obcsW',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'obcsW', obcsWperiod,
     I                    obcsWstartdate1, obcsWstartdate2,
     U                    obcsWstartTime, errCount,
     I                    myThid )
      ENDIF
# ifdef ALLOW_SEAICE
      IF ( useOBCS .AND. useSEAICE ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START siobN',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'siobN', siobNperiod,
     I                    siobNstartdate1, siobNstartdate2,
     U                    siobNstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START siobS',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'siobS', siobSperiod,
     I                    siobSstartdate1, siobSstartdate2,
     U                    siobSstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START siobE',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'siobE', siobEperiod,
     I                    siobEstartdate1, siobEstartdate2,
     U                    siobEstartTime, errCount,
     I                    myThid )
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START siobW',myThid)
# endif
       CALL EXF_GETFFIELD_START( useOBCSYearlyFields,
     I                    'exf', 'siobW', siobWperiod,
     I                    siobWstartdate1, siobWstartdate2,
     U                    siobWstartTime, errCount,
     I                    myThid )
      ENDIF
# endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_CLIMSST_RELAXATION
      IF ( climsstfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START climsst',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'climsst', climsstperiod,
     I                    climsststartdate1, climsststartdate2,
     U                    climsstStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_CLIMSST_RELAXATION */

#ifdef ALLOW_CLIMSSS_RELAXATION
      IF ( climsssfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START climsss',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'climsss', climsssperiod,
     I                    climsssstartdate1, climsssstartdate2,
     U                    climsssStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_CLIMSSS_RELAXATION */

#ifdef ALLOW_CLIMSTRESS_RELAXATION
      IF ( climustrfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START climustr',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'climustr', climustrperiod,
     I                    climustrstartdate1, climustrstartdate2,
     U                    climustrStartTime, errCount,
     I                    myThid )
      ENDIF

      IF ( climvstrfile .NE. ' ' ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFFIELD_START climvstr',myThid)
# endif
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'exf', 'climvstr', climvstrperiod,
     I                    climvstrstartdate1, climvstrstartdate2,
     U                    climvstrStartTime, errCount,
     I                    myThid )
      ENDIF
#endif /* ALLOW_CLIMSTRESS_RELAXATION */

#ifdef USE_EXF_INTERPOLATION
C--   For vector fields, set flag to interpolate the 2 components together
C-    wind-stress:
      uvInterp_stress = ustressfile.NE.' ' .AND. vstressfile.NE.' '
     &  .AND. ustress_interpMethod.GE.1 .AND. vstress_interpMethod.GE.1
      uvInterp_stress = uvInterp_stress
     &  .AND. ustressStartTime .EQ. vstressStartTime
     &  .AND. ustressperiod    .EQ. vstressperiod
      uvInterp_stress = uvInterp_stress
     &  .AND. ustress_nlon .EQ. vstress_nlon
     &  .AND. ustress_nlat .EQ. vstress_nlat
     &  .AND. ustress_lon0 .EQ. vstress_lon0
     &  .AND. ustress_lat0 .EQ. vstress_lat0
     &  .AND. ustress_lon_inc .EQ. vstress_lon_inc
      IF ( uvInterp_stress ) THEN
        DO j=1,MIN(ustress_nlat-1,MAX_LAT_INC)
          uvInterp_stress = uvInterp_stress
     &     .AND. ustress_lat_inc(j) .EQ. vstress_lat_inc(j)
        ENDDO
      ENDIF
C-    wind:
      uvInterp_wind = uwindfile.NE.' ' .AND. vwindfile.NE.' '
     &  .AND. uwind_interpMethod.GE.1 .AND. vwind_interpMethod.GE.1
      uvInterp_wind = uvInterp_wind
     &  .AND. uwindStartTime .EQ. vwindStartTime
     &  .AND. uwindperiod    .EQ. vwindperiod
      uvInterp_wind = uvInterp_wind
     &  .AND. uwind_nlon .EQ. vwind_nlon
     &  .AND. uwind_nlat .EQ. vwind_nlat
     &  .AND. uwind_lon0 .EQ. vwind_lon0
     &  .AND. uwind_lat0 .EQ. vwind_lat0
     &  .AND. uwind_lon_inc .EQ. vwind_lon_inc
      IF ( uvInterp_wind ) THEN
        DO j=1,MIN(uwind_nlat-1,MAX_LAT_INC)
          uvInterp_wind = uvInterp_wind
     &     .AND. uwind_lat_inc(j) .EQ. vwind_lat_inc(j)
        ENDDO
      ENDIF
C-    clim wind-stress:
      uvInterp_climstr = climustrfile.NE.' ' .AND. climvstrfile.NE.' '
     &  .AND.climustr_interpMethod.GE.1 .AND.climvstr_interpMethod.GE.1
      uvInterp_climstr = uvInterp_climstr
     &  .AND. climustrStartTime .EQ. climvstrStartTime
     &  .AND. climustrperiod    .EQ. climvstrperiod
      uvInterp_climstr = uvInterp_climstr
     &  .AND. climustr_nlon .EQ. climvstr_nlon
     &  .AND. climustr_nlat .EQ. climvstr_nlat
     &  .AND. climustr_lon0 .EQ. climvstr_lon0
     &  .AND. climustr_lat0 .EQ. climvstr_lat0
     &  .AND. climustr_lon_inc .EQ. climvstr_lon_inc
      IF ( uvInterp_climstr ) THEN
        DO j=1,MIN(climustr_nlat-1,MAX_LAT_INC)
          uvInterp_climstr = uvInterp_climstr
     &     .AND. climustr_lat_inc(j) .EQ. climvstr_lat_inc(j)
        ENDDO
      ENDIF
# ifdef EXF_USE_OLD_INTERP_POLE
      uvInterp_stress = .FALSE.
      uvInterp_wind   = .FALSE.
      uvInterp_climstr= .FALSE.
# endif
#endif  /* USE_EXF_INTERPOLATION */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'EXF_INIT_FIXED: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R EXF_INIT_FIXED'
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

#ifdef ALLOW_ZENITHANGLE
      IF ( useExfZenAlbedo .OR. useExfZenIncoming ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('EXF_ZENITHANGLE_TABLE',myThid)
# endif
       CALL EXF_ZENITHANGLE_TABLE(myThid)
      ENDIF
#endif /* ALLOW_ZENITHANGLE */

C--   Summarize the External forcing setup.
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('EXF_SUMMARY',myThid)
# endif
      CALL EXF_SUMMARY( myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXF_DIAGNOSTICS_INIT',myThid)
# endif
        CALL EXF_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('EXF_INIT_FIXED',myThid)
#endif

      RETURN
      END
