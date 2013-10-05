C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_init_fixed.F,v 1.26 2013/10/05 19:36:12 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_ICEFRONT
# include "ICEFRONT_OPTIONS.h"
#endif

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
#ifdef ALLOW_ICEFRONT
# include "ICEFRONT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
#ifdef USE_EXF_INTERPOLATION
      INTEGER j
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('EXF_INIT_FIXED',myThid)
#endif

      _BEGIN_MASTER( myThid )

C--   Set mask for each input field
C     ' ' = no masking; 'c' = centered mask; 'w' = western mask; 's' = southern
      hfluxmask    = 'c'
      sfluxmask    = 'c'
      atempmask    = 'c'
      aqhmask      = 'c'
      evapmask     = 'c'
      precipmask   = 'c'
      snowprecipmask='c'
      runoffmask   = 'c'
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
      areamaskmask = 'c'
      climsstmask  = 'c'
      climsssmask  = 'c'
      climustrmask = 'w'
      climvstrmask = 's'
      sgrunoffmask = 'c'

      IF ( useSEAICE ) THEN
C     Avoid masking of vector fields with pkg/seaice (for B/C-grid interp.)
C     but keep it for fields that might involve calculation using SST on land
       hfluxmask    = ' '
       sfluxmask    = ' '
c      atempmask    = ' '
c      aqhmask      = ' '
c      evapmask     = ' '
c      precipmask   = ' '
c      snowprecipmask=' '
c      runoffmask   = ' '
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
c      areamaskmask = ' '
c      climsstmask  = ' '
c      climsssmask  = ' '
       climustrmask = ' '
       climvstrmask = ' '
c      sgrunoffmask = ' '
      ENDIF

C--   Complete the start date specifications for the forcing
C     fields to get a complete calendar date array.
C     FLDstartdate, for FLD = { uwind, vwind, wspeed, etc. },
C     returns time in seconds of first FLD record from the
C     beginning of the model integration or, if useYearlyFields,
C     from the beginning of the year.

      IF ( useAtmWind ) THEN
      IF ( uwindfile .NE. ' ' .AND. uwindperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START uwind',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          uwindstartdate1, uwindstartdate2,
     O                          uwindstartdate,
     I                          myThid )
      ENDIF
      IF ( vwindfile .NE. ' ' .AND. vwindperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START vwind',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          vwindstartdate1, vwindstartdate2,
     O                          vwindstartdate,
     I                          myThid )
      ENDIF
      ENDIF

      IF ( wspeedfile .NE. ' ' .AND. wspeedperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START wspeed',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          wspeedstartdate1, wspeedstartdate2,
     O                          wspeedstartdate,
     I                          myThid )
      ENDIF

      IF ( .NOT.useAtmWind ) THEN
      IF ( ustressfile .NE. ' ' .AND. ustressperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START ustress',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          ustressstartdate1, ustressstartdate2,
     O                          ustressstartdate,
     I                          myThid )
      ENDIF
      IF ( vstressfile .NE. ' ' .AND. vstressperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START vstress',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          vstressstartdate1, vstressstartdate2,
     O                          vstressstartdate,
     I                          myThid )
      ENDIF
      ENDIF

#ifdef ALLOW_ATM_TEMP
      IF ( atempfile .NE. ' ' .AND. atempperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START atemp',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          atempstartdate1, atempstartdate2,
     O                          atempstartdate,
     I                          myThid )
      ENDIF
      IF ( aqhfile .NE. ' ' .AND. aqhperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START aqh',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          aqhstartdate1, aqhstartdate2,
     O                          aqhstartdate,
     I                          myThid )
      ENDIF
      IF ( lwfluxfile .NE. ' ' .AND. lwfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START lwflux',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          lwfluxstartdate1, lwfluxstartdate2,
     O                          lwfluxstartdate,
     I                          myThid )
      ENDIF
      IF ( precipfile .NE. ' ' .AND. precipperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START precip',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          precipstartdate1, precipstartdate2,
     O                          precipstartdate,
     I                          myThid )
      ENDIF
      IF ( snowprecipfile .NE. ' ' .AND. snowprecipperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFIELD_START snowprecip',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                   snowprecipstartdate1, snowprecipstartdate2,
     O                   snowprecipstartdate,
     I                          myThid )
      ENDIF
#else /* ifndef ALLOW_ATM_TEMP */
      IF ( hfluxfile .NE. ' ' .AND. hfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START hflux',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          hfluxstartdate1, hfluxstartdate2,
     O                          hfluxstartdate,
     I                          myThid )
      ENDIF
      IF ( sfluxfile .NE. ' ' .AND. sfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START sflux',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          sfluxstartdate1, sfluxstartdate2,
     O                          sfluxstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .NE. ' ' .AND. swfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START swflux',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          swfluxstartdate1, swfluxstartdate2,
     O                          swfluxstartdate,
     I                          myThid )
      ENDIF
#endif /* defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING) */

#ifdef EXF_READ_EVAP
      IF ( evapfile .NE. ' ' .AND. evapperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START evap',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          evapstartdate1, evapstartdate2,
     O                          evapstartdate,
     I                          myThid )
      ENDIF
#endif /* EXF_READ_EVAP */

#ifdef ALLOW_RUNOFF
      IF ( runofffile .NE. ' ' .AND. runoffperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START runoff',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          runoffstartdate1, runoffstartdate2,
     O                          runoffstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_ICEFRONT
      IF ( sgrunofffile .NE. ' ' .AND. sgrunoffperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START sgrunoff',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          sgrunoffstartdate1, sgrunoffstartdate2,
     O                          sgrunoffstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_DOWNWARD_RADIATION
      IF ( swdownfile .NE. ' ' .AND. swdownperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START swdown',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          swdownstartdate1, swdownstartdate2,
     O                          swdownstartdate,
     I                          myThid )
      ENDIF
      IF ( lwdownfile .NE. ' ' .AND. lwdownperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START lwdown',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          lwdownstartdate1, lwdownstartdate2,
     O                          lwdownstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
      IF ( apressurefile .NE. ' ' .AND. apressureperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode)
     &    CALL DEBUG_CALL('GETFIELD_START apressure',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                   apressurestartdate1, apressurestartdate2,
     O                   apressurestartdate,
     I                          myThid )
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

#ifdef EXF_SEAICE_FRACTION
      IF ( areamaskfile .NE. ' ' .AND. areamaskperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START areamask',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          areamaskstartdate1, areamaskstartdate2,
     O                          areamaskstartdate,
     I                          myThid )
      ENDIF
#endif /* EXF_SEAICE_FRACTION */

#ifdef ALLOW_OBCS
      IF ( obcsNperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START obcsN',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          obcsNstartdate1, obcsNstartdate2,
     O                          obcsNstartdate,
     I                          myThid )
      ENDIF
      IF ( obcsSperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START obcsS',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          obcsSstartdate1, obcsSstartdate2,
     O                          obcsSstartdate,
     I                          myThid )
      ENDIF
      IF ( obcsEperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START obcsE',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          obcsEstartdate1, obcsEstartdate2,
     O                          obcsEstartdate,
     I                          myThid )
      ENDIF
      IF ( obcsWperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START obcsW',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          obcsWstartdate1, obcsWstartdate2,
     O                          obcsWstartdate,
     I                          myThid )
      ENDIF
# ifdef ALLOW_SEAICE
      IF ( siobNperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START siobN',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          siobNstartdate1, siobNstartdate2,
     O                          siobNstartdate,
     I                          myThid )
      ENDIF
      IF ( siobSperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START siobS',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          siobSstartdate1, siobSstartdate2,
     O                          siobSstartdate,
     I                          myThid )
      ENDIF
      IF ( siobEperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START siobE',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          siobEstartdate1, siobEstartdate2,
     O                          siobEstartdate,
     I                          myThid )
      ENDIF
      IF ( siobWperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START siobW',myThid)
# endif
       CALL EXF_GETFIELD_START( useOBCSYearlyFields,
     I                          siobWstartdate1, siobWstartdate2,
     O                          siobWstartdate,
     I                          myThid )
      ENDIF
# endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_CLIMSST_RELAXATION
      IF ( climsstfile .NE. ' ' .AND. climsstperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START climsst',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          climsststartdate1, climsststartdate2,
     O                          climsststartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_CLIMSST_RELAXATION */

#ifdef ALLOW_CLIMSSS_RELAXATION
      IF ( climsssfile .NE. ' ' .AND. climsssperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START climsss',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          climsssstartdate1, climsssstartdate2,
     O                          climsssstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_CLIMSSS_RELAXATION */

#ifdef ALLOW_CLIMSTRESS_RELAXATION
      IF ( climustrfile .NE. ' ' .AND. climustrperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START climustr',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          climustrstartdate1, climustrstartdate2,
     O                          climustrstartdate,
     I                          myThid )
      ENDIF

      IF ( climvstrfile .NE. ' ' .AND. climvstrperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('GETFIELD_START climvstr',myThid)
# endif
       CALL EXF_GETFIELD_START( useExfYearlyFields,
     I                          climvstrstartdate1, climvstrstartdate2,
     O                          climvstrstartdate,
     I                          myThid )
      ENDIF
#endif /* ALLOW_CLIMSTRESS_RELAXATION */

#ifdef USE_EXF_INTERPOLATION
C--   For vector fields, set flag to interpolate the 2 components together
C-    wind-stress:
      IF ( .NOT.useAtmWind ) THEN
      uvInterp_stress = ustressfile.NE.' ' .AND. vstressfile.NE.' '
     &  .AND. ustress_interpMethod.GE.1 .AND. vstress_interpMethod.GE.1
      uvInterp_stress = uvInterp_stress
     &  .AND. ustressstartdate .EQ. vstressstartdate
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
      ENDIF
C-    wind:
      IF ( useAtmWind ) THEN
      uvInterp_wind = uwindfile.NE.' ' .AND. vwindfile.NE.' '
     &  .AND. uwind_interpMethod.GE.1 .AND. vwind_interpMethod.GE.1
      uvInterp_wind = uvInterp_wind
     &  .AND. uwindstartdate .EQ. vwindstartdate
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
      ENDIF
C-    clim wind-stress:
      uvInterp_climstr = climustrfile.NE.' ' .AND. climvstrfile.NE.' '
     &  .AND.climustr_interpMethod.GE.1 .AND.climvstr_interpMethod.GE.1
      uvInterp_climstr = uvInterp_climstr
     &  .AND. climustrstartdate .EQ. climvstrstartdate
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
