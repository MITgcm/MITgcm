C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_init_fixed.F,v 1.20 2012/01/05 20:39:53 jmc Exp $
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
#include "cal.h"
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
      INTEGER date_array(4), difftime(4), yearStartDate(4)
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
C     Do not filter with pkg/seaice because of B/C-grid interpolation
       hfluxmask    = ' '
       sfluxmask    = ' '
       atempmask    = ' '
       aqhmask      = ' '
       evapmask     = ' '
       precipmask   = ' '
       snowprecipmask=' '
       runoffmask   = ' '
       ustressmask  = ' '
       vstressmask  = ' '
       uwindmask    = ' '
       vwindmask    = ' '
       wspeedmask   = ' '
       swfluxmask   = ' '
       lwfluxmask   = ' '
       swdownmask   = ' '
       lwdownmask   = ' '
       apressuremask= ' '
       areamaskmask = ' '
       climsstmask  = ' '
       climsssmask  = ' '
       climustrmask = ' '
       climvstrmask = ' '
       sgrunoffmask = ' '
      ENDIF

C--   Complete the start date specifications for the forcing
C     fields to get a complete calendar date array.
C     FLDstartdate, for FLD = { uwind, vwind, wspeed, etc. },
C     returns time in seconds of first FLD record from the
C     beginning of the model integration or, if useYearlyFields,
C     from the beginning of the year.

#ifdef ALLOW_ATM_WIND
      IF ( uwindfile .NE. ' ' .AND. uwindperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE uwind',myThid)
# endif
       CALL CAL_FULLDATE  ( uwindstartdate1, uwindstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   uwindstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   uwindstartdate       ,myThid)
        uwindstartdate   =  modelstart + uwindstartdate
       ENDIF
      ENDIF
      IF ( vwindfile .NE. ' ' .AND. vwindperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE vwind',myThid)
# endif
       CALL CAL_FULLDATE  ( vwindstartdate1, vwindstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   vwindstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   vwindstartdate       ,myThid)
        vwindstartdate   =  modelstart + vwindstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_ATM_WIND */
      IF ( wspeedfile .NE. ' ' .AND. wspeedperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE wspeed',myThid)
# endif
       CALL CAL_FULLDATE  ( wspeedstartdate1, wspeedstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   wspeedstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   wspeedstartdate      ,myThid)
        wspeedstartdate  =  modelstart + wspeedstartdate
       ENDIF
      ENDIF
      IF ( ustressfile .NE. ' ' .AND. ustressperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE ustress',myThid)
# endif
       CALL CAL_FULLDATE  ( ustressstartdate1, ustressstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   ustressstartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   ustressstartdate     ,myThid)
        ustressstartdate =  modelstart + ustressstartdate
       ENDIF
      ENDIF
      IF ( vstressfile .NE. ' ' .AND. vstressperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE vstress',myThid)
# endif
       CALL CAL_FULLDATE  ( vstressstartdate1, vstressstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   vstressstartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   vstressstartdate     ,myThid)
        vstressstartdate =  modelstart + vstressstartdate
       ENDIF
      ENDIF

#ifdef ALLOW_ATM_TEMP
      IF ( atempfile .NE. ' ' .AND. atempperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE atemp',myThid)
# endif
       CALL CAL_FULLDATE  ( atempstartdate1, atempstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   atempstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   atempstartdate       ,myThid)
        atempstartdate   =  modelstart + atempstartdate
       ENDIF
      ENDIF
      IF ( aqhfile .NE. ' ' .AND. aqhperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE aqh',myThid)
# endif
       CALL CAL_FULLDATE  ( aqhstartdate1, aqhstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   aqhstartdate         ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   aqhstartdate         ,myThid)
        aqhstartdate     =  modelstart + aqhstartdate
       ENDIF
      ENDIF
      IF ( lwfluxfile .NE. ' ' .AND. lwfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE lwflux',myThid)
# endif
       CALL CAL_FULLDATE  ( lwfluxstartdate1, lwfluxstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   lwfluxstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   lwfluxstartdate      ,myThid)
        lwfluxstartdate  =  modelstart + lwfluxstartdate
       ENDIF
      ENDIF
      IF ( precipfile .NE. ' ' .AND. precipperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE precip',myThid)
# endif
       CALL CAL_FULLDATE  ( precipstartdate1, precipstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   precipstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   precipstartdate      ,myThid)
        precipstartdate  =  modelstart + precipstartdate
       ENDIF
      ENDIF
      IF ( snowprecipfile .NE. ' ' .AND. snowprecipperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE snowprecip',myThid)
# endif
       CALL CAL_FULLDATE( snowprecipstartdate1, snowprecipstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   snowprecipstartdate  ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   snowprecipstartdate  ,myThid)
        snowprecipstartdate=modelstart + snowprecipstartdate
       ENDIF
      ENDIF
#else /* ifndef ALLOW_ATM_TEMP */
      IF ( hfluxfile .NE. ' ' .AND. hfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE hflux',myThid)
# endif
       CALL CAL_FULLDATE  ( hfluxstartdate1, hfluxstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   hfluxstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   hfluxstartdate       ,myThid)
        hfluxstartdate   =  modelstart + hfluxstartdate
       ENDIF
      ENDIF
      IF ( sfluxfile .NE. ' ' .AND. sfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE sflux',myThid)
# endif
       CALL CAL_FULLDATE  ( sfluxstartdate1, sfluxstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   sfluxstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   sfluxstartdate       ,myThid)
        sfluxstartdate   =  modelstart + sfluxstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .NE. ' ' .AND. swfluxperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE swflux',myThid)
# endif
       CALL CAL_FULLDATE  ( swfluxstartdate1, swfluxstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   swfluxstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   swfluxstartdate      ,myThid)
        swfluxstartdate  =  modelstart + swfluxstartdate
       ENDIF
      ENDIF
#endif /* defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING) */

#ifdef EXF_READ_EVAP
      IF ( evapfile .NE. ' ' .AND. evapperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE evap',myThid)
# endif
       CALL CAL_FULLDATE  ( evapstartdate1, evapstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   evapstartdate        ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   evapstartdate        ,myThid)
        evapstartdate    =  modelstart + evapstartdate
       ENDIF
      ENDIF
#endif /* EXF_READ_EVAP */

#ifdef ALLOW_RUNOFF
      IF ( runofffile .NE. ' ' .AND. runoffperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE runoff',myThid)
# endif
       CALL CAL_FULLDATE  ( runoffstartdate1, runoffstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   runoffstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   runoffstartdate      ,myThid)
        runoffstartdate  =  modelstart + runoffstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_RUNOFF */

#ifdef ALLOW_ICEFRONT
      IF ( sgrunofffile .NE. ' ' .AND. sgrunoffperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE sgrunoff',myThid)
# endif
       CALL CAL_FULLDATE  ( sgrunoffstartdate1, sgrunoffstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime, sgrunoffstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime, sgrunoffstartdate      ,myThid)
        sgrunoffstartdate  =  modelstart + sgrunoffstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_DOWNWARD_RADIATION
      IF ( swdownfile .NE. ' ' .AND. swdownperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE swdown',myThid)
# endif
       CALL CAL_FULLDATE  ( swdownstartdate1, swdownstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   swdownstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   swdownstartdate      ,myThid)
        swdownstartdate  =  modelstart + swdownstartdate
       ENDIF
      ENDIF
      IF ( lwdownfile .NE. ' ' .AND. lwdownperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE lwdown',myThid)
# endif
       CALL CAL_FULLDATE  ( lwdownstartdate1, lwdownstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   lwdownstartdate      ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   lwdownstartdate      ,myThid)
        lwdownstartdate  =  modelstart + lwdownstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_DOWNWARD_RADIATION */

#ifdef ATMOSPHERIC_LOADING
      IF ( apressurefile .NE. ' ' .AND. apressureperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE apressure',myThid)
# endif
       CALL CAL_FULLDATE  ( apressurestartdate1,  apressurestartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   apressurestartdate   ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   apressurestartdate   ,myThid)
        apressurestartdate= modelstart + apressurestartdate
       ENDIF
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

#ifdef ALLOW_ICE_AREAMASK
      IF ( areamaskfile .NE. ' ' .AND. areamaskperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE areamask',myThid)
# endif
       CALL CAL_FULLDATE  ( areamaskstartdate1,  areamaskstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   areamaskstartdate   ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   areamaskstartdate   ,myThid)
        areamaskstartdate= modelstart + areamaskstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_ICE_AREAMASK */

#ifdef ALLOW_OBCS
      IF ( obcsNperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE obcsN',myThid)
# endif
       CALL CAL_FULLDATE  ( obcsNstartdate1, obcsNstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsNstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsNstartdate       ,myThid)
        obcsNstartdate   =  modelstart + obcsNstartdate
       ENDIF
      ENDIF
      IF ( obcsSperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE obcsS',myThid)
# endif
       CALL CAL_FULLDATE  ( obcsSstartdate1, obcsSstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsSstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsSstartdate       ,myThid)
        obcsSstartdate   =  modelstart + obcsSstartdate
       ENDIF
      ENDIF
      IF ( obcsEperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE obcsE',myThid)
# endif
       CALL CAL_FULLDATE  ( obcsEstartdate1, obcsEstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsEstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsEstartdate       ,myThid)
        obcsEstartdate   =  modelstart + obcsEstartdate
       ENDIF
      ENDIF
      IF ( obcsWperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE obcsW',myThid)
# endif
       CALL CAL_FULLDATE  ( obcsWstartdate1, obcsWstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsWstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   obcsWstartdate       ,myThid)
        obcsWstartdate   =  modelstart + obcsWstartdate
       ENDIF
      ENDIF
# ifdef ALLOW_SEAICE
      IF ( siobNperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE siobN',myThid)
# endif
       CALL CAL_FULLDATE  ( siobNstartdate1, siobNstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobNstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobNstartdate       ,myThid)
        siobNstartdate   =  modelstart + siobNstartdate
       ENDIF
      ENDIF
      IF ( siobSperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE siobS',myThid)
# endif
       CALL CAL_FULLDATE  ( siobSstartdate1, siobSstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobSstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobSstartdate       ,myThid)
        siobSstartdate   =  modelstart + siobSstartdate
       ENDIF
      ENDIF
      IF ( siobEperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE siobE',myThid)
# endif
       CALL CAL_FULLDATE  ( siobEstartdate1, siobEstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobEstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobEstartdate       ,myThid)
        siobEstartdate   =  modelstart + siobEstartdate
       ENDIF
      ENDIF
      IF ( siobWperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE siobW',myThid)
# endif
       CALL CAL_FULLDATE  ( siobWstartdate1, siobWstartdate2,
     &      date_array, myThid )
       IF ( useOBCSYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobWstartdate       ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime,   siobWstartdate       ,myThid)
        siobWstartdate   =  modelstart + siobWstartdate
       ENDIF
      ENDIF
# endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_CLIMSST_RELAXATION
      IF ( climsstfile .NE. ' ' .AND. climsstperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE climsst',myThid)
# endif
       CALL CAL_FULLDATE( climsststartdate1, climsststartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climsststartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climsststartdate     ,myThid)
        climsststartdate =  modelstart + climsststartdate
       ENDIF
      ENDIF
#endif /* ALLOW_CLIMSST_RELAXATION */

#ifdef ALLOW_CLIMSSS_RELAXATION
      IF ( climsssfile .NE. ' ' .AND. climsssperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE climsss',myThid)
# endif
       CALL CAL_FULLDATE( climsssstartdate1, climsssstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climsssstartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climsssstartdate     ,myThid)
        climsssstartdate =  modelstart + climsssstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_CLIMSSS_RELAXATION */

#ifdef ALLOW_CLIMSTRESS_RELAXATION
      IF ( climustrfile .NE. ' ' .AND. climustrperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE climustr',myThid)
# endif
       CALL CAL_FULLDATE( climustrstartdate1, climustrstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climustrstartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climustrstartdate     ,myThid)
        climustrstartdate =  modelstart + climustrstartdate
       ENDIF
      ENDIF

      IF ( climvstrfile .NE. ' ' .AND. climvstrperiod .GT. 0. ) THEN
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CAL_FULLDATE climvstr',myThid)
# endif
       CALL CAL_FULLDATE( climvstrstartdate1, climvstrstartdate2,
     &      date_array, myThid )
       IF ( useExfYearlyFields ) THEN
        yearStartDate(1) = int(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climvstrstartdate     ,myThid)
       ELSE
        CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
        CALL cal_ToSeconds (difftime,    climvstrstartdate     ,myThid)
        climvstrstartdate =  modelstart + climvstrstartdate
       ENDIF
      ENDIF
#endif /* ALLOW_CLIMSTRESS_RELAXATION */

#ifdef USE_EXF_INTERPOLATION
C--   For vector fields, set flag to interpolate the 2 components together
C-    wind-stress:
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
C-    wind:
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
