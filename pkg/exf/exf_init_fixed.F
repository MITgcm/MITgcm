C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_init_fixed.F,v 1.2 2007/05/14 19:36:37 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER date_array(4), difftime(4)

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

      _BEGIN_MASTER( myThid )

#ifdef ALLOW_ATM_WIND
      IF ( uwindfile .NE. ' ' .AND. uwindperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( uwindstartdate1    ,      uwindstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    uwindstartdate     ,myThid )
         uwindstartdate     = modelstart   + uwindstartdate
      ENDIF
      IF ( vwindfile .NE. ' ' .AND. vwindperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( vwindstartdate1    ,      vwindstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    vwindstartdate     ,myThid )
         vwindstartdate     = modelstart   + vwindstartdate
      ENDIF
#endif /* ALLOW_ATM_WIND */
      IF ( wspeedfile .NE. ' ' .AND. wspeedperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( wspeedstartdate1, wspeedstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    wspeedstartdate     ,myThid )
         wspeedstartdate     = modelstart   + wspeedstartdate
      ENDIF
      IF ( ustressfile .NE. ' ' .AND. ustressperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( ustressstartdate1  ,    ustressstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,  ustressstartdate     ,myThid )
         ustressstartdate   = modelstart + ustressstartdate
      ENDIF
      IF ( vstressfile .NE. ' ' .AND. vstressperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( vstressstartdate1  ,    vstressstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,  vstressstartdate     ,myThid )
         vstressstartdate   = modelstart + vstressstartdate
      ENDIF

#ifdef ALLOW_ATM_TEMP
      IF ( atempfile .NE. ' ' .AND. atempperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( atempstartdate1    ,      atempstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    atempstartdate     ,myThid )
         atempstartdate     = modelstart   + atempstartdate
      ENDIF
      IF ( aqhfile .NE. ' ' .AND. aqhperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( aqhstartdate1      ,        aqhstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,      aqhstartdate     ,myThid )
         aqhstartdate       = modelstart     + aqhstartdate
      ENDIF
      IF ( lwfluxfile .NE. ' ' .AND. lwfluxperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( lwfluxstartdate1   ,     lwfluxstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   lwfluxstartdate     ,myThid )
         lwfluxstartdate    = modelstart  + lwfluxstartdate
      ENDIF
      IF ( precipfile .NE. ' ' .AND. precipperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( precipstartdate1   ,     precipstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   precipstartdate     ,myThid )
         precipstartdate    = modelstart  + precipstartdate
      ENDIF
      IF ( snowprecipfile .NE. ' ' .AND. snowprecipperiod .GT. 0. ) THEN
         CALL cal_FullDate( snowprecipstartdate1, snowprecipstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime, snowprecipstartdate, myThid )
         snowprecipstartdate    = modelstart  + snowprecipstartdate
      ENDIF
#else
      IF ( hfluxfile .NE. ' ' .AND. hfluxperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( hfluxstartdate1    ,      hfluxstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    hfluxstartdate     ,myThid )
         hfluxstartdate     = modelstart   + hfluxstartdate
      ENDIF
      IF ( sfluxfile .NE. ' ' .AND. sfluxperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( sfluxstartdate1    ,      sfluxstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,    sfluxstartdate     ,myThid )
         sfluxstartdate     = modelstart   + sfluxstartdate
      ENDIF
#endif

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .NE. ' ' .AND. swfluxperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( swfluxstartdate1   ,     swfluxstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   swfluxstartdate     ,myThid )
         swfluxstartdate    = modelstart  + swfluxstartdate
      ENDIF
#endif

#ifdef EXF_READ_EVAP
      IF ( evapfile .NE. ' ' .AND. evapperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( evapstartdate1     ,       evapstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,     evapstartdate     ,myThid )
         evapstartdate      = modelstart    + evapstartdate
      ENDIF
#endif

#ifdef ALLOW_RUNOFF
      IF ( runofffile .NE. ' ' .AND. runoffperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( runoffstartdate1   ,     runoffstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   runoffstartdate     ,myThid )
         runoffstartdate    = modelstart  + runoffstartdate
      ENDIF
#endif

#ifdef ALLOW_DOWNWARD_RADIATION
      IF ( swdownfile .NE. ' ' .AND. swdownperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( swdownstartdate1   ,     swdownstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   swdownstartdate     ,myThid )
         swdownstartdate    = modelstart  + swdownstartdate
      ENDIF
      IF ( lwdownfile .NE. ' ' .AND. lwdownperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( lwdownstartdate1   ,     lwdownstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   lwdownstartdate     ,myThid )
         lwdownstartdate    = modelstart  + lwdownstartdate
      ENDIF
#endif

#ifdef ATMOSPHERIC_LOADING
      IF ( apressurefile .NE. ' ' .AND. apressureperiod .GT. 0. ) THEN
         CALL cal_FullDate  ( apressurestartdate1,  apressurestartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,  apressurestartdate   ,myThid )
         apressurestartdate = modelstart + apressurestartdate
      ENDIF
#endif

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_NORTH
      IF ( obcsNperiod .GT. 0 ) THEN
         CALL cal_FullDate  ( obcsNstartdate1    ,      obcsNstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   obcsNstartdate      ,myThid )
         obcsNstartdate     = modelstart + obcsNstartdate
      ENDIF
#endif
#ifdef ALLOW_OBCS_SOUTH
      IF ( obcsSperiod .GT. 0 ) THEN
         CALL cal_FullDate  ( obcsSstartdate1    ,      obcsSstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   obcsSstartdate      ,myThid )
         obcsSstartdate     = modelstart + obcsSstartdate
      ENDIF
#endif
#ifdef ALLOW_OBCS_EAST
      IF ( obcsEperiod .GT. 0 ) THEN
         CALL cal_FullDate  ( obcsEstartdate1    ,      obcsEstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,   obcsEstartdate      ,myThid )
         obcsEstartdate     = modelstart + obcsEstartdate
      ENDIF
#endif
#ifdef ALLOW_OBCS_WEST
      IF ( obcsWperiod .GT. 0 ) THEN
         CALL cal_FullDate  ( obcsWstartdate1    ,      obcsWstartdate2,
     &        date_array                        ,myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds ( difftime,  obcsWstartdate       ,myThid )
         obcsWstartdate     = modelstart + obcsWstartdate
      ENDIF
#endif
#endif /* ALLOW_OBCS */

#ifdef ALLOW_CLIMSST_RELAXATION
      IF ( climsstfile .NE. ' ' .AND. climsstperiod .NE. 0. ) THEN
         CALL cal_FullDate( climsststartdate1, climsststartdate2,
     &        date_array,                  myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds (difftime,       climsststartdate  ,myThid)
         climsststartdate=modelstart+climsststartdate
      ENDIF
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      IF ( climsssfile .NE. ' ' .AND. climsssperiod .NE. 0. ) THEN
         CALL cal_FullDate( climsssstartdate1, climsssstartdate2,
     &        date_array,                  myThid )
         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
         CALL cal_ToSeconds (difftime,       climsssstartdate  ,myThid)
         climsssstartdate=modelstart+climsssstartdate
      ENDIF
#endif

      _END_MASTER( myThid )
      _BARRIER

C--   Summarize the External forcing's setup.
      CALL EXF_SUMMARY( myThid )

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL EXF_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
