#include "EXF_OPTIONS.h"
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXF_MONITOR

C     !INTERFACE:
      SUBROUTINE EXF_MONITOR(
     I               myTime, myIter, myThid )

C     !DESCRIPTION:
C     Print some statistics about input forcing fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_BLING
# include "EXF_INTERP_SIZE.h"
# include "BLING_VARS.h"
#endif

C     !INPUT PARAMETERS:
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_MONITOR

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     == Local variables ==
      _RL dummyRL(6)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      INTEGER k
#endif
      LOGICAL computed

#ifdef ALLOW_BULKFORMULAE
      computed = .TRUE.
#else
      computed = .FALSE.
#endif

      IF ( DIFFERENT_MULTIPLE(exf_monFreq,myTime,deltaTClock)
     &   ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (monitor_stdio) THEN
            mon_write_stdout = .TRUE.
          ELSE
            mon_write_stdout = .FALSE.
          ENDIF
          mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
          IF (useMNC .AND. monitor_mnc) THEN
            DO k = 1,MAX_LEN_MBUF
              mon_fname(k:k) = ' '
            ENDDO
            mon_fname(1:11) = 'monitor_exf'
            CALL MNC_CW_APPEND_VNAME(
     &           'T', '-_-_--__-__t', 0,0, myThid)
            CALL MNC_CW_SET_UDIM(mon_fname, -1, myThid)
            CALL MNC_CW_RL_W_S(
     &          'D',mon_fname,1,1,'T', myTime, myThid)
            CALL MNC_CW_SET_UDIM(mon_fname, 0, myThid)
            mon_write_mnc = .TRUE.
          ENDIF
#endif /* ALLOW_MNC */

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// Begin MONITOR EXF statistics'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

C--   endif master cpu io
        ENDIF

        CALL MON_SET_PREF('exf',myThid)
        CALL MON_OUT_I ('_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_time_sec', myTime,mon_string_none,myThid)

C       Print some statistics about input forcing fields
        IF ( stressIsOnCgrid ) THEN
          CALL MON_WRITESTATS_RL( 1, ustress, '_ustress',
     &             maskInW, maskInW, rAw, drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RL( 1, vstress, '_vstress',
     &             maskInS, maskInS, rAs, drF, dummyRL, myThid )
        ELSE
          CALL MON_WRITESTATS_RL( 1, ustress, '_ustress',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RL( 1, vstress, '_vstress',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( computed .OR. hfluxfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, hflux,   '_hflux',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( computed .OR. sfluxfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, sflux,   '_sflux',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( useAtmWind ) THEN
          CALL MON_WRITESTATS_RL( 1, uwind,   '_uwind',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RL( 1, vwind,   '_vwind',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( computed .OR. wspeedfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, wspeed,  '_wspeed',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#ifdef ALLOW_ATM_TEMP
        IF ( atempfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, atemp,   '_atemp',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( aqhfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, aqh,     '_aqh',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( lwdownfile .NE. ' ' .OR. lwfluxfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, lwflux,  '_lwflux',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( computed .OR. evapfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, evap,    '_evap',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( precipfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, precip,  '_precip',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( snowprecipfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL(1,snowprecip,'_snowprecip',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif /* ALLOW_ATM_TEMP */
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        IF ( swdownfile .NE. ' ' .OR. swfluxfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, swflux,  '_swflux',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        IF ( swdownfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, swdown,  '_swdown',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        IF ( lwdownfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, lwdown,  '_lwdown',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ATMOSPHERIC_LOADING
        IF ( apressurefile .NE. ' ' ) THEN
           CALL MON_WRITESTATS_RL( 1,apressure,'_apressure',
     &              maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_RUNOFF
        IF ( runofffile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, runoff,  '_runoff',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
# ifdef ALLOW_RUNOFTEMP
        IF ( runoftempfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, runoftemp,  '_runoftemp',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
# endif
#endif
#ifdef ALLOW_SALTFLX
        IF ( saltflxfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, saltflx,'_saltflx',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef EXF_SEAICE_FRACTION
        IF ( areamaskfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, areamask,'_areamask',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef USE_EXFCO2
        IF ( apco2file .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, apco2, '_apco2',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
        IF ( climsstfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, climsst, '_climsst',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
        IF ( climsssfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, climsss, '_climsss',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_CLIMSTRESS_RELAXATION
        IF ( climustrfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, climustr, '_climustr',
     &             maskInW, maskInW, rAw, drF, dummyRL, myThid )
        ENDIF
        IF ( climvstrfile .NE. ' ' ) THEN
          CALL MON_WRITESTATS_RL( 1, climvstr, '_climvstr',
     &             maskInS, maskInS, rAs, drF, dummyRL, myThid )
        ENDIF
#endif

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// End MONITOR EXF statistics'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif master cpu io
        ENDIF

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */

      RETURN
      END
