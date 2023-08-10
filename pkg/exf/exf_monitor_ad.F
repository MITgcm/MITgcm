#include "EXF_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADEXF_MONITOR

C     !INTERFACE:
      SUBROUTINE ADEXF_MONITOR(
     I     iwhen, myTime, myIter, myThid )

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
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
#  include "adcommon.h"
# endif
#endif

C     !INPUT PARAMETERS:
      INTEGER iwhen
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#if ( defined (ALLOW_MONITOR) && defined (ALLOW_AUTODIFF_MONITOR) )

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
      LOGICAL printMonitorHeader

      IF ( exf_adjMonSelect .GT. 0 .AND.
     &     DIFFERENT_MULTIPLE(exf_adjMonFreq,myTime,deltaTClock) ) THEN

       IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

        printMonitorHeader = ( iwhen .EQ. 3 )
     &       .OR. ( iwhen .EQ. 1 .AND. exf_adjMonSelect .GE. 3 )
     &       .OR. ( iwhen .EQ. 2 .AND. exf_adjMonSelect .GE. 2 )

        IF ( printMonitorHeader ) THEN

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
            mon_fname(1:13) = 'admonitor_exf'
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
            WRITE(msgBuf,'(A,I2)')
     &           '// Begin AD_MONITOR EXF statistics for iwhen = ',
     &           iwhen
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

C--   endif printMonitorHeader
        ENDIF
C--   endif master cpu io
       ENDIF

        CALL MON_SET_PREF('ad_exf',myThid)
        CALL MON_OUT_I ('_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_time_sec', myTime,mon_string_none,myThid)

       IF ( iwhen .EQ. 1 .AND. exf_adjMonSelect .GE. 3 ) THEN

C       Print some statistics about input forcing fields
        IF ( stressIsOnCgrid ) THEN
          CALL MON_WRITESTATS_RL( 1, adustress, '_adustress',
     &             maskInW, maskInW, rAw, drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RL( 1, advstress, '_advstress',
     &             maskInS, maskInS, rAs, drF, dummyRL, myThid )
        ELSE
          CALL MON_WRITESTATS_RL( 1, adustress, '_adustress',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RL( 1, advstress, '_advstress',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
        CALL MON_WRITESTATS_RL( 1, adhflux,   '_adhflux',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, adsflux,   '_adsflux',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#ifdef SHORTWAVE_HEATING
        CALL MON_WRITESTATS_RL( 1, adswflux,  '_adswflux',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
        CALL MON_WRITESTATS_RL( 1, adwspeed,  '_adwspeed',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )

       ELSEIF ( iwhen .EQ. 2 .AND. exf_adjMonSelect .GE. 2 ) THEN

        IF ( useAtmWind ) THEN
         CALL MON_WRITESTATS_RL( 1, aduwind,  '_aduwind',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
         CALL MON_WRITESTATS_RL( 1, advwind,  '_advwind',
     &            maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#ifdef ALLOW_ATM_TEMP
        CALL MON_WRITESTATS_RL( 1, adatemp,   '_adatemp',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, adaqh,     '_adaqh',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, adprecip,  '_adprecip',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        IF ( lwfluxfile.NE.' ' .OR. lwdownfile.EQ.' ' ) THEN
         CALL MON_WRITESTATS_RL( 1, adlwflux, '_adlwflux',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif /* ALLOW_ATM_TEMP */
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        IF ( swfluxfile.NE.' ' .OR. swdownfile.EQ.' ' ) THEN
         CALL MON_WRITESTATS_RL( 1, adswflux, '_adswflux',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        ENDIF
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        CALL MON_WRITESTATS_RL( 1, adlwdown,  '_adlwdown',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, adswdown,  '_adswdown',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef ALLOW_RUNOFF
        CALL MON_WRITESTATS_RL( 1, adrunoff,  '_adrunoff',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
cph        CALL MON_WRITESTATS_RL( 1,adapressure,'_adapressure',
cph     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef EXF_SEAICE_FRACTION
cph        CALL MON_WRITESTATS_RL( 1, areamask,'_areamask',
cph     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
cph        CALL MON_WRITESTATS_RL( 1, climsst, '_climsst',
cph     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
cph        CALL MON_WRITESTATS_RL( 1, climsss, '_climsss',
cph     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif
#ifdef ALLOW_CLIMSTRESS_RELAXATION
cph          CALL MON_WRITESTATS_RL( 1, climustr, '_climustr',
cph     &             maskInW, maskInW, rAw, drF, dummyRL, myThid )
cph          CALL MON_WRITESTATS_RL( 1, climvstr, '_climvstr',
cph     &             maskInS, maskInS, rAs, drF, dummyRL, myThid )
#endif

       ELSEIF ( iwhen .EQ. 3 ) THEN

          CALL MON_WRITESTATS_RS( 1, adfu, '_adfu',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RS( 1, adfv, '_adfv',
     &             maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RS( 1, adqnet, '_adqnet',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
          CALL MON_WRITESTATS_RS( 1, adempmr, '_adempmr',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#ifdef SHORTWAVE_HEATING
          CALL MON_WRITESTATS_RS( 1, adqsw, '_adqsw',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif

       ENDIF

       IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

        IF ( printMonitorHeader ) THEN

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A,I2)')
     &           '// End AD_MONITOR EXF statistics for iwhen = ',
     &           iwhen
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

          mon_write_stdout = .FALSE.
          mon_write_mnc    = .FALSE.

C--   endif printMonitorHeader
        ENDIF
C--   endif master cpu io
       ENDIF

C     endif different multiple
      ENDIF

#endif /* ALLOW_MONITOR */

      RETURN
      END
