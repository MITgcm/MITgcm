C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_monitor.F,v 1.15 2007/06/25 15:41:57 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

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

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_MONITOR

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      INTEGER k
#endif

      IF ( DIFFERENT_MULTIPLE(exf_monFreq,myTime,deltaTClock)
     &   ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it's the only thread that uses those flags):

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
            CALL MNC_CW_I_W_S(
     &          'I',mon_fname,1,1,'T', myIter, myThid)
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
        CALL MON_PRINTSTATS_RL(1,ustress,'_ustress',
     &       maskH,maskH,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,vstress,'_vstress',
     &       maskH,maskH,rAs,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,hflux,'_hflux',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,sflux,'_sflux',
     &       maskH,maskH,rA ,drF,myThid)
#ifdef ALLOW_ATM_WIND
        CALL MON_PRINTSTATS_RL(1,uwind,'_uwind',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,vwind,'_vwind',
     &       maskH,maskH,rA ,drF,myThid)
#endif /* ALLOW_ATM_WIND */
        CALL MON_PRINTSTATS_RL(1,wspeed,'_wspeed',
     &       maskH,maskH,rA ,drF,myThid)
#ifdef ALLOW_ATM_TEMP
        CALL MON_PRINTSTATS_RL(1,atemp,'_atemp',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,aqh,'_aqh',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,lwflux,'_lwflux',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,precip,'_precip',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,snowprecip,'_snowprecip',
     &       maskH,maskH,rA ,drF,myThid)
#endif /* ALLOW_ATM_TEMP */
#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        CALL MON_PRINTSTATS_RL(1,swflux,'_swflux',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
        CALL MON_PRINTSTATS_RL(1,evap,'_evap',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
        CALL MON_PRINTSTATS_RL(1,swdown,'_swdown',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,lwdown,'_lwdown',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ATMOSPHERIC_LOADING
        CALL MON_PRINTSTATS_RL(1,apressure,'_apressure',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_RUNOFF
        CALL MON_PRINTSTATS_RL(1,runoff,'_runoff',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_CLIMSST_RELAXATION
        CALL MON_PRINTSTATS_RL(1,climsst,'_climsst',
     &       maskH,maskH,rA ,drF,myThid)
#endif
#ifdef ALLOW_CLIMSSS_RELAXATION
        CALL MON_PRINTSTATS_RL(1,climsss,'_climsss',
     &       maskH,maskH,rA ,drF,myThid)
#endif

#ifdef ALLOW_BULK_OFFLINE
c-- Trick monitor output for testreport
C-- by writing some statistics into dynstat fields
c-- which are tested by testreport
        CALL MON_SET_PREF(' ',myThid)
        CALL MON_PRINTSTATS_RL(1,hflux,'cg2d_init_res',
     &       maskH,maskH,rA ,drF,myThid)
c--
        CALL MON_SET_PREF('dynstat',myThid)
c--
        CALL MON_PRINTSTATS_RL(1,hflux,'_theta',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,sflux,'_salt',
     &       maskH,maskH,rA ,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,ustress,'_uvel',
     &       maskH,maskH,rAw,drF,myThid)
        CALL MON_PRINTSTATS_RL(1,vstress,'_vvel',
     &       maskH,maskH,rAs,drF,myThid)
c--
        CALL MON_SET_PREF('exf',myThid)
#endif /* ALLOW_BULK_OFFLINE */

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it's the only thread that uses those flags):

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
