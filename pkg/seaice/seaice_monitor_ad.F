C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_monitor_ad.F,v 1.6 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ADSEAICE_MONITOR

C     !INTERFACE:
      SUBROUTINE ADSEAICE_MONITOR(
     I                  myTime, myIter, myThid )

C     !DESCRIPTION:
C     Print some statistics about input forcing fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_MONITOR
#  include "adcommon.h"
# endif
#endif

C     !INPUT PARAMETERS:
      INTEGER myIter
      _RL myTime
      INTEGER myThid
CEOP

#if ( defined (ALLOW_MONITOR) && defined (ALLOW_AUTODIFF_MONITOR) )

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_SITRACER
      INTEGER iTracer
      CHARACTER*(MAX_LEN_MBUF) suff
#endif
#ifdef ALLOW_MNC
      INTEGER i
#endif /* ALLOW_MNC */
      _RL dummyRL(6)

      IF ( DIFFERENT_MULTIPLE(adjmonitorFreq,myTime,deltaTClock)
     &   ) THEN

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (SEAICE_mon_stdio) THEN
            mon_write_stdout = .TRUE.
          ELSE
            mon_write_stdout = .FALSE.
          ENDIF
          mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
          IF (useMNC .AND. SEAICE_mon_mnc) THEN
            DO i = 1,MAX_LEN_MBUF
              mon_fname(i:i) = ' '
            ENDDO
            mon_fname(1:14) = 'admonitor_seaice'
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
     &             '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// Begin AD_MONITOR SEAICE statistics'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(2A)') '// ===========================',
     &             '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
          ENDIF

C--   endif master cpu io
        ENDIF

        CALL MON_SET_PREF('ad_seaice',myThid)
        CALL MON_OUT_I ('_tsnumber', myIter,mon_string_none,myThid)
        CALL MON_OUT_RL('_time_sec', myTime,mon_string_none,myThid)

C       Print some statistics about input forcing fields
#ifdef SEAICE_CGRID
        CALL MON_WRITESTATS_RL( 1, ADUICE, '_aduice',
     &           maskInW, maskInW, rAw, drF, dummyRL, myThid )
#else
        CALL MON_WRITESTATS_RL( 1, ADUICE, '_aduice',
     &                   UVM, UVM, rAz, drF, dummyRL, myThid )
#endif
#ifdef SEAICE_CGRID
        CALL MON_WRITESTATS_RL( 1, ADVICE, '_advice',
     &           maskInS, maskInS, rAs, drF, dummyRL, myThid )
#else
        CALL MON_WRITESTATS_RL( 1, ADVICE, '_advice',
     &                   UVM, UVM, rAz, drF, dummyRL, myThid )
#endif
        CALL MON_WRITESTATS_RL( 1, ADAREA, '_adarea',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, ADHEFF, '_adheff',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
        CALL MON_WRITESTATS_RL( 1, ADHSNOW,'_adhsnow',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#ifdef SEAICE_VARIABLE_SALINITY
        CALL MON_WRITESTATS_RL( 1, ADHSALT,'_adhsalt',
     &           maskInC, maskInC, rA , drF, dummyRL, myThid )
#endif /* SEAICE_VARIABLE_SALINITY */

        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF ( mon_write_stdout ) THEN
            WRITE(msgBuf,'(2A)') '// ===========================',
     &           '============================'
            CALL PRINT_MESSAGE(msgBuf, mon_ioUnit, SQUEEZE_RIGHT, 1)
            WRITE(msgBuf,'(A)') '// End AD_MONITOR SEAICE statistics'
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
