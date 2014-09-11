C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_monitor_ad.F,v 1.7 2014/09/11 19:18:52 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: ADPTRACERS_MONITOR

C !INTERFACE: ==========================================================
      SUBROUTINE ADPTRACERS_MONITOR(
     I                    myTime, myIter, myThid )

C !DESCRIPTION:
C writes out ptracer statistics

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#ifdef ALLOW_MONITOR
# include "MONITOR.h"
#endif
#ifdef ALLOW_AUTODIFF_MONITOR
# include "ptracers_adcommon.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_MONITOR

#ifdef ALLOW_AUTODIFF_MONITOR

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  ip                   :: ptracer number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER ip
      _RL dummyRL(6)
CEOP

      IF ( DIFFERENT_MULTIPLE( adjmonitorFreq,myTime,deltaTClock )
     &   ) THEN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Ptracers field monitor start
        IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

          IF (monitor_stdio) THEN
            mon_write_stdout = .TRUE.
          ELSE
            mon_write_stdout = .FALSE.
          ENDIF
          mon_write_mnc    = .FALSE.
#ifdef ALLOW_MNC
          IF (useMNC .AND. PTRACERS_monitor_mnc) THEN
            WRITE(mon_fname,'(A)') 'monitor_ptracer'
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
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(A)')
     &             '// Begin MONITOR adptracer field statistics'
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
            WRITE(msgBuf,'(2A)') '// ==========================',
     &             '============================='
            CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
          ENDIF

C--   endif master cpu io
       ENDIF

C      Print the basic statistics of ptracer variables
       CALL MON_SET_PREF('ad_trcstat_',myThid)
       DO ip = 1, PTRACERS_numInUse
         WRITE(suff,'(A9,A2)') 'adptracer',PTRACERS_ioLabel(ip)
         CALL MON_WRITESTATS_RL(
     &            Nr, adptracer(1-OLx,1-OLy,1,1,1,ip), suff,
     &            hFacC, maskInC, rA ,drF, dummyRL, myThid )
       ENDDO

C--   Ptracers field monitor finish
       IF ( MASTER_CPU_IO(myThid) ) THEN
C-    only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

        IF ( mon_write_stdout ) THEN
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A)')
     &        '// End MONITOR ptracers field statistics'
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(2A)') '// ==========================',
     &        '============================='
         CALL PRINT_MESSAGE( msgBuf, mon_ioUnit, SQUEEZE_RIGHT , 1)
        ENDIF

        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.

C-    endif master cpu io
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  endif different multiple
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */

#endif /* ALLOW_MONITOR */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
